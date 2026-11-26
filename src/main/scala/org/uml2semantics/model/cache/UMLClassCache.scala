package org.uml2semantics.model.cache

import org.uml2semantics.diagnostics.{DiagnosticCode, UMLModelException}
import org.uml2semantics.model.{PrefixNamespace, UMLClass, UMLClassChildren, UMLClassCurie, UMLClassDefinition, UMLClassIdentifier, UMLClassIdentity, UMLClassName}

import scala.collection.mutable

object ClassIdentityCache:
  private val classIdentityByClassName = mutable.Map[UMLClassName, UMLClassIdentity]()
  private val classIdentityByClassCurie = mutable.Map[UMLClassCurie, UMLClassIdentity]()

  def clear(): Unit =
    classIdentityByClassName.clear()
    classIdentityByClassCurie.clear()

  /**
   * Caches an identity under each half it has. An identity with both halves takes over the entries
   * of either half alone, which is how a class named in one place and given a curie in another
   * ends up as one identity: building an identity from one half returns what is cached for it.
   */
  def cacheUMLClassIdentity(classIdentity: UMLClassIdentity): UMLClassIdentity =
    classIdentity.nameOption.foreach(name => classIdentityByClassName += (name -> classIdentity))
    classIdentity.curieOption.foreach(curie => classIdentityByClassCurie += (curie -> classIdentity))
    classIdentity

  def getUMLClassIdentity(name: String): Option[UMLClassIdentity] =
    (UMLClassCurie(name), UMLClassName(name)) match
      case (Some(curie), _) => classIdentityByClassCurie.get(curie)
      case (_, Some(className)) => classIdentityByClassName.get(className)
      case _ if name.isBlank =>
        throw new UMLModelException(DiagnosticCode.BlankIdentity,
          "A class needs a name or a curie; a blank identifier was given.")
      case _ => throw new UMLModelException(DiagnosticCode.UnresolvableCurie,
        s"Could not interpret '$name' as a class name or curie. If it is intended to be a curie, its prefix " +
          s"must be configured via the -p or -x command line options.")

  def getUMLClassIdentity(identifier: UMLClassIdentifier): Option[UMLClassIdentity] =
    identifier match
      case name: UMLClassName => classIdentityByClassName.get(name)
      case curie: UMLClassCurie => classIdentityByClassCurie.get(curie)
  
  def getClassIdentity(classIdentity: UMLClassIdentity):  Option[UMLClassIdentity] =
    (classIdentity.curieOption, classIdentity.nameOption) match
      case (Some(curie), Some(name)) => classIdentityByClassCurie.get(curie)
        .orElse(classIdentityByClassName.get(name))
      case (Some(curie), None) => classIdentityByClassCurie.get(curie)
      case (None, Some(name)) => classIdentityByClassName.get(name)
      case (None, None) => throw new UMLModelException(DiagnosticCode.BlankIdentity,
        "A class identity must have a name or a curie.") 

object ClassCache:
  private val classesByClassIdentity = mutable.Map[UMLClassIdentity, UMLClass]()

  def clear(): Unit =
    classesByClassIdentity.clear()

  /**
   * Caches a class, merging into it what an earlier build of the same class left behind.
   *
   * The class may already be cached under this identity, or, when this identity has both a name
   * and a curie, under either half alone. A definition this build states wins, and a blank one is
   * taken from the earlier build. Children are added to: the class keeps the generalization sets of
   * the earlier build beside its own, so that no source can take away a child another source gave,
   * in whichever order the sources are read.
   *
   * This merge is also how a parent gets its children: once the classes are read, a reader builds
   * each parent again with only its identity and its children, and the two builds are combined here.
   */
  def cacheUMLClass(umlClass: UMLClass): Unit =
    val classIdentity = umlClass.classIdentity
    val partialIdentities =
      if classIdentity.nameOption.nonEmpty && classIdentity.curieOption.nonEmpty then Seq(
        UMLClassIdentity(classIdentity.nameOption, None, classIdentity.ontologyPrefix),
        UMLClassIdentity(None, classIdentity.curieOption, classIdentity.ontologyPrefix))
      else Seq.empty

    var classToCache = umlClass
    (classIdentity +: partialIdentities).flatMap(classesByClassIdentity.get).foreach { earlierClass =>
      val mergedDefinition = classToCache.classDefinitionOption match
        case Some(UMLClassDefinition(definition)) if definition.nonEmpty => classToCache.classDefinitionOption
        case _ => earlierClass.classDefinitionOption
      val mergedChildren = UMLClassChildren(
        earlierClass.children.setOfGeneralizationSets ++ classToCache.children.setOfGeneralizationSets)
      classToCache = classToCache.copy(classDefinitionOption = mergedDefinition, children = mergedChildren)
    }

    // The partial entries are now part of this one. Attributes that still point at them follow.
    partialIdentities.foreach { partialIdentity =>
      AttributeCache.updateClassIdentity(partialIdentity, classIdentity)
      removeUMLClass(partialIdentity)
    }
    classesByClassIdentity += (classToCache.classIdentity -> classToCache)
  
  /**
   * A class with its children under the identities they have now.
   *
   * A generalization set records the identity each child had when the set was built. A child may
   * gain the other half of its identity afterwards, as when a TSV row gives a class of the diagram
   * a curie, and that changes its IRI. The set is not rewritten when that happens. Instead every
   * class handed out by this cache has its children looked up again, so that a generalization
   * follows its child to the IRI the child ends up with.
   */
  private def withCurrentChildren(umlClass: UMLClass): UMLClass =
    umlClass.copy(children = UMLClassChildren(umlClass.children.setOfGeneralizationSets.map(generalizationSet =>
      generalizationSet.copy(generalizationSet = generalizationSet.generalizationSet.map(child =>
        ClassIdentityCache.getClassIdentity(child).getOrElse(child))))))

  def getUMLClass(classIdentity: UMLClassIdentity): Option[UMLClass] =
    classesByClassIdentity.get(classIdentity).map(withCurrentChildren)
    
  def getUMLClass(name: String): Option[UMLClass] =
    ClassIdentityCache.getUMLClassIdentity(name)
      .flatMap(getUMLClass)

  def removeUMLClass(classIdentity: UMLClassIdentity): Unit =
    classesByClassIdentity -= classIdentity

  def getClasses: Set[UMLClass] = classesByClassIdentity.values.map(withCurrentChildren).toSet

