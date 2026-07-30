package org.uml2semantics.model.cache

import org.uml2semantics.diagnostics.{DiagnosticCode, UMLModelException}
import org.uml2semantics.model.UMLClassIdentity.ClassIdentityBuilder
import org.uml2semantics.model.{PrefixNamespace, UMLClass, UMLClassCurie, UMLClassDefinition, UMLClassIdentifier, UMLClassIdentity, UMLClassName}

import scala.collection.mutable

object ClassIdentityBuilderCache:
  private val buildersByClassName = mutable.Map[UMLClassName, ClassIdentityBuilder]()
  private val buildersByClassCurie = mutable.Map[UMLClassCurie, ClassIdentityBuilder]()
  private val classIdentityByClassName = mutable.Map[UMLClassName, UMLClassIdentity]()
  private val classIdentityByClassCurie = mutable.Map[UMLClassCurie, UMLClassIdentity]()

//  def cacheUMLClassIdentity(className: UMLClassName, builder: ClassIdentityBuilder): Unit =
//    if !classIdentityByClassName.contains(className) then
//      val classIdentity = UMLClassIdentity(nameOption = Some(className), ontologyPrefix = builder.prefixNamespace)
//      classIdentityByClassName += (className -> classIdentity)
//      buildersByClassName += (className -> builder)
//
//  def cacheUMLClassIdentity(classCurie: UMLClassCurie, builder: ClassIdentityBuilder): Unit =
//    if !classIdentityByClassCurie.contains(classCurie) then
//      val classIdentity = UMLClassIdentity(curieOption = Some(classCurie), ontologyPrefix = builder.prefixNamespace)
//      classIdentityByClassCurie += (classCurie -> classIdentity)
//      buildersByClassCurie += (classCurie -> builder)

  def clear(): Unit =
    buildersByClassName.clear()
    buildersByClassCurie.clear()
    classIdentityByClassName.clear()
    classIdentityByClassCurie.clear()

  def cacheUMLClassIdentity(classIdentity: UMLClassIdentity, builder: ClassIdentityBuilder): UMLClassIdentity =
    classIdentity.nameOption.foreach(name =>
      classIdentityByClassName += (name -> classIdentity)
      buildersByClassName += (name -> builder)
    )

    classIdentity.curieOption.foreach(curie =>
      classIdentityByClassCurie += (curie -> classIdentity)
      buildersByClassCurie += (curie -> builder)
    )

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
  
  def getClassIdentityBuilder(classIdentity: UMLClassIdentity): Option[ClassIdentityBuilder] =
    (classIdentity.curieOption, classIdentity.nameOption) match
      case (Some(curie), Some(name)) => buildersByClassCurie.get(curie)
        .orElse(buildersByClassName.get(name))
      case (Some(curie), None) => buildersByClassCurie.get(curie)
      case (None, Some(name)) => buildersByClassName.get(name)
      case (None, None) => throw new IllegalArgumentException("ClassIdentity must have a name or curie")
      
  def getClassIdentity(classIdentity: UMLClassIdentity):  Option[UMLClassIdentity] =
    (classIdentity.curieOption, classIdentity.nameOption) match
      case (Some(curie), Some(name)) => classIdentityByClassCurie.get(curie)
        .orElse(classIdentityByClassName.get(name))
      case (Some(curie), None) => classIdentityByClassCurie.get(curie)
      case (None, Some(name)) => classIdentityByClassName.get(name)
      case (None, None) => throw new IllegalArgumentException("ClassIdentity must have a name or curie") 

object ClassBuilderCache:
  private val buildersByClassIdentity = mutable.Map[UMLClassIdentity, UMLClass.ClassBuilder]()
  private val classesByClassIdentity = mutable.Map[UMLClassIdentity, UMLClass]()

  def clear(): Unit =
    buildersByClassIdentity.clear()
    classesByClassIdentity.clear()

  def cacheUMLClass(umlClass: UMLClass, builder: UMLClass.ClassBuilder): Unit =
    // When enriching a partial (name-only or curie-only) entry with the full identity,
    // remove the partial versions to avoid duplicates
    var classToCache = umlClass
    if umlClass.classIdentity.nameOption.nonEmpty && umlClass.classIdentity.curieOption.nonEmpty then
      val partialIdentities = Seq(
        UMLClassIdentity(umlClass.classIdentity.nameOption, None, umlClass.classIdentity.ontologyPrefix),
        UMLClassIdentity(None, umlClass.classIdentity.curieOption, umlClass.classIdentity.ontologyPrefix))
      partialIdentities.foreach { partialIdentity =>
        // Merge data from the old class into the new one so we don't lose definition/children
        classesByClassIdentity.get(partialIdentity).foreach { oldClass =>
          val mergedDefinition = classToCache.classDefinitionOption match
            case Some(UMLClassDefinition(d)) if d.nonEmpty => classToCache.classDefinitionOption
            case _ => oldClass.classDefinitionOption
          val mergedChildren = if classToCache.children.setOfGeneralizationSets.nonEmpty then classToCache.children
            else oldClass.children
          classToCache = classToCache.copy(classDefinitionOption = mergedDefinition, children = mergedChildren)
        }
        // Update attributes that still reference the old partial class identity
        AttributeBuilderCache.updateClassIdentity(partialIdentity, umlClass.classIdentity)
        removeUMLClass(partialIdentity)
      }
    buildersByClassIdentity += (classToCache.classIdentity -> builder)
    classesByClassIdentity += (classToCache.classIdentity -> classToCache)
  
  def getUMLClass(classIdentity: UMLClassIdentity): Option[UMLClass] =
    classesByClassIdentity.get(classIdentity)
    
  def getUMLClass(name: String): Option[UMLClass] =
    ClassIdentityBuilderCache.getUMLClassIdentity(name)
      .flatMap(getUMLClass)

  def getUMLClassBuilder(classIdentity: UMLClassIdentity): Option[UMLClass.ClassBuilder] =
    buildersByClassIdentity.get(classIdentity)

  def getUMLClassBuilder(name: String): Option[UMLClass.ClassBuilder] =
    ClassIdentityBuilderCache.getUMLClassIdentity(name)
      .flatMap(getUMLClassBuilder)
    
  def removeUMLClass(classIdentity: UMLClassIdentity): Unit =
    buildersByClassIdentity -= classIdentity
    classesByClassIdentity -= classIdentity

  def getClasses: Set[UMLClass] = classesByClassIdentity.values.toSet

