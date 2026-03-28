package org.uml2semantics.model.cache

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
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")

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
    // When enriching with curie, remove any existing name-only version to avoid duplicates
    var classToCache = umlClass
    if umlClass.classIdentity.nameOption.nonEmpty && umlClass.classIdentity.curieOption.nonEmpty then
      val nameOnlyIdentity = UMLClassIdentity(umlClass.classIdentity.nameOption, None, umlClass.classIdentity.ontologyPrefix)
      // Merge data from the old class into the new one so we don't lose definition/children
      classesByClassIdentity.get(nameOnlyIdentity).foreach { oldClass =>
        val mergedDefinition = umlClass.classDefinitionOption match
          case Some(UMLClassDefinition(d)) if d.nonEmpty => umlClass.classDefinitionOption
          case _ => oldClass.classDefinitionOption
        val mergedChildren = if umlClass.children.setOfGeneralizationSets.nonEmpty then umlClass.children
          else oldClass.children
        classToCache = umlClass.copy(classDefinitionOption = mergedDefinition, children = mergedChildren)
      }
      // Update attributes that still reference the old name-only class identity
      AttributeBuilderCache.updateClassIdentity(nameOnlyIdentity, umlClass.classIdentity)
      removeUMLClass(nameOnlyIdentity)
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

