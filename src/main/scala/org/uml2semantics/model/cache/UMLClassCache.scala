package org.uml2semantics.model.cache

import org.uml2semantics.model.UMLClassIdentity.ClassIdentityBuilder
import org.uml2semantics.model.{PrefixNamespace, UMLClass, UMLClassCurie, UMLClassIdentifier, UMLClassIdentity, UMLClassName}

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

object ClassBuilderCache:
  private val buildersByClassIdentity = mutable.Map[UMLClassIdentity, UMLClass.ClassBuilder]()
  private val classesByClassIdentity = mutable.Map[UMLClassIdentity, UMLClass]()

  def cacheUMLClass(umlClass: UMLClass, builder: UMLClass.ClassBuilder): Unit =
    buildersByClassIdentity += (umlClass.classIdentity -> builder)
    classesByClassIdentity += (umlClass.classIdentity -> umlClass)


  def getUMLClass(classIdentity: UMLClassIdentity): Option[UMLClass] =
    classesByClassIdentity.get(classIdentity)

  def getUMLClassBuilder(classIdentity: UMLClassIdentity): Option[UMLClass.ClassBuilder] =
    buildersByClassIdentity.get(classIdentity)

  def getUMLClassBuilder(name: String): Option[UMLClass.ClassBuilder] =
    ClassIdentityBuilderCache.getUMLClassIdentity(name)
      .flatMap(getUMLClassBuilder)
    
  def getClasses: Set[UMLClass] = classesByClassIdentity.values.toSet

