package org.uml2semantics.model.cache

import org.uml2semantics.model.{UMLAttributeCurie, UMLAttributeIdentifier, UMLAttributeIdentity, UMLAttributeName, UMLClassCurie, UMLClassIdentifier, UMLClassIdentity, UMLClassName}
import org.uml2semantics.model.UMLAttributeIdentity.AttributeIdentityBuilder
import org.uml2semantics.model.cache.ClassIdentityBuilderCache

import scala.collection.mutable

object AttributeIdentityBuilderCache:
  private val buildersByClassNameAttributeName =  mutable.Map[UMLClassName, mutable.Map[UMLAttributeName, AttributeIdentityBuilder]]()
  private val buildersByClassNameAttributeCurie = mutable.Map[UMLClassName, mutable.Map[UMLAttributeCurie, AttributeIdentityBuilder]]()
  private val buildersByClassCurieAttributeName = mutable.Map[UMLClassCurie, mutable.Map[UMLAttributeName, AttributeIdentityBuilder]]()
  private val buildersByClassCurieAttributeCurie = mutable.Map[UMLClassCurie, mutable.Map[UMLAttributeCurie, AttributeIdentityBuilder]]()
  private val attributeIdentityByClassNameAttributeName = mutable.Map[UMLClassName, mutable.Map[UMLAttributeName, UMLAttributeIdentity]]()
  private val attributeIdentityByClassNameAttributeCurie = mutable.Map[UMLClassName, mutable.Map[UMLAttributeCurie, UMLAttributeIdentity]]()
  private val attributeIdentityByClassCurieAttributeName = mutable.Map[UMLClassCurie, mutable.Map[UMLAttributeName, UMLAttributeIdentity]]()
  private val attributeIdentityByClassCurieAttributeCurie = mutable.Map[UMLClassCurie, mutable.Map[UMLAttributeCurie, UMLAttributeIdentity]]()


  def cacheUMLAttributeIdentity(attributeIdentity: UMLAttributeIdentity, builder: AttributeIdentityBuilder): UMLAttributeIdentity =
    def updateCache[K, V](map: mutable.Map[K, mutable.Map[V, UMLAttributeIdentity]],
                          key: K, subKey: V, value: UMLAttributeIdentity): Unit =
      if !map.contains(key) then map += (key -> mutable.Map(subKey -> value))
      else map(key) += (subKey -> value)

    def updateBuilderCache[K, V](map: mutable.Map[K, mutable.Map[V, AttributeIdentityBuilder]],
                                 key: K, subKey: V, value: AttributeIdentityBuilder): Unit =
      if !map.contains(key) then map += (key -> mutable.Map(subKey -> value))
      else map(key) += (subKey -> value)

    attributeIdentity.classIdentity.nameOption.foreach { className =>
      attributeIdentity.nameOption.foreach { attributeName =>
        updateCache(attributeIdentityByClassNameAttributeName, className, attributeName, attributeIdentity)
        updateBuilderCache(buildersByClassNameAttributeName, className, attributeName, builder)
      }
      attributeIdentity.curieOption.foreach { attributeCurie =>
        updateCache(attributeIdentityByClassNameAttributeCurie, className, attributeCurie, attributeIdentity)
        updateBuilderCache(buildersByClassNameAttributeCurie, className, attributeCurie, builder)
      }
    }
    attributeIdentity.classIdentity.curieOption.foreach { classCurie =>
      attributeIdentity.nameOption.foreach { attributeName =>
        updateCache(attributeIdentityByClassCurieAttributeName, classCurie, attributeName, attributeIdentity)
        updateBuilderCache(buildersByClassCurieAttributeName, classCurie, attributeName, builder)
      }
      attributeIdentity.curieOption.foreach { attributeCurie =>
        updateCache(attributeIdentityByClassCurieAttributeCurie, classCurie, attributeCurie, attributeIdentity)
        updateBuilderCache(buildersByClassCurieAttributeCurie, classCurie, attributeCurie, builder)
      }
    }

    attributeIdentity

  def getUMLAttributeIdentity(classIdentifier: String, attributeIdentifier: String): Option[UMLAttributeIdentity] =
    ClassIdentityBuilderCache.getUMLClassIdentity(classIdentifier).flatMap { cachedClassIdentity =>
      val attributeIdentity = UMLAttributeIdentity(cachedClassIdentity, attributeIdentifier)
      (attributeIdentity.nameOption, attributeIdentity.curieOption) match
        case (_, Some(attributeCurie)) =>
          cachedClassIdentity.nameOption.flatMap(attributeIdentityByClassNameAttributeCurie.get(_).flatMap(_.get(attributeCurie)))
            .orElse(cachedClassIdentity.curieOption.flatMap(attributeIdentityByClassCurieAttributeCurie.get(_).flatMap(_.get(attributeCurie))))
        case (Some(attributeName), _) =>
          cachedClassIdentity.nameOption.flatMap(attributeIdentityByClassNameAttributeName.get(_).flatMap(_.get(attributeName)))
            .orElse(cachedClassIdentity.curieOption.flatMap(attributeIdentityByClassCurieAttributeName.get(_).flatMap(_.get(attributeName))))
        case _ => throw new IllegalArgumentException("Name and curie must not be empty.")
    }

  def getUMLAttributeIdentity(classIdentity: UMLClassIdentity, attributeIdentifier: UMLAttributeIdentifier):
    Option[UMLAttributeIdentity] =

    attributeIdentifier match
      case name: UMLAttributeName =>
        classIdentity.nameOption.flatMap(attributeIdentityByClassNameAttributeName.get(_).flatMap(_.get(name)))
          .orElse(classIdentity.curieOption.flatMap(attributeIdentityByClassCurieAttributeName.get(_).flatMap(_.get(name))))
      case curie: UMLAttributeCurie =>
        classIdentity.nameOption.flatMap(attributeIdentityByClassNameAttributeCurie.get(_).flatMap(_.get(curie)))
          .orElse(classIdentity.curieOption.flatMap(attributeIdentityByClassCurieAttributeCurie.get(_).flatMap(_.get(curie))))

  def getUMLAttributeIdentityBuilder(classIdentity: UMLClassIdentity, attributeIdentity: UMLAttributeIdentity): 
    Option[AttributeIdentityBuilder] =
    
    (attributeIdentity.curieOption, attributeIdentity.nameOption) match
      case (Some(curie), Some(name)) =>
        buildersByClassCurieAttributeCurie.get(classIdentity.curieOption.get)
          .flatMap(_.get(curie))
          .orElse(buildersByClassNameAttributeName.get(classIdentity.nameOption.get)
            .flatMap(_.get(name)))
      case (Some(curie), None) =>
        buildersByClassCurieAttributeCurie.get(classIdentity.curieOption.get)
          .flatMap(_.get(curie))
      case (None, Some(name)) =>
        buildersByClassNameAttributeName.get(classIdentity.nameOption.get)
          .flatMap(_.get(name))
      case (None, None) => throw new IllegalArgumentException("AttributeIdentity must have a name or curie")






  
    
    









