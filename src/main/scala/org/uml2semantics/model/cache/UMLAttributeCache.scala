package org.uml2semantics.model.cache

import org.uml2semantics.model.{UMLAttribute, UMLAttributeCurie, UMLAttributeDefinition, UMLAttributeIdentity, UMLAttributeName, UMLClassCurie, UMLClassIdentifier, UMLClassIdentity, UMLClassName, UMLMultiplicity}

import scala.collection.mutable

object AttributeIdentityCache:
  private val attributeIdentityByClassNameAttributeName = mutable.Map[UMLClassName, mutable.Map[UMLAttributeName, UMLAttributeIdentity]]()
  private val attributeIdentityByClassNameAttributeCurie = mutable.Map[UMLClassName, mutable.Map[UMLAttributeCurie, UMLAttributeIdentity]]()
  private val attributeIdentityByClassCurieAttributeName = mutable.Map[UMLClassCurie, mutable.Map[UMLAttributeName, UMLAttributeIdentity]]()
  private val attributeIdentityByClassCurieAttributeCurie = mutable.Map[UMLClassCurie, mutable.Map[UMLAttributeCurie, UMLAttributeIdentity]]()


  def clear(): Unit =
    attributeIdentityByClassNameAttributeName.clear()
    attributeIdentityByClassNameAttributeCurie.clear()
    attributeIdentityByClassCurieAttributeName.clear()
    attributeIdentityByClassCurieAttributeCurie.clear()

  def cacheUMLAttributeIdentity(attributeIdentity: UMLAttributeIdentity): UMLAttributeIdentity =
    def updateCache[K, V](map: mutable.Map[K, mutable.Map[V, UMLAttributeIdentity]],
                          key: K, subKey: V, value: UMLAttributeIdentity): Unit =
      if !map.contains(key) then map += (key -> mutable.Map(subKey -> value))
      else map(key) += (subKey -> value)

    attributeIdentity.classIdentity.nameOption.foreach { className =>
      attributeIdentity.nameOption.foreach { attributeName =>
        updateCache(attributeIdentityByClassNameAttributeName, className, attributeName, attributeIdentity)
      }
      attributeIdentity.curieOption.foreach { attributeCurie =>
        updateCache(attributeIdentityByClassNameAttributeCurie, className, attributeCurie, attributeIdentity)
      }
    }
    attributeIdentity.classIdentity.curieOption.foreach { classCurie =>
      attributeIdentity.nameOption.foreach { attributeName =>
        updateCache(attributeIdentityByClassCurieAttributeName, classCurie, attributeName, attributeIdentity)
      }
      attributeIdentity.curieOption.foreach { attributeCurie =>
        updateCache(attributeIdentityByClassCurieAttributeCurie, classCurie, attributeCurie, attributeIdentity)
      }
    }

    attributeIdentity

  def getUMLAttributeIdentity(attributeIdentity: UMLAttributeIdentity): Option[UMLAttributeIdentity] =
    attributeIdentity.classIdentity.nameOption.flatMap { className =>
      attributeIdentity.nameOption.flatMap { attributeName =>
        attributeIdentityByClassNameAttributeName.get(className).flatMap(_.get(attributeName))
      }.orElse {
        attributeIdentity.curieOption.flatMap { attributeCurie =>
          attributeIdentityByClassNameAttributeCurie.get(className).flatMap(_.get(attributeCurie))
        }
      }
    }.orElse {
      attributeIdentity.classIdentity.curieOption.flatMap { classCurie =>
        attributeIdentity.nameOption.flatMap { attributeName =>
          attributeIdentityByClassCurieAttributeName.get(classCurie).flatMap(_.get(attributeName))
        }.orElse {
          attributeIdentity.curieOption.flatMap { attributeCurie =>
            attributeIdentityByClassCurieAttributeCurie.get(classCurie).flatMap(_.get(attributeCurie))
          }
        }
      }
    }


object AttributeCache:
  private val attributesByAttributeIdentity = mutable.Map[UMLAttributeIdentity, UMLAttribute]()

  def clear(): Unit =
    attributesByAttributeIdentity.clear()

  def cacheUMLAttribute(umlAttribute: UMLAttribute): Unit =
    // When enriching with curie, remove any existing name-only version to avoid duplicates.
    // The old entry may exist under two possible keys:
    // 1. Original: name-only class identity + name-only attribute (before class was enriched)
    // 2. Migrated: enriched class identity + name-only attribute (after updateClassIdentity ran)
    var attributeToCache = umlAttribute
    if umlAttribute.attributeIdentity.nameOption.isDefined && umlAttribute.attributeIdentity.curieOption.isDefined then
      val classIdentity = umlAttribute.attributeIdentity.classIdentity
      // Try finding with the original name-only class identity key
      val nameOnlyClassIdentity = UMLClassIdentity(classIdentity.nameOption, None, classIdentity.ontologyPrefix)
      val oldAttributeName = umlAttribute.attributeIdentity.nameOption.flatMap(an =>
        UMLAttributeName(nameOnlyClassIdentity.getClassIdentifier, an.getName))
      val nameOnlyIdentity = UMLAttributeIdentity(nameOnlyClassIdentity, oldAttributeName, None)
      // Also try finding with the current (enriched) class identity but no attribute curie
      val migratedIdentity = UMLAttributeIdentity(classIdentity, umlAttribute.attributeIdentity.nameOption, None)
      // Find the old attribute under either key to merge its data
      val oldAttribute = attributesByAttributeIdentity.get(nameOnlyIdentity)
        .orElse(attributesByAttributeIdentity.get(migratedIdentity))
      oldAttribute.foreach { old =>
        val mergedType = umlAttribute.typeOfAttribute.orElse(old.typeOfAttribute)
        val mergedMultiplicity = if umlAttribute.multiplicity != UMLMultiplicity() then umlAttribute.multiplicity
          else old.multiplicity
        val mergedDefinition = umlAttribute.definition match
          case Some(UMLAttributeDefinition(d)) if d.nonEmpty => umlAttribute.definition
          case _ => old.definition
        attributeToCache = umlAttribute.copy(typeOfAttribute = mergedType, multiplicity = mergedMultiplicity, definition = mergedDefinition)
      }
      removeUMLAttribute(nameOnlyIdentity)
      removeUMLAttribute(migratedIdentity)
    attributesByAttributeIdentity += (attributeToCache.attributeIdentity -> attributeToCache)

  def removeUMLAttribute(attributeIdentity: UMLAttributeIdentity): Unit =
    attributesByAttributeIdentity -= attributeIdentity

  /**
   * Updates all cached attributes whose class identity matches oldClassIdentity
   * to use newClassIdentity instead. This is needed when a class is enriched with
   * a curie (e.g., Person -> schema:Person) so that attributes not explicitly
   * overridden (e.g., dateOfBirth) still reference the enriched class identity.
   */
  def updateClassIdentity(oldClassIdentity: UMLClassIdentity, newClassIdentity: UMLClassIdentity): Unit =
    val entriesToUpdate = attributesByAttributeIdentity.toList.filter(_._1.classIdentity == oldClassIdentity)
    entriesToUpdate.foreach { case (oldAttrIdentity, oldAttribute) =>
      val newAttrName = oldAttrIdentity.nameOption.map(_.copy(classIdentifier = newClassIdentity.getClassIdentifier))
      val newAttrCurie = oldAttrIdentity.curieOption.map(_.copy(classIdentifier = newClassIdentity.getClassIdentifier))
      val newAttrIdentity = UMLAttributeIdentity(newClassIdentity, newAttrName, newAttrCurie)
      val newAttribute = oldAttribute.copy(attributeIdentity = newAttrIdentity)

      attributesByAttributeIdentity -= oldAttrIdentity
      attributesByAttributeIdentity += (newAttrIdentity -> newAttribute)
    }

  def getAttributes: Set[UMLAttribute] = attributesByAttributeIdentity.values.toSet