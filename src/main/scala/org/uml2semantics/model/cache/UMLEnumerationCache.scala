package org.uml2semantics.model.cache

import org.uml2semantics.model.UMLEnumeration

import scala.collection.mutable

/**
 * The enumerations read so far, each holding its values.
 *
 * Unlike the class and attribute caches this one keeps no builders. Enrichment happens here, by
 * merging what is cached with what arrives, so there is nothing to fetch, extend and rebuild.
 */
object EnumerationCache:
  private val enumerations = mutable.ArrayBuffer[UMLEnumeration]()

  def clear(): Unit =
    enumerations.clear()

  /**
   * Caches an enumeration, enriching whatever is already cached under either half of its identity,
   * and returns the result. An enumeration carrying both a name and a curie can match two entries,
   * one known only by the name and one only by the curie; all three then become one.
   */
  def cacheUMLEnumeration(enumeration: UMLEnumeration): UMLEnumeration =
    val alreadyCached = enumerations.filter(_.identity.sharesAHalfWith(enumeration.identity)).toList
    val enriched = alreadyCached.foldLeft(enumeration)((enriching, older) => older.enrichedBy(enriching))
    alreadyCached match
      case first :: rest =>
        rest.foreach(enumerations -= _)
        enumerations(enumerations.indexOf(first)) = enriched
      case Nil =>
        enumerations += enriched
    enriched

  /** The enumeration that `nameOrCurie` is the name or the curie of, if there is one. */
  def getUMLEnumeration(nameOrCurie: String): Option[UMLEnumeration] =
    enumerations.find(_.identity.isIdentifiedBy(nameOrCurie))

  def getEnumerations: Set[UMLEnumeration] = enumerations.toSet
