package org.uml2semantics.model.cache

import org.uml2semantics.diagnostics.SourceLocation
import org.uml2semantics.model.{UMLEnumeration, UMLEnumerationIdentity}

import scala.collection.mutable

/**
 * The enumerations read so far, each holding its values.
 *
 * Unlike the class and attribute caches this one keeps no builders. Enrichment happens here, by
 * merging what is cached with what arrives, so there is nothing to fetch, extend and rebuild.
 */
object EnumerationCache:
  private val enumerations = mutable.ArrayBuffer[UMLEnumeration]()
  private val declarations = mutable.ArrayBuffer[(UMLEnumerationIdentity, SourceLocation)]()

  def clear(): Unit =
    enumerations.clear()
    declarations.clear()

  /**
   * Notes where an input declared an enumeration. Something can be found out about an enumeration
   * only after every input has been read, namely that it ended up without values, and by then the
   * reader that could have said where it was declared has long returned.
   */
  def recordDeclaration(identity: UMLEnumerationIdentity, location: SourceLocation): Unit =
    declarations += (identity -> location)

  /** Where `enumeration` was first declared, if an input declared it instead of building it on the fly. */
  def whereDeclared(enumeration: UMLEnumeration): Option[SourceLocation] =
    declarations.collectFirst {
      case (identity, location) if identity.sharesAHalfWith(enumeration.identity) => location
    }

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
