package org.uml2semantics.model

import org.uml2semantics.diagnostics.{DiagnosticCode, UMLModelException}
import org.uml2semantics.model.cache.{ClassBuilderCache, EnumerationCache}

/**
 * Reads the two halves of an identity, a name and a curie, as the modeller wrote them.
 *
 * Each half is judged on its own: a name with a colon in it or a curie whose prefix is not
 * configured is an error even when the other half is fine, because silently dropping a half the
 * modeller wrote would leave them with an identity they did not ask for.
 */
private[model] object IdentityHalves:

  /** `kind` names what is being identified, for the messages: "An enumeration", say. */
  def read(kind: String, name: Option[String], curie: Option[String]): (Option[String], Option[Curie]) =
    val givenName = name.filterNot(_.isBlank)
    val givenCurie = curie.filterNot(_.isBlank)

    givenName.filter(_.contains(':')).foreach(nameWithColon =>
      throw new UMLModelException(DiagnosticCode.NameContainsColon,
        s"Could not interpret '$nameWithColon' as a name, because a name may not contain a colon. " +
          s"If it is intended to be a curie, put it in the Curie column."))

    val resolvedCurie = givenCurie.map(unresolved => Curie.fromString(unresolved).getOrElse(
      throw new UMLModelException(DiagnosticCode.UnresolvableCurie,
        s"Could not interpret '$unresolved' as a curie. Its prefix must be configured via the -p or " +
          s"-x command line options.")))

    if givenName.isEmpty && resolvedCurie.isEmpty then
      throw new UMLModelException(DiagnosticCode.BlankIdentity, s"$kind needs a name or a curie; both were blank.")

    (givenName, resolvedCurie)

  /** The name-or-curie rule: an identifier containing a colon is a curie, anything else is a name. */
  def readNameOrCurie(kind: String, nameOrCurie: String): (Option[String], Option[Curie]) =
    if nameOrCurie.contains(':') then read(kind, None, Some(nameOrCurie))
    else read(kind, Some(nameOrCurie), None)

case class UMLEnumerationName(name: String)

/**
 * How an enumeration is known: a name, a curie, or both. The ontology prefix is the one a
 * name-only identity will be minted under.
 */
case class UMLEnumerationIdentity(nameOption: Option[UMLEnumerationName] = None,
                                  curieOption: Option[Curie] = None,
                                  ontologyPrefix: PrefixNamespace) extends LabeledIRI:

  /** Minted as a class IRI is: the curie if there is one, otherwise the name under the ontology prefix. */
  def getIRI: String =
    (curieOption, nameOption) match
      case (Some(curie), _) => curie.getIRI
      case (_, Some(name)) => val iri = ontologyPrefix.prefixIRI.iri
        iri + (if iri.matches(".*[/#]$") then name.name else s"$PATH_SEPARATOR" + name.name)
      case _ => throw new UMLModelException(DiagnosticCode.BlankIdentity, "Name and curie must not be empty.")

  /** The label prefers the name over the curie, as it does for a class. */
  def getLabel: String =
    nameOption.map(_.name).orElse(curieOption.map(_.curie)).getOrElse(
      throw new UMLModelException(DiagnosticCode.BlankIdentity, "Name and curie must not be empty."))

  /** Whether `nameOrCurie` is either half of this identity. */
  def isIdentifiedBy(nameOrCurie: String): Boolean =
    nameOption.exists(_.name == nameOrCurie) || curieOption.exists(_.curie == nameOrCurie)

  /** Two identities sharing a name or a curie identify the same enumeration. */
  def sharesAHalfWith(other: UMLEnumerationIdentity): Boolean =
    nameOption.exists(other.nameOption.contains) || curieOption.exists(other.curieOption.contains)

case class UMLEnumerationDefinition(definition: String)

case class UMLEnumerationValueName(name: String)

/**
 * How a value is known within its enumeration. A value carries no reference to its enumeration:
 * it lives inside it, which is what scopes its name. For the same reason it cannot mint its own
 * IRI; see `UMLEnumeration.getValueIRI`.
 */
case class UMLEnumerationValueIdentity(nameOption: Option[UMLEnumerationValueName] = None,
                                       curieOption: Option[Curie] = None):

  def getLabel: String =
    nameOption.map(_.name).orElse(curieOption.map(_.curie)).getOrElse(
      throw new UMLModelException(DiagnosticCode.BlankIdentity, "Name and curie must not be empty."))

  def sharesAHalfWith(other: UMLEnumerationValueIdentity): Boolean =
    nameOption.exists(other.nameOption.contains) || curieOption.exists(other.curieOption.contains)

case class UMLEnumerationValueDefinition(definition: String)

/** A UML enumeration literal. */
case class UMLEnumerationValue(identity: UMLEnumerationValueIdentity,
                               definition: Option[UMLEnumerationValueDefinition] = None):

  def getLabel: String = identity.getLabel

  /** This value with whatever `newer` supplies laid over it; what `newer` leaves out is kept. */
  def enrichedBy(newer: UMLEnumerationValue): UMLEnumerationValue =
    UMLEnumerationValue(
      UMLEnumerationValueIdentity(
        newer.identity.nameOption.orElse(identity.nameOption),
        newer.identity.curieOption.orElse(identity.curieOption)),
      newer.definition.orElse(definition))

object UMLEnumerationValue:

  def builder(prefixNamespace: PrefixNamespace): EnumerationValueBuilder =
    EnumerationValueBuilder(prefixNamespace)

  class EnumerationValueBuilder(prefixNamespace: PrefixNamespace):
    private var enumeration: Option[String] = None
    private var name: Option[String] = None
    private var curie: Option[String] = None
    private var definition: Option[String] = None

    /** The enumeration the value belongs to, by its name or its curie. */
    def withEnumeration(nameOrCurie: String): EnumerationValueBuilder =
      this.enumeration = Some(nameOrCurie)
      this

    def withNameAndCurie(name: String, curie: String): EnumerationValueBuilder =
      this.name = Some(name)
      this.curie = Some(curie)
      this

    /** For input that has one field for both: an identifier with a colon is a curie, any other a name. */
    def withNameOrCurie(nameOrCurie: String): EnumerationValueBuilder =
      if nameOrCurie.contains(':') then withNameAndCurie("", nameOrCurie) else withNameAndCurie(nameOrCurie, "")

    def withDefinition(definition: String): EnumerationValueBuilder =
      this.definition = Some(definition)
      this

    /**
     * Builds the value and adds it to its enumeration in the cache. An enumeration that was never
     * declared is built on the fly, but only once the value itself has been accepted: a value we
     * refused is not evidence that its enumeration exists.
     */
    def build: UMLEnumerationValue =
      val (valueName, valueCurie) = IdentityHalves.read("An enumeration value", name, curie)
      val value = UMLEnumerationValue(
        UMLEnumerationValueIdentity(valueName.map(UMLEnumerationValueName(_)), valueCurie),
        definition.filterNot(_.isBlank).map(UMLEnumerationValueDefinition(_)))

      val enumerationNameOrCurie = enumeration.filterNot(_.isBlank).getOrElse(
        throw new UMLModelException(DiagnosticCode.BlankIdentity,
          "An enumeration value needs the enumeration it belongs to; none was given."))
      val cachedEnumeration = EnumerationCache.getUMLEnumeration(enumerationNameOrCurie)
      val enumerationIdentity = cachedEnumeration.map(_.identity)
        .getOrElse(UMLEnumeration.identityOfNewEnumeration(
          IdentityHalves.readNameOrCurie("An enumeration", enumerationNameOrCurie), prefixNamespace))
      cachedEnumeration.foreach(_.refuseValueWithTakenIRI(value))

      EnumerationCache.cacheUMLEnumeration(UMLEnumeration(enumerationIdentity, None, Set(value)))
      value

/**
 * A UML enumeration together with its values. The values are held by the enumeration rather than
 * cached beside it, so a value cannot exist outside an enumeration and enriching an enumeration's
 * identity never has to chase its values.
 */
case class UMLEnumeration(identity: UMLEnumerationIdentity,
                          definition: Option[UMLEnumerationDefinition] = None,
                          values: Set[UMLEnumerationValue] = Set.empty):

  def getLabel: String = identity.getLabel

  def getIRI: String = identity.getIRI

  /**
   * The IRI of one of this enumeration's values. A curie expands directly. A name-only value is
   * minted under the ontology prefix and qualified by the label of this enumeration, exactly as a
   * name-only attribute is qualified by its class, so that `Other` in two enumerations is two IRIs.
   * It cannot collide with an attribute's IRI, because a class and an enumeration never share a
   * label. Value names are the one kind of name that is often not a plain identifier, so the name is
   * made safe for an IRI first; the label keeps it as given.
   */
  def getValueIRI(value: UMLEnumerationValue): String =
    (value.identity.curieOption, value.identity.nameOption) match
      case (Some(curie), _) => curie.getIRI
      case (_, Some(name)) => val iri = identity.ontologyPrefix.prefixIRI.iri
        val safeName = UMLEnumeration.safeInIRI(name.name)
        iri + (if iri.matches(".*#$") then getLabel + PATH_SEPARATOR + safeName
          else if iri.matches(".*/$") then getLabel + FRAGMENT_SEPARATOR + safeName
          else s"$PATH_SEPARATOR" + getLabel + FRAGMENT_SEPARATOR + safeName)
      case _ => throw new UMLModelException(DiagnosticCode.BlankIdentity, "Name and curie must not be empty.")

  /**
   * Refuses a value about to join this enumeration when another of its values already has the IRI
   * the newcomer would get, as `Hp 0` and `Hp_0` do once the space has become an underscore. Two
   * values sharing an IRI would be one individual with two labels. A newcomer that is a value
   * already here, arriving again to be enriched, is judged as the value it will become.
   */
  private[model] def refuseValueWithTakenIRI(newcomer: UMLEnumerationValue): Unit =
    val (sameValue, otherValues) = values.partition(_.identity.sharesAHalfWith(newcomer.identity))
    val enriched = sameValue.foldLeft(newcomer)((enriching, older) => older.enrichedBy(enriching))
    val iri = getValueIRI(enriched)
    otherValues.find(getValueIRI(_) == iri).foreach(other =>
      throw new UMLModelException(DiagnosticCode.EnumerationValueIRIConflict,
        s"Value '${newcomer.getLabel}' of enumeration '$getLabel' would get the IRI $iri, which its value " +
          s"'${other.getLabel}' already has."))

  /**
   * This enumeration with whatever `newer` supplies laid over it. A half of the identity or a
   * definition that `newer` leaves out is kept, and the values of both are combined, a value
   * known to both being enriched in the same way.
   */
  def enrichedBy(newer: UMLEnumeration): UMLEnumeration =
    UMLEnumeration(
      UMLEnumerationIdentity(
        newer.identity.nameOption.orElse(identity.nameOption),
        newer.identity.curieOption.orElse(identity.curieOption),
        newer.identity.ontologyPrefix),
      newer.definition.orElse(definition),
      newer.values.foldLeft(values)(UMLEnumeration.withValue))

object UMLEnumeration:

  def builder(prefixNamespace: PrefixNamespace): EnumerationBuilder =
    EnumerationBuilder(prefixNamespace)

  /**
   * A value name as it goes into an IRI. Letters and digits of any script are kept, and so are the
   * hyphen, the full stop, the underscore and the tilde. Every other character becomes an
   * underscore, one for one, so `Sh 2/Sh 0` is `Sh_2_Sh_0`.
   */
  private def safeInIRI(valueName: String): String =
    valueName.codePoints().toArray.map(codePoint =>
      if Character.isLetterOrDigit(codePoint) || "-._~".indexOf(codePoint) >= 0 then Character.toString(codePoint)
      else "_").mkString

  private def withValue(values: Set[UMLEnumerationValue], newer: UMLEnumerationValue): Set[UMLEnumerationValue] =
    val (sameValue, otherValues) = values.partition(_.identity.sharesAHalfWith(newer.identity))
    otherValues + sameValue.foldLeft(newer)((enriching, older) => older.enrichedBy(enriching))

  /**
   * The identity of an enumeration that is about to come into being, either declared or built on
   * the fly. Classes and enumerations share one namespace, as classifiers do in a UML package, so
   * an identity that a class already holds is refused.
   */
  private[model] def identityOfNewEnumeration(halves: (Option[String], Option[Curie]),
                                              prefixNamespace: PrefixNamespace): UMLEnumerationIdentity =
    val (enumerationName, enumerationCurie) = halves

    (enumerationName ++ enumerationCurie.map(_.curie)).find(ClassBuilderCache.getUMLClass(_).isDefined)
      .foreach(half => throw new UMLModelException(DiagnosticCode.ClassifierKindConflict,
        s"'$half' already identifies a class, so it cannot also identify an enumeration."))

    UMLEnumerationIdentity(enumerationName.map(UMLEnumerationName(_)), enumerationCurie, prefixNamespace)

  class EnumerationBuilder(prefixNamespace: PrefixNamespace):
    private var name: Option[String] = None
    private var curie: Option[String] = None
    private var definition: Option[String] = None

    def withNameAndCurie(name: String, curie: String): EnumerationBuilder =
      this.name = Some(name)
      this.curie = Some(curie)
      this

    /** For input that has one field for both: an identifier with a colon is a curie, any other a name. */
    def withNameOrCurie(nameOrCurie: String): EnumerationBuilder =
      if nameOrCurie.contains(':') then withNameAndCurie("", nameOrCurie) else withNameAndCurie(nameOrCurie, "")

    def withDefinition(definition: String): EnumerationBuilder =
      this.definition = Some(definition)
      this

    /**
     * Builds the enumeration and caches it. The result is the enumeration as the cache now holds
     * it, which carries more than this builder was given when an earlier row already described it.
     */
    def build: UMLEnumeration =
      val identity = identityOfNewEnumeration(IdentityHalves.read("An enumeration", name, curie), prefixNamespace)
      EnumerationCache.cacheUMLEnumeration(
        UMLEnumeration(identity, definition.filterNot(_.isBlank).map(UMLEnumerationDefinition(_))))
