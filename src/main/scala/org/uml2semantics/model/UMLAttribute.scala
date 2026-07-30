package org.uml2semantics.model

import com.typesafe.scalalogging.Logger
import org.uml2semantics.diagnostics.{DiagnosticCode, Diagnostics, SourceLocation, UMLModelException}
import org.uml2semantics.inline.Code
import org.uml2semantics.model.UMLCardinality.>=
import org.uml2semantics.model.cache.{AttributeBuilderCache, AttributeIdentityBuilderCache, ClassBuilderCache}

sealed trait UMLAttributeIdentifier(classIdentifier: UMLClassIdentifier) extends UMLNamedElement

case class UMLAttributeName(classIdentifier: UMLClassIdentifier, name: String) extends UMLAttributeIdentifier(classIdentifier):
  def isEmpty: Boolean = name.isEmpty
  def nonEmpty: Boolean = name.nonEmpty
  def getName: String = name

object UMLAttributeName:
  def apply(classIdentifier: UMLClassIdentifier, name: String): Option[UMLAttributeName] =
    Option.when(classIdentifier.nonEmpty && !name.isBlank && !name.contains(':'))
      (new UMLAttributeName(classIdentifier, name))

case class UMLAttributeCurie(classIdentifier: UMLClassIdentifier, curie: Curie) extends UMLAttributeIdentifier(classIdentifier):
  def isEmpty: Boolean = curie.isEmpty
  def nonEmpty: Boolean = curie.nonEmpty
  def getName: String = curie.curie

object UMLAttributeCurie:
  def apply(classIdentifier: UMLClassIdentifier, curie: String): Option[UMLAttributeCurie] =
    if classIdentifier.nonEmpty then
      Curie.fromString(curie).map(new UMLAttributeCurie(classIdentifier: UMLClassIdentifier, _))
    else None


case class UMLAttributeIdentity(classIdentity: UMLClassIdentity,
                                nameOption: Option[UMLAttributeName] = None,
                                curieOption: Option[UMLAttributeCurie] = None) extends LabeledIRI:
  def getIRI: String =
    (curieOption, nameOption) match
      case (Some(curie), _) => curie.curie.getIRI
      case (_, Some(name)) => val iri = classIdentity.ontologyPrefix.prefixIRI.iri
        iri + (if iri.matches(".*#$") then classIdentity.getLabel + PATH_SEPARATOR + name.getName
          else if iri.matches(".*/$") then classIdentity.getLabel + FRAGMENT_SEPARATOR + name.getName
          else  s"$PATH_SEPARATOR" + classIdentity.getLabel + FRAGMENT_SEPARATOR + name.getName)
      case _ => throw new UMLModelException(DiagnosticCode.BlankIdentity, "Name and curie must not be empty.")

  def getLabel: String =
    (curieOption, nameOption) match
      case (_, Some(name)) => name.getName
      case (Some(curie), _) => curie.getName
      case _ => throw new UMLModelException(DiagnosticCode.BlankIdentity, "Name and curie must not be empty.")

object UMLAttributeIdentity:
  def apply(classIdentity: UMLClassIdentity, attributeIdentifier: String): UMLAttributeIdentity =
    (UMLAttributeCurie(classIdentity.getClassIdentifier, attributeIdentifier),
      UMLAttributeName(classIdentity.getClassIdentifier, attributeIdentifier)) match
      case (Some(curie), _) => UMLAttributeIdentity(classIdentity, None, Some(curie))
      case (_, Some(name)) => UMLAttributeIdentity(classIdentity, Some(name), None)
      case _ => throw new UMLModelException(DiagnosticCode.UnresolvableCurie,
        s"Could not interpret '$attributeIdentifier' as an attribute name or curie. If it is intended to be a " +
          s"curie, its prefix must be configured via the -p or -x command line options.")
      
//  def apply(classIdentifier: UMLClassIdentifier, attributeIdentifier: UMLAttributeIdentifier):  UMLAttributeIdentity =
//    val classIdentity = UMLClassIdentity(classIdentifier)
//    attributeIdentifier match
//      case name: UMLAttributeName => UMLAttributeIdentity(classIdentifier, Some(name), None)
//      case curie: UMLAttributeCurie => UMLAttributeIdentity(classIdentifier, None, Some(curie))


  class AttributeIdentityBuilder(var prefixNamespace: PrefixNamespace):
    protected var classIdentityBuilder = UMLClassIdentity.builder(prefixNamespace)
    protected var nameOption: Option[String] = None
    protected var curieOption: Option[String] = None

    def withClassName(name: String): AttributeIdentityBuilder =
      this.classIdentityBuilder = classIdentityBuilder.withName(name)
      this

    def withClassCurie(curie: String): AttributeIdentityBuilder =
      this.classIdentityBuilder = classIdentityBuilder.withCurie(curie)
      this

    def withClassIdentity(classIdentity: UMLClassIdentity): AttributeIdentityBuilder =
      this.classIdentityBuilder = classIdentityBuilder.withClassIdentity(classIdentity)
      this

    def withName(name: String): AttributeIdentityBuilder =
      this.nameOption = Some(name)
      this

    def withCurie(curieAsString: String): AttributeIdentityBuilder =
      this.curieOption = Some(curieAsString)
      this

    def withNameAndCurie(name: String, curie: String): AttributeIdentityBuilder =
      this.nameOption = Some(name)
      this.curieOption = Some(curie)
      this

    def withNameOrCurie(nameOrCurie: String): AttributeIdentityBuilder =
      if nameOrCurie.contains(':') then
        this.curieOption = Some(nameOrCurie)
      else
        this.nameOption = Some(nameOrCurie)
      this

    def build: UMLAttributeIdentity =
      val classIdentity = classIdentityBuilder.build

      if nameOption.isEmpty && curieOption.isEmpty then
        throw new UMLModelException(DiagnosticCode.BlankIdentity, "Name and curie must not be empty.")

      val attributeNameOption: Option[UMLAttributeName] = nameOption.flatMap(UMLAttributeName(
        classIdentity.getClassIdentifier, _))
      val attributeCurieOption: Option[UMLAttributeCurie] = curieOption.flatMap(UMLAttributeCurie(
        classIdentity.getClassIdentifier, _))
      if attributeNameOption.isEmpty && attributeCurieOption.isEmpty then
        (nameOption.filterNot(_.isBlank), curieOption.filterNot(_.isBlank)) match
          case (None, None) =>
            throw new UMLModelException(DiagnosticCode.BlankIdentity,
              "An attribute needs a name or a curie; both were blank.")
          case (Some(nameWithColon), None) =>
            throw new UMLModelException(DiagnosticCode.NameContainsColon,
              s"Could not interpret '$nameWithColon' as an attribute name, because a name may not contain a colon. " +
                s"If it is intended to be a curie, put it in the Curie column.")
          case (_, Some(unresolvableCurie)) =>
            throw new UMLModelException(DiagnosticCode.UnresolvableCurie,
              s"Could not interpret '$unresolvableCurie' as a curie. Its prefix must be configured via the -p or " +
                s"-x command line options.")
      val thisAttributeIdentity: UMLAttributeIdentity = UMLAttributeIdentity(classIdentity, attributeNameOption, attributeCurieOption)
      if attributeNameOption.isDefined && attributeCurieOption.isDefined then
        AttributeIdentityBuilderCache.cacheUMLAttributeIdentity(thisAttributeIdentity,  this)
        return thisAttributeIdentity

      AttributeIdentityBuilderCache.getUMLAttributeIdentity(thisAttributeIdentity).getOrElse {
        AttributeIdentityBuilderCache.cacheUMLAttributeIdentity(thisAttributeIdentity,  this)
      }


opaque type UMLInfinite <: Char = '*'

object UMLInfinite:
  def apply(): UMLInfinite = '*'

  def unapply(s: String): Boolean = s == "*"

opaque type UMLNonNegativeInteger <: Int = Int

object UMLNonNegativeInteger:
  def apply(n: Int): UMLNonNegativeInteger =
    require(n >= 0)
    n

  def unapply(s: String): Option[UMLNonNegativeInteger] =
    try
      val someInt: Int = s.toInt
      if someInt >= 0 then
        Some(UMLNonNegativeInteger(someInt))
      else
        None
    catch
      case e: Exception => None

sealed trait UMLCardinality

case class UMLInfiniteCardinality(infinite: UMLInfinite) extends UMLCardinality

case class UMLNonNegativeCardinality(nonNegativeInteger: UMLNonNegativeInteger) extends UMLCardinality

object UMLCardinality:
  private val logger = Logger[this.type]

  private val One = UMLNonNegativeCardinality(UMLNonNegativeInteger(1))

  /**
   * A cardinality defaults to 1 when it is unspecified, in line with the UML specification,
   * and also when it cannot be parsed — a typo degrades to the default rather than losing the
   * attribute. The unparsable case is reported as a warning so the modeller can find the typo.
   */
  def apply(s: String,
            diagnostics: Diagnostics = Diagnostics.discarding,
            location: Option[SourceLocation] = None): UMLCardinality =
    logger.debug(s"s=$s ${Code.source}")
    s match
      case UMLNonNegativeInteger(i) => UMLNonNegativeCardinality(i)
      case UMLInfinite() => UMLInfiniteCardinality(UMLInfinite())
      case _ if s.isBlank => One
      case _ =>
        val message = s"Could not read '$s' as a cardinality; expected a non-negative integer or '*'. Using 1."
        logger.warn(message)
        diagnostics.warn(DiagnosticCode.UnparsableCardinality, message, location)
        One

  def >=(c1: UMLCardinality, c2: UMLCardinality): Boolean =
    logger.debug(s"c1=$c1, c2=$c2 ${Code.source}")
    (c1, c2) match
      case (UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) => true
      case (UMLInfiniteCardinality(_), UMLNonNegativeCardinality(_)) => true
      case (UMLNonNegativeCardinality(_), UMLInfiniteCardinality(_)) => false
      case (UMLNonNegativeCardinality(t1), UMLNonNegativeCardinality(t2)) => t1 >= t2

  def ==(c1: UMLCardinality, c2: UMLCardinality): Boolean =
    logger.debug(s"c1=$c1, c2=$c2 ${Code.source}")
    (c1, c2) match
      case (UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) => true
      case (UMLInfiniteCardinality(_), UMLNonNegativeCardinality(_)) => false
      case (UMLNonNegativeCardinality(_), UMLInfiniteCardinality(_)) => false
      case (UMLNonNegativeCardinality(t1), UMLNonNegativeCardinality(t2)) => t1 == t2

/**
 * The constructor is private so that `apply` is the only way in. Without that, the synthesized
 * two-argument constructor would build multiplicities that break the invariant while bypassing the
 * check entirely — an invariant that is declared but not enforced.
 */
case class UMLMultiplicity private (min: UMLCardinality = UMLCardinality("1"),
                                    max: UMLCardinality = UMLCardinality("1"))

object UMLMultiplicity:
  private val logger = Logger[this.type]

  def apply(): UMLMultiplicity =
    logger.debug(s"${Code.source}")
    new UMLMultiplicity()

  /** [0,*] — the multiplicity that constrains nothing, and so asserts nothing. */
  private val Unconstrained =
    new UMLMultiplicity(UMLNonNegativeCardinality(UMLNonNegativeInteger(0)), UMLInfiniteCardinality(UMLInfinite()))

  /**
   * An unusable multiplicity costs the attribute its cardinality restriction, not its existence.
   * The pair falls back to [0,*] — asserting nothing about cardinality — and the attribute keeps its
   * type, its bounds' owner, its label and its definition. Asserting no cardinality is weaker than
   * the modeller intended; asserting a repaired one would be a constraint they never wrote, and a
   * wrong exact cardinality can make otherwise sound instance data inconsistent.
   */
  def apply(min: UMLCardinality, max: UMLCardinality,
            diagnostics: Diagnostics, location: Option[SourceLocation]): UMLMultiplicity =
    logger.debug(s"min=$min, max=$max ${Code.source}")
    (min, max) match
      case (UMLInfiniteCardinality(_), UMLNonNegativeCardinality(maxCardinality)) =>
        val message = s"An unbounded min cardinality is meaningless: [*, $maxCardinality] asserts no " +
          s"cardinality restriction."
        logger.error(message)
        diagnostics.error(DiagnosticCode.UnboundedMinMultiplicity, message, location)
        Unconstrained
      case _ if !(>=(max, min)) =>
        val message = s"Max cardinality must be greater than or equal to min cardinality, but min=$min " +
          s"and max=$max found; no cardinality restriction is asserted."
        logger.error(message)
        diagnostics.error(DiagnosticCode.MaxBelowMin, message, location)
        Unconstrained
      case _ => new UMLMultiplicity(min, max)

sealed trait UMLAttributeType
case class UMLXMLDataType(attributeType: SupportedDataType) extends UMLAttributeType
case class UMLClassType(attributeType: UMLClass) extends UMLAttributeType
case class CurieBasedUMLClassAttributeType(attributeType: Curie) extends UMLAttributeType

object UMLAttributeType:
  private val logger = Logger[this.type]

  def apply(attributeTypeName: String, ontologyPrefix: PrefixNamespace): UMLAttributeType =
    logger.debug(s"s=$attributeTypeName ${Code.source}")
    // Precondition guard. A blank type is handled by AttributeBuilder.withType, which is the only
    // place that can express "no type"; without this, resolution would fall through to building a
    // class from the blank string and report a blank *class identity* — the wrong column entirely.
    if attributeTypeName.isBlank then
      throw new UMLModelException(DiagnosticCode.BlankAttributeType,
        "An attribute needs a type; a blank type matches no datatype, no class and no curie.")
    val curieOption = Curie.fromString(attributeTypeName)
    val attributeType = curieOption.flatMap(curie => SupportedDataType(curie))
      .orElse(ClassBuilderCache.getUMLClass(attributeTypeName))
      .orElse(curieOption.flatMap(_ => curieOption))
      .orElse(Some(UMLClass.builder(ontologyPrefix).withNameOrCurie(attributeTypeName).build))
      .get

    attributeType match
      case dataType: SupportedDataType => UMLXMLDataType(dataType)
      case umlClass: UMLClass => UMLClassType(umlClass)
      case curie: Curie => CurieBasedUMLClassAttributeType(curie)

case class UMLAttributeDefinition(definition: String)

case class UMLAttribute(attributeIdentity: UMLAttributeIdentity,
                        typeOfAttribute: Option[UMLAttributeType] = None,
                        multiplicity: UMLMultiplicity = UMLMultiplicity(),
                        definition: Option[UMLAttributeDefinition] = None) extends LabeledIRI:

  def getIRI: String = attributeIdentity.getIRI

  def getLabel: String = attributeIdentity.getLabel

object UMLAttribute:
  def builder (prefixNamespace: PrefixNamespace): AttributeBuilder = new AttributeBuilder(prefixNamespace)

  class AttributeBuilder(prefixNamespace: PrefixNamespace):
    private var attributeIdentityBuilder = UMLAttributeIdentity.AttributeIdentityBuilder(prefixNamespace)
    private var typeOfAttribute: Option[UMLAttributeType] = None
    private var multiplicity: UMLMultiplicity = UMLMultiplicity()
    private var definition: Option[String] = None

    def withClassName(name: String): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withClassName(name)
      this

    def withClassCurie(curie: String): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withClassCurie(curie)
      this

    def withClassIdentity(classIdentity: UMLClassIdentity): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withClassIdentity(classIdentity)
      this

    def withName(name: String): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withName(name)
      this

    def withCurie(curieAsString: String): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withCurie(curieAsString)
      this

    def withNameAndCurie(name: String, curie: String): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withNameAndCurie(name, curie)
      this

    def withNameOrCurie(nameOrCurie: String): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withNameOrCurie(nameOrCurie)
      this

    def withType(attributeType: UMLAttributeType): AttributeBuilder =
      this.typeOfAttribute = Some(attributeType)
      this

    /**
     * A blank type leaves the attribute genuinely untyped rather than rejecting it. Two kinds of
     * modeller use this tool: one converting a finished UML diagram, for whom a missing type is an
     * omission, and one drafting an ontology through UML because they are not fluent in OWL, for
     * whom an untyped attribute is a legitimate intermediate state. Since the tool cannot tell them
     * apart, it accepts the attribute and warns.
     */
    def withType(attributeType: String,
                 diagnostics: Diagnostics = Diagnostics.discarding,
                 location: Option[SourceLocation] = None): AttributeBuilder =
      if attributeType.isBlank then
        diagnostics.warn(DiagnosticCode.BlankAttributeType,
          "No type given; the attribute becomes an object property with a domain but no range.",
          location)
        this.typeOfAttribute = None
      else
        this.typeOfAttribute = Some(UMLAttributeType(attributeType, prefixNamespace))
      this

    def withMultiplicity(min: String, max: String,
                         diagnostics: Diagnostics = Diagnostics.discarding,
                         location: Option[SourceLocation] = None): AttributeBuilder =
      val minCardinality = UMLCardinality(min, diagnostics, location)
      val maxCardinality = UMLCardinality(max, diagnostics, location)
      this.multiplicity = UMLMultiplicity(minCardinality, maxCardinality, diagnostics, location)
      this

    def withDefinition(definition: String): AttributeBuilder =
      this.definition = Some(definition)
      this

    def build: UMLAttribute =
      val umlAttribute = UMLAttribute(
        attributeIdentityBuilder.build,
        typeOfAttribute,
        multiplicity,
        definition.map(UMLAttributeDefinition.apply(_)))
      AttributeBuilderCache.cacheUMLAttribute(umlAttribute, this)
      umlAttribute
