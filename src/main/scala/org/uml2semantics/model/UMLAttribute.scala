package org.uml2semantics.model

import org.uml2semantics.model.cache.{AttributeBuilderCache, AttributeIdentityBuilderCache}
import com.typesafe.scalalogging.Logger
import org.uml2semantics.inline.Code
import org.uml2semantics.model.UMLCardinality.>=

sealed trait UMLAttributeIdentifier(classIdentifier: UMLClassIdentifier) extends UMLNamedElement

case class UMLAttributeName(classIdentifier: UMLClassIdentifier, name: String) extends UMLAttributeIdentifier(classIdentifier):
  def isEmpty: Boolean = name.isEmpty
  def nonEmpty: Boolean = name.nonEmpty
  def getName: String = classIdentifier.getName + FRAGMENT_SEPARATOR + name

object UMLAttributeName:
  def apply(classIdentifier: UMLClassIdentifier, name: String): Option[UMLAttributeName] =
    Option.when(classIdentifier.nonEmpty && !name.isBlank && !name.contains(':'))
      (new UMLAttributeName(classIdentifier, name))

case class UMLAttributeCurie(classIdentifier: UMLClassIdentifier, curie: Curie) extends UMLAttributeIdentifier(classIdentifier):
  def isEmpty: Boolean = curie.isEmpty
  def nonEmpty: Boolean = curie.nonEmpty
  def getName: String = classIdentifier.getName + FRAGMENT_SEPARATOR + curie

object UMLAttributeCurie:
  def apply(classIdentifier: UMLClassIdentifier, curie: String): Option[UMLAttributeCurie] =
    if classIdentifier.nonEmpty then
      Curie.fromString(curie).map(new UMLAttributeCurie(classIdentifier: UMLClassIdentifier, _))
    else None


case class UMLAttributeIdentity(classIdentity: UMLClassIdentity,
                                nameOption: Option[UMLAttributeName] = None,
                                curieOption: Option[UMLAttributeCurie] = None) extends UMLIdentity:
  def getIRI: String =
    (curieOption, nameOption) match
      case (Some(curie), _) => curie.curie.toIRI
      case (_, Some(name)) => val iri = classIdentity.ontologyPrefix.prefixIRI.iri
        iri + (if iri.matches(".*#$") then name.getName
          else if iri.matches(".*/$") then classIdentity.getLabel + FRAGMENT_SEPARATOR + name.getName
          else  PATH_SEPARATOR + classIdentity.getLabel + FRAGMENT_SEPARATOR + name.getName)
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")

  def getLabel: String =
    (curieOption, nameOption) match
      case (_, Some(name)) => name.getName
      case (Some(curie), _) => curie.getName
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")

object UMLAttributeIdentity:
  def apply(classIdentity: UMLClassIdentity, attributeIdentifier: String): UMLAttributeIdentity =
    (UMLAttributeCurie(classIdentity.getClassIdentifier, attributeIdentifier),
      UMLAttributeName(classIdentity.getClassIdentifier, attributeIdentifier)) match
      case (Some(curie), _) => UMLAttributeIdentity(classIdentity, None, Some(curie))
      case (_, Some(name)) => UMLAttributeIdentity(classIdentity, Some(name), None)
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")
      
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
        throw new IllegalArgumentException("Name and curie must not be empty.")

      val attributeNameOption: Option[UMLAttributeName] = nameOption.flatMap(UMLAttributeName(
        classIdentity.getClassIdentifier, _))
      val attributeCurieOption: Option[UMLAttributeCurie] = curieOption.flatMap(UMLAttributeCurie(
        classIdentity.getClassIdentifier, _))
      val attributeIdentity: UMLAttributeIdentity = UMLAttributeIdentity(classIdentity, attributeNameOption, attributeCurieOption)
      if attributeNameOption.isDefined && attributeCurieOption.isDefined then
        AttributeIdentityBuilderCache.cacheUMLAttributeIdentity(attributeIdentity,  this)
        return attributeIdentity

      val attributeIdentityBuilderOption =
        AttributeIdentityBuilderCache.getUMLAttributeIdentityBuilder(classIdentity, attributeIdentity)
      if attributeIdentityBuilderOption.isDefined then
        attributeIdentityBuilderOption.get.build
      else
        attributeIdentity

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

  def apply(s: String): UMLCardinality =
    logger.debug(s"s=$s ${Code.source}")
    s match
      case UMLNonNegativeInteger(i) => UMLNonNegativeCardinality(i)
      case UMLInfinite() => UMLInfiniteCardinality(UMLInfinite())
      case _ => UMLNonNegativeCardinality(UMLNonNegativeInteger(1))

  def >=(c1: UMLCardinality, c2: UMLCardinality): Boolean =
    logger.debug(s"c1=$c1, c2=$c2 ${Code.source}")
    (c1, c2) match
      case (UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) => false
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

case class UMLMultiplicity(min: UMLCardinality = UMLCardinality("1"),
                           max: UMLCardinality = UMLCardinality("1"))

object UMLMultiplicity:
  private val logger = Logger[this.type]

  def apply(): UMLMultiplicity =
    logger.debug(s"${Code.source}")
    new UMLMultiplicity()

  def apply(min: UMLCardinality, max: UMLCardinality): UMLMultiplicity =
    logger.debug(s"min=$min, max=$max ${Code.source}")
    require(>=(max, min),
      s"""Max cardinality must be greater or equal than min cardinality,
         but "min=$min" and "max=$max" found.""")
    new UMLMultiplicity(min, max)

sealed trait UMLClassAttributeType
case class UMLXMLDataType(attributeType: SupportedDataType) extends UMLClassAttributeType
case class UMLClassIdentityType(attributeType: UMLClassIdentity) extends UMLClassAttributeType
case class CurieBasedUMLClassAttributeType(attributeType: Curie) extends UMLClassAttributeType

case class UMLAttributeDefinition(definition: String = "")

case class UMLAttribute(attributeIdentity: UMLAttributeIdentity,
                        typeOfAttribute: Option[UMLClassAttributeType] = None,
                        multiplicity: UMLMultiplicity = UMLMultiplicity(),
                        definition: UMLAttributeDefinition = UMLAttributeDefinition()) extends UMLIdentity:

  def getIRI: String = attributeIdentity.getIRI

  def getLabel: String = attributeIdentity.getLabel

object UMLAttribute:
//  def builder (prefixNamespace: PrefixNamespace): AttributeBuilder = new AttributeBuilder(prefixNamespace)

  class AttributeBuilder(prefixNamespace: PrefixNamespace):
    private var attributeIdentityBuilder = UMLAttributeIdentity.AttributeIdentityBuilder(prefixNamespace)
    private var typeOfAttribute: Option[UMLClassAttributeType] = None
    private var multiplicity: UMLMultiplicity = UMLMultiplicity()
    private var definition: Option[String] = None

    def withClassName(name: String): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withName(name)
      this

    def withClassCurie(curie: String): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withCurie(curie)
      this

    def withName(name: String): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withName(name)
      this

    def withCurie(curieAsString: String): AttributeBuilder =
      this.attributeIdentityBuilder = attributeIdentityBuilder.withCurie(curieAsString)
      this

    def withTypeOfAttribute(attributeType: UMLClassAttributeType): AttributeBuilder =
      this.typeOfAttribute = Some(attributeType)
      this

    def withMultiplicity(min: UMLCardinality, max: UMLCardinality): AttributeBuilder =
      this.multiplicity = UMLMultiplicity(min, max)
      this

    def withDefinition(definition: String): AttributeBuilder =
      this.definition = Some(definition)
      this
      
    def build: UMLAttribute =
      val umlAttribute = UMLAttribute(
        attributeIdentityBuilder.build, 
        typeOfAttribute, 
        multiplicity,
        UMLAttributeDefinition(definition.getOrElse("")))
      AttributeBuilderCache.cacheUMLAttribute(umlAttribute, this)
      umlAttribute
