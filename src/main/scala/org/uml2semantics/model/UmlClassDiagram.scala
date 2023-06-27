package org.uml2semantics.model
import java.io.File
import com.typesafe.scalalogging.Logger
import org.uml2semantics.model.UmlCardinality.>=

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

sealed trait UmlClassId:
  def id: String
case class UmlClassName(name: String = "") extends UmlClassId:
  def nonEmpty: Boolean = name.nonEmpty
  override def id: String = name


/**
 *
 * @param shortName I.e., GO_0043226
 */
case class UmlClassShortName(shortName: String = "") extends UmlClassId:
  def nonEmpty: Boolean = shortName.nonEmpty
  override def id: String = shortName

/*
@Todo: Add support for Curies
*/
//case class ClassCurie(curie: String = "") /*extends ClassId*/:
//  def nonEmpty: Boolean = curie.nonEmpty
//  override def id: String = curie


sealed trait UmlClassAttributeId:
  def id: String
case class UmlClassAttributeName(classId: UmlClassId, name: String = "") extends UmlClassAttributeId:
  require(classId.id.nonEmpty && name.nonEmpty || name.isEmpty, "classId cannot be empty.")
  def nonEmpty: Boolean = classId.id.nonEmpty && name.nonEmpty
  override def id: String = classId.id +"#" + name

case class UmlClassAttributeShortName(classId: UmlClassId, shortName: String = "") extends UmlClassAttributeId:
  require(classId.id.nonEmpty && shortName.nonEmpty || shortName.isEmpty, "classId cannot be empty.")
  def nonEmpty: Boolean = classId.id.nonEmpty && shortName.nonEmpty
  override def id: String = classId.id +"#" + shortName

case class UmlClassAttributeIdentity(attributeShortName: UmlClassAttributeShortName,
                                     attributeName: UmlClassAttributeName,
                                     ontologyPrefix: OntologyPrefix):
  require(attributeShortName.nonEmpty || attributeName.nonEmpty, "An attribute must have a name or short name.")
  var tmpAttributeId: UmlClassAttributeId = _
  if attributeShortName.nonEmpty then
    tmpAttributeId = attributeShortName
  else if attributeName.nonEmpty then
    tmpAttributeId = attributeName
  val attributeIRI: UmlClassAttributeIRI = UmlClassAttributeIRI(ontologyPrefix, tmpAttributeId)

/*
@Todo: Add support for Curies
*/
case class UmlClassParentIds(setOfParentIds: Set[UmlClassId])
object UmlClassParentIds:
  private val logger = Logger[UmlClassParentIds]
  @targetName("fromSetOfStrings")
  def apply(setOfParentIds: Set[String]): UmlClassParentIds =
    logger.trace(s"setOfParentIds=$setOfParentIds")
    logger.trace(s"setOfParentIds.isEmpty=${setOfParentIds.isEmpty}")
    val setOfParentUncertainClassIds = setOfParentIds
      .filterNot(s => s.isEmpty)
      .map(m => UmlClassIdentity.findClassId(m).classId)
    new UmlClassParentIds(setOfParentUncertainClassIds)

case class UmlClasses(mapOfUmlClasses: Map[UmlClassId, UmlClass])

case class UmlClassAttributes(mapOfUmlClassAttributes: Map[UmlClassAttributeId, UmlClassAttribute])

case class UmlClassIRI(ontologyPrefix: OntologyPrefix, classId: UmlClassId):
  val iri: String = ontologyPrefix.ontologyPrefix + classId.id
//  override def toString: String = ontologyPrefix.ontologyPrefix + classId

case class UmlClassAttributeIRI(ontologyPrefix: OntologyPrefix, attributeId: UmlClassAttributeId):
  val iri: String = ontologyPrefix.ontologyPrefix + attributeId.id

case class OntologyIRI(ontologyIRI: String)
case class OntologyPrefix(ontologyPrefix: String):
  def +(classId: UmlClassId): UmlClassIRI =
    UmlClassIRI(OntologyPrefix(ontologyPrefix), classId)

case class UmlClassDefinition(definition: String = "")

opaque type UmlInfinite <: Char = '*'

object UmlInfinite:
  def apply(): UmlInfinite = '*'

  def unapply(s: String): Boolean = s == "*"

opaque type UmlNonNegativeInteger <: Int = Int

object UmlNonNegativeInteger:
  def apply(n: Int): UmlNonNegativeInteger =
    require(n >= 0)
    n

  def unapply(s: String): Boolean = s.toInt >= 0

sealed trait UmlCardinality
case class UmlInfiniteCardinality(infinite: UmlInfinite) extends UmlCardinality
case class UmlNonNegativeCardinality(nonNegativeInteger: UmlNonNegativeInteger) extends UmlCardinality
object UmlCardinality:
  def apply(s: String): UmlCardinality =
    s match
      case UmlNonNegativeInteger() => UmlNonNegativeCardinality(UmlNonNegativeInteger(s.toInt))
      case UmlInfinite() => UmlInfiniteCardinality(UmlInfinite())
      case _ => UmlNonNegativeCardinality(UmlNonNegativeInteger(1))

  def >=(c1: UmlCardinality, c2: UmlCardinality): Boolean = (c1, c2) match
    case (UmlInfiniteCardinality(t1), UmlInfiniteCardinality(t2)) => false
    case (UmlInfiniteCardinality(t1), UmlNonNegativeCardinality(t2)) => true
    case (UmlNonNegativeCardinality(t1), UmlInfiniteCardinality(t2)) => false
    case (UmlNonNegativeCardinality(t1), UmlNonNegativeCardinality(t2)) => t1 >= t2


case class UmlMultiplicity(min: UmlCardinality,
                           max: UmlCardinality)

object UmlMultiplicity:
  def apply (min: UmlCardinality, max: UmlCardinality): UmlMultiplicity =
    require(>=(max, min), "max cardinality must be greater or equal than min cardinality")
    new UmlMultiplicity(min, max)

case class UmlClassIdentity(classShortName: UmlClassShortName,
                            className: UmlClassName,
                            ontologyPrefix: OntologyPrefix):
  var classIRI: UmlClassIRI = _
  var classId: UmlClassId = _

object UmlClassIdentity:
  var classIdentityByShortName: mutable.Map[UmlClassShortName, UmlClassIdentity] = mutable.HashMap[UmlClassShortName, UmlClassIdentity]()
  var classIdentityByName: mutable.HashMap[UmlClassName, UmlClassIdentity] = mutable.HashMap[UmlClassName, UmlClassIdentity]()
  var classIdentityByIRI: mutable.HashMap[UmlClassIRI, UmlClassIdentity] = mutable.HashMap[UmlClassIRI, UmlClassIdentity]()
  private val logger = Logger[UmlClassIdentity]
  def apply(classShortName: UmlClassShortName = UmlClassShortName(),
            className: UmlClassName = UmlClassName(),
            ontologyPrefix: OntologyPrefix): UmlClassIdentity =
    require(classShortName.nonEmpty || className.nonEmpty, "A class must have either a shortname or a name.")
    val classIdentity: UmlClassIdentity = new UmlClassIdentity(classShortName, className, ontologyPrefix)
    if classShortName.nonEmpty then
      classIdentity.classId = classShortName
    else if className.nonEmpty then
      classIdentity.classId = className
    classIdentity.classIRI = UmlClassIRI(ontologyPrefix, classIdentity.classId)

    if classShortName.nonEmpty then
      classIdentityByShortName += (classShortName -> classIdentity)

    if className.nonEmpty then
      classIdentityByName += (className -> classIdentity)

    classIdentityByIRI += (classIdentity.classIRI -> classIdentity)
    classIdentity

  def findClassId(string: String): UmlClassIdentity =
    val classShortNameOption = classIdentityByShortName.get(UmlClassShortName(string))
    val classNameOption = classIdentityByName.get(UmlClassName(string))
    var classIdentity: UmlClassIdentity = null

    if classShortNameOption.isDefined then
      classIdentity = classShortNameOption.get
    else if classNameOption.isDefined then
      classIdentity = classNameOption.get
    else
      logger.debug(s"ClassId=$string could not be found!")
    classIdentity

end UmlClassIdentity


sealed trait UmlClassDiagramElement
case class UmlClass(classIdentity: UmlClassIdentity,
                    classDefinition: UmlClassDefinition = UmlClassDefinition(),
                    classParentIds: UmlClassParentIds = new UmlClassParentIds(Set[UmlClassId]()))
  extends UmlClassDiagramElement

// @todo Define a list of supported primitives
type UmlPrimitive = String
object UmlPrimitive:
  def apply(string: String): UmlPrimitive = string
  given CanEqual[UmlPrimitive, UmlPrimitive] = CanEqual.derived

case class UmlClassAttributeDefinition(definition: String = "")
enum UmlClassAttributeType:
  case Primitive(string: String)
  case ClassIdentity

case class UmlClassAttribute(attributeId: UmlClassAttributeIdentity,
                             typeOfAttribute: UmlPrimitive|UmlClassIdentity,
                             multiplicity: UmlMultiplicity,
                             definition: UmlClassAttributeDefinition = UmlClassAttributeDefinition())
  extends UmlClassDiagramElement

case class UmlClassDiagram(owlOntologyFile: File,
                           ontologyIRI: OntologyIRI,
                           ontologyPrefix: OntologyPrefix,
                           umlClasses: UmlClasses)
object UmlClassDiagram:
  def apply(owlOntologyFile: File, ontologyIRI: OntologyIRI, ontologyPrefix: OntologyPrefix) =
    new UmlClassDiagram(owlOntologyFile,ontologyIRI, ontologyPrefix, UmlClasses(Map()))
