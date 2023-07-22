package org.uml2semantics.model

import com.typesafe.scalalogging.Logger
import org.uml2semantics.inline.Code
import org.uml2semantics.model.UmlCardinality.{>=, logger}
import sourcecode.Text

import java.io.File
import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

sealed trait UmlClassId:
  def nonEmpty: Boolean

  def id: String

case class UmlClassName(name: String = "") extends UmlClassId:
  override def nonEmpty: Boolean = name.nonEmpty

  override def id: String = name

case class UmlClassCurie(curieOption: Option[Curie] = None) extends UmlClassId:
  override def nonEmpty: Boolean = curieOption.nonEmpty

  override def id: String = curieOption.get.curie

private val FRAGMENT_SEPARATOR: String = "/"

sealed trait UmlClassAttributeId:
  def nonEmpty: Boolean

  def id(classId: UmlClassId): String

case class UmlClassAttributeName(name: String = "") extends UmlClassAttributeId:
  private val logger = Logger[this.type]

  override def nonEmpty: Boolean = name.nonEmpty

  override def id(classId: UmlClassId): String =
    logger.debug(s"classId=$classId ${Code.source}")
    classId match
      case UmlClassCurie(curieOption) => curieOption.get.prefixReference.reference + FRAGMENT_SEPARATOR + name
      case _ => classId.id + FRAGMENT_SEPARATOR + name

case class UmlClassAttributeCurie(curieOption: Option[Curie]) extends UmlClassAttributeId:
  private val logger = Logger[this.type]

  override def nonEmpty: Boolean = curieOption.nonEmpty

  override def id(classId: UmlClassId): String =
    logger.debug(s"classId=$classId ${Code.source}")
    classId match
      case UmlClassCurie(curieOption) => curieOption.get.prefixReference.reference + FRAGMENT_SEPARATOR +
        curieOption.get.prefixReference.reference
      case _ => classId.id + FRAGMENT_SEPARATOR + curieOption.get.prefixReference.reference

case class UmlClassAttributeIdentity(classId: UmlClassId,
                                     attributeName: UmlClassAttributeName = UmlClassAttributeName(),
                                     attributeCurie: UmlClassAttributeCurie = UmlClassAttributeCurie(None),
                                     ontologyPrefix: PrefixNamespace):
  var attributeIRI: UmlClassAttributeIRI = _
  var attributeId: UmlClassAttributeId = _
  var attributeLabel: String = _

object UmlClassAttributeIdentity:
  private val logger = Logger[this.type]

  private val attrIdentityByName: mutable.HashMap[UmlClassAttributeName, UmlClassAttributeIdentity] =
    mutable.HashMap[UmlClassAttributeName, UmlClassAttributeIdentity]()
  private val attrIdentityByIRI: mutable.HashMap[UmlClassAttributeIRI, UmlClassAttributeIdentity] =
    mutable.HashMap[UmlClassAttributeIRI, UmlClassAttributeIdentity]()
  private val attrIdentityByCurie: mutable.HashMap[UmlClassAttributeCurie, UmlClassAttributeIdentity] =
    mutable.HashMap[UmlClassAttributeCurie, UmlClassAttributeIdentity]()

  def apply(classId: UmlClassId,
            attributeName: UmlClassAttributeName = UmlClassAttributeName(),
            attributeCurie: UmlClassAttributeCurie = UmlClassAttributeCurie(None),
            ontologyPrefix: PrefixNamespace): UmlClassAttributeIdentity =
    logger.debug(s"classId=$classId, attributeName=$attributeName, attributeCurie=$attributeCurie, " +
      s"ontologyPrefix=$ontologyPrefix ${Code.source}")
    //    logger.debug(s"${Code.arguments} ${Code.source}")
    require(classId.nonEmpty && (attributeName.nonEmpty || attributeCurie.nonEmpty),
      "An attribute must have a classId and either a name.")
    val classAttributeIdentity: UmlClassAttributeIdentity =
      new UmlClassAttributeIdentity(classId, attributeName, attributeCurie, ontologyPrefix)

    (attributeName, attributeCurie.curieOption) match
      case (UmlClassAttributeName(""), Some(curie)) =>
        logger.debug(s"case (UmlClassAttributeName(\"\"), Some(curie)) ${Code.source}")
        populate(classId, attributeCurie, ontologyPrefix, classAttributeIdentity, curie.prefixReference.reference)
        attrIdentityByCurie += (attributeCurie -> classAttributeIdentity)
      case (UmlClassAttributeName(strAttributeName), Some(curie)) =>
        logger.debug(s"case (UmlClassAttributeName(strAttributeName), Some(curie)) ${Code.source}")
        populate(classId, attributeCurie, ontologyPrefix, classAttributeIdentity, strAttributeName)
        attrIdentityByCurie += (attributeCurie -> classAttributeIdentity)
        attrIdentityByName += (attributeName -> classAttributeIdentity)
      case (UmlClassAttributeName(strAttributeName), None) =>
        logger.debug(s"case (UmlClassAttributeName(strAttributeName), None) ${Code.source}")
        populate(classId, attributeName, ontologyPrefix, classAttributeIdentity, strAttributeName)
        attrIdentityByName += (attributeName -> classAttributeIdentity)

    attrIdentityByIRI += (classAttributeIdentity.attributeIRI -> classAttributeIdentity)

    classAttributeIdentity

  private def populate(classId: UmlClassId, attributeId: UmlClassAttributeId, ontologyPrefix: PrefixNamespace,
                       attributeIdentity: UmlClassAttributeIdentity,
                       label: String): Unit = {
    logger.debug(s"classId=$classId, attributeId=$attributeId, ontologyPrefix=$ontologyPrefix",
      s"attributeIdentity=$attributeIdentity, label=$label ${Code.source}")
    //    logger.debug(s"${Code.arguments} ${Code.source}")
    val attributeIRI: UmlClassAttributeIRI = UmlClassAttributeIRI(ontologyPrefix, classId, attributeId)
    attributeIdentity.attributeId = attributeId
    attributeIdentity.attributeIRI = attributeIRI
    attributeIdentity.attributeLabel = label
  }


/*
@Todo: Add support for Curies
*/
case class UmlClassParentIds(setOfParentIds: Set[UmlClassId])

object UmlClassParentIds:
  private val logger = Logger[this.type]

  @targetName("fromSetOfStrings")
  def apply(setOfParentIds: Set[String]): UmlClassParentIds =
    logger.debug(s"setOfParentIds=$setOfParentIds Ids${Code.source}")
    logger.debug(s"setOfParentIds.isEmpty=${setOfParentIds.isEmpty}")
    val setOfParentUncertainClassIds = setOfParentIds
      .filterNot(s => s.isEmpty)
      .map(m => UmlClassIdentity.findClassId(m).get.classId)
    new UmlClassParentIds(setOfParentUncertainClassIds)

case class UmlClasses(mapOfUmlClasses: Map[UmlClassId, UmlClass])

case class UmlClassAttributes(mapOfUmlClassAttributes: Map[UmlClassAttributeId, UmlClassAttribute])

case class UmlClassIRI(iri: String)

object UmlClassIRI:
  private val logger = Logger[this.type]

  def apply(ontologyPrefix: PrefixNamespace, classId: UmlClassId): UmlClassIRI =
    logger.debug(s"ontologyPrefix=$ontologyPrefix, classId=$classId ${Code.source}")
    classId match
      case classIdType: UmlClassCurie =>
        val prefixNamespace = classIdType.curieOption.get
        new UmlClassIRI(PrefixNamespace.getPrefixNamespace(prefixNamespace.prefixName).get.prefixIRI.iri +
          classIdType.curieOption.get.prefixReference.reference)
      case _ => new UmlClassIRI(ontologyPrefix.prefixIRI.iri + classId.id)

case class UmlClassAttributeIRI(iri: String)

object UmlClassAttributeIRI:
  private val logger = Logger[this.type]

  def apply(ontologyPrefix: PrefixNamespace, classId: UmlClassId, classAttributeId: UmlClassAttributeId): UmlClassAttributeIRI =
    logger.debug(s"ontologyPrefix.prefixIRI.iri = ${ontologyPrefix.prefixIRI.iri}, " +
      s"classAttributeId.id = ${classAttributeId.id} ${Code.source}")
    classAttributeId match
      case classAttributeIdType: UmlClassAttributeCurie =>
        val prefixNamespace = classAttributeIdType.curieOption.get
        new UmlClassAttributeIRI(PrefixNamespace.getPrefixNamespace(prefixNamespace.prefixName).get.prefixIRI.iri +
          prefixNamespace.prefixReference.reference)
      case _ => new UmlClassAttributeIRI(ontologyPrefix.prefixIRI.iri + classAttributeId.id(classId))

case class OntologyIRI(ontologyIRI: String)

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

  def unapply(s: String): Option[UmlNonNegativeInteger] =
    try
      val someInt: Int = s.toInt
      if someInt >= 0 then
        Some(UmlNonNegativeInteger(someInt))
      else
        None
    catch
      case e: Exception => None

sealed trait UmlCardinality

case class UmlInfiniteCardinality(infinite: UmlInfinite) extends UmlCardinality

case class UmlNonNegativeCardinality(nonNegativeInteger: UmlNonNegativeInteger) extends UmlCardinality

object UmlCardinality:
  private val logger = Logger[this.type]

  def apply(s: String): UmlCardinality =
    logger.debug(s"s=$s ${Code.source}")
    s match
      case UmlNonNegativeInteger(i) => UmlNonNegativeCardinality(i)
      case UmlInfinite() => UmlInfiniteCardinality(UmlInfinite())
      case _ => UmlNonNegativeCardinality(UmlNonNegativeInteger(1))

  def >=(c1: UmlCardinality, c2: UmlCardinality): Boolean =
    logger.debug(s"c1=$c1, c2=$c2 ${Code.source}")
    (c1, c2) match
      case (UmlInfiniteCardinality(_), UmlInfiniteCardinality(_)) => false
      case (UmlInfiniteCardinality(_), UmlNonNegativeCardinality(_)) => true
      case (UmlNonNegativeCardinality(_), UmlInfiniteCardinality(_)) => false
      case (UmlNonNegativeCardinality(t1), UmlNonNegativeCardinality(t2)) => t1 >= t2

  def ==(c1: UmlCardinality, c2: UmlCardinality): Boolean =
    logger.debug(s"c1=$c1, c2=$c2 ${Code.source}")
    (c1, c2) match
      case (UmlInfiniteCardinality(_), UmlInfiniteCardinality(_)) => true
      case (UmlInfiniteCardinality(_), UmlNonNegativeCardinality(_)) => false
      case (UmlNonNegativeCardinality(_), UmlInfiniteCardinality(_)) => false
      case (UmlNonNegativeCardinality(t1), UmlNonNegativeCardinality(t2)) => t1 == t2

case class UmlMultiplicity(min: UmlCardinality = UmlCardinality("1"),
                           max: UmlCardinality = UmlCardinality("1"))

object UmlMultiplicity:
  private val logger = Logger[this.type]

  def apply(min: UmlCardinality, max: UmlCardinality): UmlMultiplicity =
    logger.debug(s"min=$min, max=$max ${Code.source}")
    require(>=(max, min),
      s"""Max cardinality must be greater or equal than min cardinality,
         but "min=$min" and "max=$max" found.""")
    new UmlMultiplicity(min, max)

case class UmlClassIdentity(className: UmlClassName,
                            classCurie: UmlClassCurie,
                            ontologyPrefix: PrefixNamespace):
  var classIRI: UmlClassIRI = _
  var classId: UmlClassId = _
  var classLabel: String = _

object UmlClassIdentity:
  private val logger = Logger[this.type]

  private val classIdentityByName: mutable.HashMap[UmlClassName, UmlClassIdentity] = mutable.HashMap[UmlClassName, UmlClassIdentity]()
  private val classIdentityByIRI: mutable.HashMap[UmlClassIRI, UmlClassIdentity] = mutable.HashMap[UmlClassIRI, UmlClassIdentity]()
  private val classIdentityByCurie: mutable.HashMap[UmlClassCurie, UmlClassIdentity] = mutable.HashMap[UmlClassCurie, UmlClassIdentity]()

  /**
   *
   * @param className
   * @param classCurie
   * @param ontologyPrefix
   * @return
   */
  def apply(className: UmlClassName = UmlClassName(),
            classCurie: UmlClassCurie = UmlClassCurie(),
            ontologyPrefix: PrefixNamespace): UmlClassIdentity =
    logger.debug(s"className = $className and classCurie=$classCurie ${Code.source}")
    require(classCurie.nonEmpty || className.nonEmpty, "A class must have either a curie or a name.")
    val classIdentity: UmlClassIdentity = new UmlClassIdentity(className, classCurie, ontologyPrefix)
    (className, classCurie.curieOption) match
      case (UmlClassName(""), Some(curie)) =>
        logger.debug(s"case (UmlClassName(\"\"), Some(curie)) ${Code.source}")
        populate(classCurie, ontologyPrefix, classIdentity, curie.prefixReference.reference)
        classIdentityByCurie += (classCurie -> classIdentity)
      case (UmlClassName(strClassName), Some(curie)) =>
        logger.debug(s"case (UmlClassName(strClassName), Some(curie)) ${Code.source}")
        populate(classCurie, ontologyPrefix, classIdentity, strClassName)
        classIdentityByCurie += (classCurie -> classIdentity)
        classIdentityByName += (className -> classIdentity)
      case (UmlClassName(strClassName), None) =>
        logger.debug(s"case (UmlClassName(strClassName), None) ${Code.source}")
        populate(className, ontologyPrefix, classIdentity, strClassName)
        classIdentityByName += (className -> classIdentity)

    classIdentityByIRI += (classIdentity.classIRI -> classIdentity)

    classIdentity


  private def populate(classId: UmlClassId, ontologyPrefix: PrefixNamespace, classIdentity: UmlClassIdentity,
                       label: String): Unit = {
    logger.debug(s"classId=$classId, ontologyPrefix=$ontologyPrefix, classIdentity=$classIdentity, label=$label, ${Code.source}")
    val classIRI: UmlClassIRI = UmlClassIRI(ontologyPrefix, classId)
    classIdentity.classId = classId
    classIdentity.classIRI = classIRI
    classIdentity.classLabel = label
  }

  def findClassId(s: String): Option[UmlClassIdentity] =
    logger.debug(s"s=$s ${Code.source}")
    val classNameOption = classIdentityByName.get(UmlClassName(s))
    var classIdentityOption: Option[UmlClassIdentity] = None

    if classNameOption.isDefined then
      logger.debug(s"classNameOption=${classNameOption.get} ${Code.source}")
      classIdentityOption = classNameOption
    else if Curie.isPossibleCurie(s) then
      logger.debug(s"s=$s ${Code.source}")
      classIdentityOption = classIdentityByCurie.get(UmlClassCurie(Some(Curie(s))))

    logger.debug(s"classIdentityOption=${classIdentityOption.getOrElse(None)} ${Code.source}")
    classIdentityOption

  def unapply(s: String): Option[UmlClassIdentity] = findClassId(s)

end UmlClassIdentity


sealed trait UmlClassDiagramElement

case class UmlClass(classIdentity: UmlClassIdentity,
                    classDefinition: UmlClassDefinition = UmlClassDefinition(),
                    classParentIds: UmlClassParentIds = new UmlClassParentIds(Set[UmlClassId]()))
  extends UmlClassDiagramElement


case class UmlClassAttributeDefinition(definition: String = "")

sealed trait UmlClassAttributeType

case class UmlXMLDataType(attributeType: XMLDataType) extends UmlClassAttributeType

case class UmlClassIdentityType(attributeType: UmlClassIdentity) extends UmlClassAttributeType

object UmlClassAttributeType:
  private val logger = Logger[this.type]

  def apply(s: String): UmlClassAttributeType =
    logger.debug(s"s=$s ${Code.source}")
    require(UmlClassIdentity.findClassId(s).nonEmpty || XMLDataType.unapply(s).nonEmpty,
      s"""A class attribute must have a type that is either a class, that has been specified,
        or an XML data type. "$s" is not recognised as either a class or an XML data type.""")
    UmlClassIdentity.findClassId(s) match
      case Some(classId) => UmlClassIdentityType(classId)
      case None => UmlXMLDataType(XMLDataType.valueOf(s))

  def unapply(s: String): Option[UmlClassAttributeType] =
    logger.debug(s"s=$s ${Code.source}")
    s match
      case UmlClassIdentity(className, classCurie, ontologyPrefix) => Some(UmlClassIdentityType(
        UmlClassIdentity(className, classCurie, ontologyPrefix)))
      case XMLDataType(i) => Some(UmlXMLDataType(i))
      case _ => None

case class UmlClassAttribute(attributeIdentity: UmlClassAttributeIdentity,
                             typeOfAttribute: UmlClassAttributeType,
                             multiplicity: UmlMultiplicity,
                             definition: UmlClassAttributeDefinition = UmlClassAttributeDefinition())
  extends UmlClassDiagramElement

case class UmlClassDiagram(owlOntologyFile: File,
                           ontologyIRI: OntologyIRI,
                           ontologyPrefix: PrefixNamespace,
                           umlClasses: UmlClasses,
                           umlClassAttributes: UmlClassAttributes)

object UmlClassDiagram:
  private val logger = Logger[this.type]

  def apply(owlOntologyFile: File, ontologyIRI: OntologyIRI, ontologyPrefix: PrefixNamespace): UmlClassDiagram =
    logger.debug(s"owlOntologyFile=$owlOntologyFile, ontologyIRI=$ontologyIRI, ontologyPrefix=$ontologyPrefix ${Code.source}")
    new UmlClassDiagram(owlOntologyFile, ontologyIRI, ontologyPrefix, UmlClasses(Map()), UmlClassAttributes(Map()))
