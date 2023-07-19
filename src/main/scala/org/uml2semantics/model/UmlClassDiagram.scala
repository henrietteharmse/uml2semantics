package org.uml2semantics.model
import java.io.File
import com.typesafe.scalalogging.Logger
import org.uml2semantics.model.UmlCardinality.>=

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

sealed trait UmlClassId:
  def nonEmpty: Boolean
  def id: String
case class UmlClassName(name: String = "") extends UmlClassId:
  override def nonEmpty: Boolean = name.nonEmpty
  override def id: String = name


/**
 *
 * @param shortName I.e., GO_0043226. Short names are defined at https://www.w3.org/TR/rif-dtb/#def-shortname.
 *                  When using short names for defining UML classes etc, we will assume that IRI is formed by concatenating
 *                  ontology prefix and shortname.
 */
case class UmlClassShortName(shortName: String = "") extends UmlClassId:
  override def nonEmpty: Boolean = shortName.nonEmpty
  override def id: String = shortName

case class UmlClassCurie(curieOption: Option[Curie] = None) extends UmlClassId:
  override def nonEmpty: Boolean = curieOption.nonEmpty
  override def id: String = curieOption.get.curie

object UmlClassCurie:
  def apply(prefixName:PrefixName, prefixReference: PrefixReference): UmlClassCurie =
    new UmlClassCurie(Some(Curie(prefixName, prefixReference)))

private val FRAGMENT_SEPARATOR: String = "#"
sealed trait UmlClassAttributeId:
  def nonEmpty: Boolean
  def id(classId: UmlClassId): String
case class UmlClassAttributeName(name: String = "") extends UmlClassAttributeId:
  override def nonEmpty: Boolean = name.nonEmpty

  override def id(classId: UmlClassId): String = classId match
    case UmlClassCurie(curieOption) => curieOption.get.prefixReference.reference + FRAGMENT_SEPARATOR + name
    case _ => classId.id + FRAGMENT_SEPARATOR + name

case class UmlClassAttributeShortName(shortName: String = "") extends UmlClassAttributeId:
  override def nonEmpty: Boolean = shortName.nonEmpty
  override def id(classId: UmlClassId): String = classId match
    case UmlClassCurie(curieOption) => curieOption.get.prefixReference.reference + FRAGMENT_SEPARATOR + shortName
    case _ => classId.id + FRAGMENT_SEPARATOR + shortName

case class UmlClassAttributeCurie(curieOption: Option[Curie]) extends  UmlClassAttributeId:
  override def nonEmpty: Boolean = curieOption.nonEmpty

  override def id(classId: UmlClassId): String = classId match
    case UmlClassCurie(curieOption) => curieOption.get.prefixReference.reference + FRAGMENT_SEPARATOR +
      curieOption.get.prefixReference.reference
    case _ => classId.id + FRAGMENT_SEPARATOR + curieOption.get.prefixReference.reference

case class UmlClassAttributeIdentity(classId: UmlClassId,
                                     attributeShortName: UmlClassAttributeShortName = UmlClassAttributeShortName(),
                                     attributeName: UmlClassAttributeName = UmlClassAttributeName(),
                                     attributeCurie: UmlClassAttributeCurie = UmlClassAttributeCurie(None),
                                     ontologyPrefix: PrefixNamespace):
  var attributeIRI: UmlClassAttributeIRI = _
  var attributeId: UmlClassAttributeId = _
  var attributeLabel: String = _

object UmlClassAttributeIdentity:

  private val attrIdentityByShortName: mutable.Map[UmlClassAttributeShortName, UmlClassAttributeIdentity] =
    mutable.HashMap[UmlClassAttributeShortName, UmlClassAttributeIdentity]()
  private val attrIdentityByName: mutable.HashMap[UmlClassAttributeName, UmlClassAttributeIdentity] =
    mutable.HashMap[UmlClassAttributeName, UmlClassAttributeIdentity]()
  private val attrIdentityByIRI: mutable.HashMap[UmlClassAttributeIRI, UmlClassAttributeIdentity] =
    mutable.HashMap[UmlClassAttributeIRI, UmlClassAttributeIdentity]()
  private val attrIdentityByCurie: mutable.HashMap[UmlClassAttributeCurie, UmlClassAttributeIdentity] =
    mutable.HashMap[UmlClassAttributeCurie, UmlClassAttributeIdentity]()

  def apply(classId: UmlClassId,
            attributeShortName: UmlClassAttributeShortName = UmlClassAttributeShortName(),
            attributeName: UmlClassAttributeName = UmlClassAttributeName(),
            attributeCurie: UmlClassAttributeCurie = UmlClassAttributeCurie(None),
            ontologyPrefix: PrefixNamespace): UmlClassAttributeIdentity =
    require(classId.nonEmpty && (attributeShortName.nonEmpty || attributeName.nonEmpty || attributeCurie.nonEmpty ),
      "An attribute must have a classId and either a name or short name.")
    val classAttributeIdentity: UmlClassAttributeIdentity =
      new UmlClassAttributeIdentity(classId, attributeShortName, attributeName, attributeCurie, ontologyPrefix)

    (attributeShortName, attributeName, attributeCurie.curieOption) match
      case (UmlClassAttributeShortName(""), UmlClassAttributeName(""), Some(curie)) =>
        populate(classId, attributeCurie, ontologyPrefix, classAttributeIdentity, curie.prefixReference.reference)
        attrIdentityByCurie += (attributeCurie -> classAttributeIdentity)
      case (UmlClassAttributeShortName(""), UmlClassAttributeName(strAttributeName), Some(curie)) =>
        populate(classId, attributeCurie, ontologyPrefix, classAttributeIdentity, strAttributeName)
        attrIdentityByCurie += (attributeCurie -> classAttributeIdentity)
        attrIdentityByName += (attributeName -> classAttributeIdentity)
      case (UmlClassAttributeShortName(""), UmlClassAttributeName(strAttributeName), None) =>
        populate(classId, attributeName, ontologyPrefix, classAttributeIdentity, strAttributeName)
        attrIdentityByName += (attributeName -> classAttributeIdentity)
      case (UmlClassAttributeShortName(strAttributeShortName), UmlClassAttributeName(""), None) =>
        populate(classId, attributeShortName, ontologyPrefix, classAttributeIdentity, strAttributeShortName)
        attrIdentityByShortName += (attributeShortName -> classAttributeIdentity)
      case (UmlClassAttributeShortName(strAttributeShortName), UmlClassAttributeName(""), Some(curie)) =>
        populate(classId, attributeCurie, ontologyPrefix, classAttributeIdentity, strAttributeShortName)
        attrIdentityByCurie += (attributeCurie -> classAttributeIdentity)
        attrIdentityByShortName += (attributeShortName -> classAttributeIdentity)
      case (UmlClassAttributeShortName(strAttributeShortName), UmlClassAttributeName(strAttributeName), None) =>
        populate(classId, attributeShortName, ontologyPrefix, classAttributeIdentity, strAttributeName)
        attrIdentityByName += (attributeName -> classAttributeIdentity)
        attrIdentityByShortName += (attributeShortName -> classAttributeIdentity)
      case (UmlClassAttributeShortName(strAttributeShortName), UmlClassAttributeName(strAttributeName), Some(curie)) =>
        populate(classId, attributeCurie, ontologyPrefix, classAttributeIdentity, strAttributeName)
        attrIdentityByCurie += (attributeCurie -> classAttributeIdentity)
        attrIdentityByName += (attributeName -> classAttributeIdentity)
        attrIdentityByShortName += (attributeShortName -> classAttributeIdentity)

      attrIdentityByIRI += (classAttributeIdentity.attributeIRI -> classAttributeIdentity)

    classAttributeIdentity

  private def populate(classId: UmlClassId, attributeId: UmlClassAttributeId, ontologyPrefix: PrefixNamespace,
                       attributeIdentity: UmlClassAttributeIdentity,
                       label: String): Unit = {
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
  private val logger = Logger[UmlClassParentIds]
  @targetName("fromSetOfStrings")
  def apply(setOfParentIds: Set[String]): UmlClassParentIds =
    logger.trace(s"setOfParentIds=$setOfParentIds")
    logger.trace(s"setOfParentIds.isEmpty=${setOfParentIds.isEmpty}")
    val setOfParentUncertainClassIds = setOfParentIds
      .filterNot(s => s.isEmpty)
      .map(m => UmlClassIdentity.findClassId(m).get.classId)
    new UmlClassParentIds(setOfParentUncertainClassIds)

case class UmlClasses(mapOfUmlClasses: Map[UmlClassId, UmlClass])

case class UmlClassAttributes(mapOfUmlClassAttributes: Map[UmlClassAttributeId, UmlClassAttribute])

case class UmlClassIRI(iri: String)

object UmlClassIRI:
  private val logger = Logger[UmlClassIRI]
  def apply(ontologyPrefix: PrefixNamespace, classId: UmlClassId): UmlClassIRI = classId match
    case classIdType: UmlClassCurie =>
      val prefixNamespace = classIdType.curieOption.get
      new UmlClassIRI(PrefixNamespace.getPrefixNamespace(prefixNamespace.prefixName).get.prefixIRI.iri +
        classIdType.curieOption.get.prefixReference.reference)
    case _ => new UmlClassIRI(ontologyPrefix.prefixIRI.iri + classId.id)

case class UmlClassAttributeIRI(iri: String)

object UmlClassAttributeIRI:
  private val logger = Logger[UmlClassAttributeIRI]
  def apply(ontologyPrefix: PrefixNamespace, classId: UmlClassId, classAttributeId: UmlClassAttributeId): UmlClassAttributeIRI =
    logger.trace(s"ontologyPrefix.prefixIRI.iri = ${ontologyPrefix.prefixIRI.iri} and classAttributeId.id = ${classAttributeId.id}")
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
      if someInt >=0 then
        Some(UmlNonNegativeInteger(someInt))
      else
        None
    catch
      case e: Exception => None

sealed trait UmlCardinality
case class UmlInfiniteCardinality(infinite: UmlInfinite) extends UmlCardinality
case class UmlNonNegativeCardinality(nonNegativeInteger: UmlNonNegativeInteger) extends UmlCardinality
object UmlCardinality:
  def apply(s: String): UmlCardinality =
    s match
      case UmlNonNegativeInteger(i) => UmlNonNegativeCardinality(i)
      case UmlInfinite() => UmlInfiniteCardinality(UmlInfinite())
      case _ => UmlNonNegativeCardinality(UmlNonNegativeInteger(1))

  def >=(c1: UmlCardinality, c2: UmlCardinality): Boolean = (c1, c2) match
    case (UmlInfiniteCardinality(_), UmlInfiniteCardinality(_)) => false
    case (UmlInfiniteCardinality(_), UmlNonNegativeCardinality(_)) => true
    case (UmlNonNegativeCardinality(_), UmlInfiniteCardinality(_)) => false
    case (UmlNonNegativeCardinality(t1), UmlNonNegativeCardinality(t2)) => t1 >= t2

  def ==(c1: UmlCardinality, c2: UmlCardinality): Boolean = (c1, c2) match
    case (UmlInfiniteCardinality(_), UmlInfiniteCardinality(_)) => true
    case (UmlInfiniteCardinality(_), UmlNonNegativeCardinality(_)) => false
    case (UmlNonNegativeCardinality(_), UmlInfiniteCardinality(_)) => false
    case (UmlNonNegativeCardinality(t1), UmlNonNegativeCardinality(t2)) => t1 == t2

case class UmlMultiplicity(min: UmlCardinality,
                           max: UmlCardinality)

object UmlMultiplicity:
  def apply (min: UmlCardinality, max: UmlCardinality): UmlMultiplicity =
    require(>=(max, min),
      s"""Max cardinality must be greater or equal than min cardinality,
         but "min=$min" and "max=$max" found.""")
    new UmlMultiplicity(min, max)

case class UmlClassIdentity(classShortName: UmlClassShortName,
                            className: UmlClassName,
                            classCurie: UmlClassCurie,
                            ontologyPrefix: PrefixNamespace):
  var classIRI: UmlClassIRI = _
  var classId: UmlClassId = _
  var classLabel: String = _

object UmlClassIdentity:
  private val classIdentityByShortName: mutable.Map[UmlClassShortName, UmlClassIdentity] = mutable.HashMap[UmlClassShortName, UmlClassIdentity]()
  private val classIdentityByName: mutable.HashMap[UmlClassName, UmlClassIdentity] = mutable.HashMap[UmlClassName, UmlClassIdentity]()
  private val classIdentityByIRI: mutable.HashMap[UmlClassIRI, UmlClassIdentity] = mutable.HashMap[UmlClassIRI, UmlClassIdentity]()
  private val classIdentityByCurie: mutable.HashMap[UmlClassCurie, UmlClassIdentity] = mutable.HashMap[UmlClassCurie, UmlClassIdentity]()
  private val logger = Logger[UmlClassIdentity]

  /**
   *
   * @param classShortName
   * @param className
   * @param classCurie
   * @param ontologyPrefix
   * @return
   */
  def apply(classShortName: UmlClassShortName = UmlClassShortName(),
            className: UmlClassName = UmlClassName(),
            classCurie: UmlClassCurie = UmlClassCurie(),
            ontologyPrefix: PrefixNamespace): UmlClassIdentity =
    logger.trace(s"classShortName = $classShortName, className = $className and classCurie=$classCurie")
    require(classCurie.nonEmpty || classShortName.nonEmpty || className.nonEmpty, "A class must have either a curie, shortname or a name.")
    val classIdentity: UmlClassIdentity = new UmlClassIdentity(classShortName, className, classCurie, ontologyPrefix)
    (classShortName, className, classCurie.curieOption) match
      case (UmlClassShortName(""), UmlClassName(""), Some(curie)) =>
        populate(classCurie, ontologyPrefix, classIdentity, curie.prefixReference.reference )
        classIdentityByCurie += (classCurie -> classIdentity)
      case (UmlClassShortName(""), UmlClassName(strClassName), Some(curie)) =>
        populate(classCurie, ontologyPrefix, classIdentity, strClassName)
        classIdentityByCurie += (classCurie -> classIdentity)
        classIdentityByName += (className -> classIdentity)
      case (UmlClassShortName(""), UmlClassName(strClassName), None) =>
        populate(className, ontologyPrefix, classIdentity, strClassName)
        classIdentityByName += (className -> classIdentity)
      case (UmlClassShortName(strClassShortName), UmlClassName(""), None) =>
        populate(classShortName, ontologyPrefix, classIdentity, strClassShortName)
        classIdentityByShortName += (classShortName -> classIdentity)
      case (UmlClassShortName(strClassShortName), UmlClassName(""), Some(curie)) =>
        populate(classCurie, ontologyPrefix, classIdentity, strClassShortName)
        classIdentityByCurie += (classCurie -> classIdentity)
        classIdentityByShortName += (classShortName -> classIdentity)
      case (UmlClassShortName(strClassShortName), UmlClassName(strClassName), None) =>
        populate(classShortName, ontologyPrefix, classIdentity, strClassName)
        classIdentityByName += (className -> classIdentity)
        classIdentityByShortName += (classShortName -> classIdentity)
      case (UmlClassShortName(strClassShortName), UmlClassName(strClassName), Some(curie)) =>
        populate(classCurie, ontologyPrefix, classIdentity, strClassName)
        classIdentityByCurie += (classCurie -> classIdentity)
        classIdentityByName += (className -> classIdentity)
        classIdentityByShortName += (classShortName -> classIdentity)

      classIdentityByIRI += (classIdentity.classIRI -> classIdentity)

    classIdentity


  private def populate(classId: UmlClassId, ontologyPrefix: PrefixNamespace, classIdentity: UmlClassIdentity,
                       label: String): Unit = {
    val classIRI: UmlClassIRI = UmlClassIRI(ontologyPrefix, classId)
    classIdentity.classId = classId
    classIdentity.classIRI = classIRI
    classIdentity.classLabel = label
  }

  def findClassId(string: String): Option[UmlClassIdentity] =
    val classShortNameOption = classIdentityByShortName.get(UmlClassShortName(string))
    val classNameOption = classIdentityByName.get(UmlClassName(string))
    var classIdentityOption: Option[UmlClassIdentity] = None

    if classShortNameOption.isDefined then
      classIdentityOption = classShortNameOption
    else if classNameOption.isDefined then
      classIdentityOption = classNameOption
    else if Curie.isPossibleCurie(string) then
      classIdentityOption = classIdentityByCurie.get(UmlClassCurie(Some(Curie(string))))

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
  private val logger = Logger[UmlClassAttributeType]

  def apply(s: String): UmlClassAttributeType =
    require(UmlClassIdentity.findClassId(s).nonEmpty || XMLDataType.unapply(s).nonEmpty,
      s"""A class attribute must have a type that is either a class, that has been specified,
        or an XML data type. "$s" is not recognised as either a class or an XML data type.""")
    UmlClassIdentity.findClassId(s) match
      case Some(classId) => UmlClassIdentityType(classId)
      case None => UmlXMLDataType(XMLDataType.valueOf(s))

  def unapply(s: String): Option[UmlClassAttributeType] = s match
    case UmlClassIdentity(classShortName, className, classCurie, ontologyPrefix) => Some(UmlClassIdentityType(
      UmlClassIdentity(classShortName, className, classCurie, ontologyPrefix)))
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
  def apply(owlOntologyFile: File, ontologyIRI: OntologyIRI, ontologyPrefix: PrefixNamespace): UmlClassDiagram =
    new UmlClassDiagram(owlOntologyFile,ontologyIRI, ontologyPrefix, UmlClasses(Map()), UmlClassAttributes(Map()))
