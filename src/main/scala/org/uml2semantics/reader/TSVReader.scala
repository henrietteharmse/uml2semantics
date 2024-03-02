package org.uml2semantics.reader

import com.github.tototoshi.csv.*
import com.typesafe.scalalogging.Logger
import org.uml2semantics.*
import org.uml2semantics.model.*

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.Set

enum ClassesHeader:
  case Name, Curie, Definition, ParentNames

enum ClassAttributesHeader:
  case Class, Curie, Name, ClassEnumOrPrimitiveType, MinMultiplicity, MaxMultiplicity, Definition

enum EnumerationsHeader:
  case Name, Curie, Definition

enum EnumerationValuesHeader:
  case Enumeration, Name, Curie, Definition

def parseClasses(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace): UMLClasses =
  import ClassesHeader.*
  val logger = Logger("parseClasses")
  logger.info("Start")
  implicit object TsvFormat extends TSVFormat {}

  val reader = CSVReader.open(maybeTsvFile.get)
  var umlClasses = mutable.Set[UMLClass]()

  reader.allWithHeaders().foreach(m => {
    logger.trace(s"m = $m")

    val parentNamedElementsSet =
      val maybeParentNames = m.get(ParentNames.toString)
      logger.trace(s"maybeParentNames.get=${maybeParentNames.get}")
      if maybeParentNames.isDefined then
        maybeParentNames.get
      else ""
    .split('|').map(_.trim).toSet

    val curieOption: Option[Curie] = if m(Curie.toString).contains(":") then
      Some(org.uml2semantics.model.Curie(m(Curie.toString)))
    else
      None

    val umlClass = UMLClass(
      UMLClassIdentity(
        UMLClassName(m(Name.toString)),
        UMLClassCurie(curieOption),
        ontologyPrefix
        ),
      UMLClassDefinition(m(Definition.toString)),
      UMLClassParentNamedElements(parentNamedElementsSet)
    )
    logger.trace(s"umlClasses.getClass.hasCode=${umlClasses.getClass.hashCode}")
    umlClasses += umlClass
  })
  reader.close()
  val umlClassesByNamedElement = umlClasses.map(umlClass => (umlClass.classIdentity.classNamedElement, umlClass)).toMap
  logger.trace(s"umlClassesByNamedElement = $umlClassesByNamedElement")
  logger.info("Done")
  UMLClasses(umlClassesByNamedElement)
end parseClasses


def parseAttributes(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace): UMLClassAttributes =
  import ClassAttributesHeader.*
  val logger = Logger("TsvReader: parseAttributes")
  implicit object TsvFormat extends TSVFormat {}
  var umlClassAttributes = mutable.Set[UMLClassAttribute]()

  if maybeTsvFile.isDefined then
    val reader = CSVReader.open(maybeTsvFile.get)
    logger.info("Start")
    reader.allWithHeaders().foreach(m => {
      logger.trace(s"m = $m")

      val classNamedElement = UMLClassIdentity.findClassNamedElement(m(ClassAttributesHeader.Class.toString))
      val enumerationNamedElement = UMLEnumerationIdentity.findEnumerationNamedElement(m(ClassAttributesHeader.Class.toString))
      logger.trace(s"classNamedElement = $classNamedElement")
      if classNamedElement.isDefined || enumerationNamedElement.isDefined then
        logger.trace(s"mClassOrPrimitiveType.toString = {${m(ClassEnumOrPrimitiveType.toString)}}")
        val curieOption: Option[Curie] = if m(Curie.toString).contains(":") then
          Some(org.uml2semantics.model.Curie(m(Curie.toString)))
        else
          None
        val umlClassAttribute = UMLClassAttribute(
          UMLClassAttributeIdentity(classNamedElement.get.classNamedElement,
            UMLClassAttributeName(m(Name.toString)),
            UMLClassAttributeCurie(curieOption),
            ontologyPrefix
          ),
          UMLClassAttributeType(m(ClassEnumOrPrimitiveType.toString)),
          UMLMultiplicity(UMLCardinality(m(MinMultiplicity.toString)), UMLCardinality(m(MaxMultiplicity.toString))),
          UMLClassAttributeDefinition(m(Definition.toString))
        )
        umlClassAttributes += umlClassAttribute
    })
    reader.close()

  val umlClassAttributesById = umlClassAttributes.map(umlClassAttribute => (umlClassAttribute.attributeIdentity.attributeNamedElement, umlClassAttribute)).toMap
  logger.trace(s"umlClassAttributesById = $umlClassAttributesById")
  logger.info("Done")
  UMLClassAttributes(umlClassAttributesById)
end parseAttributes

def parseEnumerations(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace): UMLEnumerations =
  import EnumerationsHeader.*
  val logger = Logger("parseEnumerations")
  logger.info("Start")
  implicit object TsvFormat extends TSVFormat {}

  var umlEnumerations = mutable.Set[UMLEnumeration]()
  if maybeTsvFile.isDefined then
    val reader = CSVReader.open(maybeTsvFile.get)


    reader.allWithHeaders().foreach(m => {
      logger.trace(s"m = $m")

      val curieOption: Option[Curie] = if m(Curie.toString).contains(":") then
        Some(org.uml2semantics.model.Curie(m(Curie.toString)))
      else
        None

      val umlEnumeration = UMLEnumeration(
        UMLEnumerationIdentity(
          UMLEnumerationName(m(Name.toString)),
          UMLEnumerationCurie(curieOption),
          ontologyPrefix
        ),
        UMLEnumerationDefinition(m(Definition.toString))
      )
      logger.trace(s"umlEnumeration.getClass.hasCode=${umlEnumeration.getClass.hashCode}")
      umlEnumerations += umlEnumeration
    })
    reader.close()

  val umlEnumerationByNamedElement = umlEnumerations.map(
    umlEnumeration => (umlEnumeration.enumerationIdentity.enumerationNamedElement, umlEnumeration)).toMap
  logger.trace(s"umlEnumerationByNamedElement = $umlEnumerationByNamedElement")
  logger.info("Done")
  UMLEnumerations(umlEnumerationByNamedElement)
end parseEnumerations

def parseEnumerationValues(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace): UMLEnumerationValues =
  import EnumerationValuesHeader.*
  val logger = Logger("parseEnumerationValues")
  implicit object TsvFormat extends TSVFormat {}

  if maybeTsvFile.isDefined then {
    val reader = CSVReader.open(maybeTsvFile.get)
    var umlEnumerationValues = mutable.Set[UMLEnumerationValue]()

    logger.info("Start")
    reader.allWithHeaders().foreach(m => {
      logger.trace(s"m = $m")

      val enumerationIdentityOption = UMLEnumerationIdentity.findEnumerationNamedElement(
        m(EnumerationValuesHeader.Enumeration.toString))
      logger.trace(s"--------------- enumerationIdentityOption = $enumerationIdentityOption")
      if enumerationIdentityOption.isDefined then
        val enumerationIdentity = enumerationIdentityOption.get
        val curieOption: Option[Curie] = if m(Curie.toString).contains(":") then
          Some(org.uml2semantics.model.Curie(m(Curie.toString)))
        else
          None
        val enumerationValueIdentity = UMLEnumerationValueIdentity(enumerationIdentity.enumerationNamedElement,
          UMLEnumerationValueName(m(Name.toString)),
          UMLEnumerationValueCurie(curieOption),
          ontologyPrefix
        )
        logger.trace(s"enumerationValueIdentity = $enumerationValueIdentity")
        val umlEnumerationValue = UMLEnumerationValue(enumerationValueIdentity,
          UMLEnumerationValueDefinition(m(Definition.toString))
        )
        logger.trace(s"About to cache enumerationIdentity=$enumerationIdentity and enumerationValueIdentity=$enumerationValueIdentity")
        UMLEnumeration.cache(enumerationIdentity, enumerationValueIdentity)
        umlEnumerationValues += umlEnumerationValue
    })
    reader.close()
    val umlEnumerationValuesById = umlEnumerationValues.map(umlEnumerationValue => (umlEnumerationValue.valueIdentity.valueNamedElement, umlEnumerationValue)).toMap
    logger.trace(s"umlEnumerationValuesById = $umlEnumerationValuesById")
    logger.info("Done")
    UMLEnumerationValues(umlEnumerationValuesById)
  }
  else
    UMLEnumerationValues(scala.collection.immutable.HashMap[UMLEnumerationValueNamedElement, UMLEnumerationValue]())
end parseEnumerationValues

def parseUMLClassDiagram(input: InputParameters): Option[UMLClassDiagram] =
  val umlClasses = parseClasses(input.classesTsv, PrefixNamespace(input.ontologyPrefix))
  if umlClasses.umlClasses.isEmpty then
    return None
  val umlEnumerations = parseEnumerations(input.enumerationsTsv, PrefixNamespace(input.ontologyPrefix))
  val umlAttributes = parseAttributes(input.attributesTsv, PrefixNamespace(input.ontologyPrefix))
  val umlEnumerationValues = parseEnumerationValues(input.enumerationValuesTsv, PrefixNamespace(input.ontologyPrefix))

  Some(UMLClassDiagram(
    input.owlOntologyFile.get,
    OntologyIRI(input.ontologyIRI),
    PrefixNamespace(input.ontologyPrefix),
    umlClasses,
    umlAttributes,
    umlEnumerations,
    umlEnumerationValues))



