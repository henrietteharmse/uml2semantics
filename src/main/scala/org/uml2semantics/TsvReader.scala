package org.uml2semantics

import com.github.tototoshi.csv.*
import com.typesafe.scalalogging.Logger
import org.uml2semantics.model.*
import java.io.File
import scala.collection.mutable
import scala.collection.mutable.Set

enum ClassesHeader:
  case ShortName, Name, Definition, ParentIds

enum AttributesHeader:
  case ClassId, ShortName, Name, PrimitiveType, ClassType, MinMultiplicity, MaxMultiplicity, Definition

def parseClasses(maybeTsvFile: Option[File], ontologyPrefix: OntologyPrefix): UmlClasses =
  import ClassesHeader.*
  val logger = Logger("parseClasses")
  implicit object TsvFormat extends TSVFormat {}

  val reader = CSVReader.open(maybeTsvFile.get)
  val umlClasses = mutable.Set[UmlClass]()

  reader.allWithHeaders().foreach(m => {
    logger.trace(s"m = $m")

    val parentIdsSet =
      val maybeParentIds = m.get(ParentIds.toString)
      logger.trace(s"maybeParentIds.get=${maybeParentIds.get}")
      if maybeParentIds.isDefined then
        maybeParentIds.get
      else ""
    .split('|').map(_.trim).toSet

    val umlClass = UmlClass(
      UmlClassIdentity(
        UmlClassShortName(m(ShortName.toString)),
        UmlClassName(m(Name.toString)),
        ontologyPrefix
        ),
      UmlClassDefinition(m(Definition.toString)),
      UmlClassParentIds(parentIdsSet)
    )
    logger.trace(s"umlClasses.getClass.hasCode=${umlClasses.getClass.hashCode}")
    umlClasses += umlClass
  })
  reader.close()
  val umlClassesById = umlClasses.map(umlClass => (umlClass.classIdentity.classId, umlClass)).toMap
  logger.trace(s"umlClassesById = $umlClassesById")
  UmlClasses(umlClassesById)
end parseClasses


def parseAttributes(maybeTsvFile: Option[File], ontologyPrefix: OntologyPrefix): UmlClassAttributes =
  import AttributesHeader.*
  val logger = Logger("parseAttributes")
  implicit object TsvFormat extends TSVFormat {}

  val reader = CSVReader.open(maybeTsvFile.get)
  val umlClassAttributes = mutable.Set[UmlClassAttribute]()

  logger.trace("########## parseAttributes")
  reader.allWithHeaders().foreach(m => {
    logger.trace(s"m = $m")

    val classId = UmlClassIdentity.findClassId(m(AttributesHeader.ClassId.toString))
    logger.trace(s"classId = $classId")
    if classId.isDefined then
      val umlClassAttribute = UmlClassAttribute(
        UmlClassAttributeIdentity(classId.get.classId,
          UmlClassAttributeShortName(m(ShortName.toString)),
          UmlClassAttributeName(m(Name.toString)),
          ontologyPrefix
        ),
//        m.get(PrimitiveType.toString).get,
        UmlMultiplicity(UmlCardinality(m(MinMultiplicity.toString)), UmlCardinality(m(MaxMultiplicity.toString))),
        UmlClassAttributeDefinition(m(Definition.toString))
      )
      logger.trace(s"umlClassAttributes.getClass.hasCode=${umlClassAttributes.getClass.hashCode()}")
      umlClassAttributes += umlClassAttribute
  })
  reader.close()
  val umlClassAttributesById = umlClassAttributes.map(umlClassAttribute => (umlClassAttribute.attributeIdentity.attributeId, umlClassAttribute)).toMap
  logger.trace(s"umlClassAttributesById = $umlClassAttributesById")
  UmlClassAttributes(umlClassAttributesById)
end parseAttributes

def parseUMLClassDiagram(input: InputParameters): UmlClassDiagram =
  UmlClassDiagram(
    input.owlOntologyFile.get,
    OntologyIRI(input.ontologyIRI),
    OntologyPrefix(input.ontologyPrefix),
    parseClasses(input.classesTsv, OntologyPrefix(input.ontologyPrefix)),
    parseAttributes(input.attributesTsv, OntologyPrefix(input.ontologyPrefix)))



