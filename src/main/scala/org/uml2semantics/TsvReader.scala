package org.uml2semantics

import com.github.tototoshi.csv.*
import com.typesafe.scalalogging.Logger
import org.uml2semantics.model.{ClassCurie, ClassDefinition, ClassIRI, ClassId, ClassName, OntologyIRI, OntologyPrefix, ClassParentIds, UmlClass, UmlClassDiagram, ClassIdentity, UncertainClassId}

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.Set

enum ClassesHeader:
  case Curie, Name, Definition, ParentIds

enum AttributesHeader:
  case ClassId, Curie, Name, Type, PrimitiveOrClass, MinMultiplicity, MaxMultiplicity, Definition

def parseClasses(maybeTsvFile: Option[File], ontologyPrefix: OntologyPrefix): Map[ClassId, UmlClass] =
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

    val curieOption = m.get(Curie.toString)
    val nameOption = m.get(Name.toString)
    val umlClass = UmlClass(
      ClassIdentity(
        ClassCurie(curieOption.get),
        ClassName(nameOption.get),
        ontologyPrefix
        ),
      ClassDefinition(m.get(Definition.toString).get),
      ClassParentIds(parentIdsSet)
    )
    logger.trace(s"umlClasses.getClass.hasCode=${umlClasses.getClass.hashCode}")
    umlClasses += umlClass
  })
  reader.close()
  val umlClassesById = umlClasses.map(umlClass => (umlClass.classIdentity.classIRI.classId, umlClass)).toMap
  logger.trace(s"umlClassesById = $umlClassesById")
  umlClassesById
end parseClasses

//def parseAttributes(maybeTsvFile: Option[File]):Unit =
//  import AtrributesHeader.*
//
//  val logger = Logger("parseAttributes")
//
//  implicit object TsvFormat extends TSVFormat {}
//
//  val reader = CSVReader.open(maybeTsvFile.get)
//  val umlClasses = mutable.Set[UmlClass]()
//
//  reader.allWithHeaders().foreach(m => {
//    logger.trace(s"m = $m")
//
//    val parentIdsSet =
//      val maybeParentIds = m.get(ParentIds.toString)
//      logger.trace(s"maybeParentIds.get=${maybeParentIds.get}")
//      if maybeParentIds.isDefined then
//        maybeParentIds.get
//      else ""
//        .split('|').map(_.trim).toSet
//
//    val umlClass = UmlClass(
//      UmlIdentity(m.get(Curie.toString), m.get(Name.toString)),
//      m.get(Definition.toString),
//      parentIdsSet
//    )
//    logger.trace(s"umlClasses.getClass.hasCode=${umlClasses.getClass.hashCode}")
//    umlClasses += umlClass
//  })
//  reader.close()
//  val umlClassesById = umlClasses.map(umlClass => (umlClass.id.identity, umlClass)).toMap
//  logger.trace(s"umlClassesById = $umlClassesById")
//  umlClassesById
//  0
//end parseAttributes


def parseUMLClassDiagram(input: InputParameters): UmlClassDiagram =
  UmlClassDiagram(
    input.owlOntologyFile.get,
    OntologyIRI(input.ontologyIRI),
    OntologyPrefix(input.ontologyPrefix),
    parseClasses(input.classesTsv, OntologyPrefix(input.ontologyPrefix)))



