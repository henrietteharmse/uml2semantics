package org.uml2owl

import java.io.File
import com.github.tototoshi.csv.*

import scala.collection.mutable
import scala.collection.mutable.Set
import com.typesafe.scalalogging.Logger

enum ClassesHeader:
  def name: String = toString
  case Curie, Name, Definition, ParentIds
end ClassesHeader

def parseClasses(maybeTsvFile: Option[File]): Map[String, UmlClass] =
  import org.uml2owl.ClassesHeader.*
  val logger = Logger("parseClasses")
  implicit object TsvFormat extends TSVFormat {}

  val reader = CSVReader.open(maybeTsvFile.get)
  val umlClasses = mutable.Set[UmlClass]()

  reader.allWithHeaders().foreach(m => {
    logger.trace(s"m = $m")

    val parentIdsSet =
      val maybeParentIds = m.get(ParentIds.name)
      logger.trace(s"maybeParentIds.get=${maybeParentIds.get}")
      if maybeParentIds.isDefined then
        maybeParentIds.get
      else ""
    .split('|').map(_.trim).toSet

    val umlClass = UmlClass(
      UmlIdentity(m.get(Curie.name), m.get(Name.name)),
      m.get(Definition.name),
      parentIdsSet
    )
    logger.trace(s"umlClasses.getClass.hasCode=${umlClasses.getClass.hashCode}")
    umlClasses += umlClass
  })
  reader.close()
  val umlClassesById = umlClasses.map(umlClass => (umlClass.id.identity, umlClass)).toMap
  logger.trace(s"umlClassesById = ${umlClassesById}")
  umlClassesById
end parseClasses

def parseUMLClassDiagram(input: InputParameters): UmlClassDiagram =
  UmlClassDiagram(input.owlOntologyFile.get, input.ontologyIRI, parseClasses(input.classesTsv))



