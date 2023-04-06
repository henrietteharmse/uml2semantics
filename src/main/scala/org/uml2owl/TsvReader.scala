package org.uml2owl

import java.io.File
import com.github.tototoshi.csv.*

import scala.collection.mutable
import scala.collection.mutable.Set

enum ClassesHeader:
  def name: String = toString
  case Curie, Name, Definition, ParentIds
end ClassesHeader

def parseClasses(maybeTsvFile: Option[File]): Map[String, UmlClass] =
  import org.uml2owl.ClassesHeader.*
  new TSVFormat {}

  val reader = CSVReader.open(maybeTsvFile.get)
  var umlClasses = mutable.Set[UmlClass]()

  reader.allWithHeaders().foreach(m => {
    val errorMsg = "Error: ClassId cannot be empty"
    val classId = if !m.contains(Curie.name) then errorMsg else
      if m(Curie.name).isEmpty then errorMsg else {
        m(Curie.name)
      }
    val parentIdsSet =
      val maybeParentIds = m.get(ParentIds.name)
      if maybeParentIds.isDefined
        then maybeParentIds.get
        else ""
    .split('|').map(_.trim).toSet

    val umlClass = UmlClass(
      classId,
      m.get(Name.name),
      m.get(Definition.name),
      parentIdsSet
    )
    println(s"umlClasses=${umlClasses.getClass}")
    umlClasses += umlClass
  })
  reader.close()
  val umlClassesById = umlClasses.map(umlClass => (umlClass.curie, umlClass)).toMap
  println(s"umlClassesById type = ${umlClassesById.getClass}")
  umlClassesById

def parseUMLClassDiagram(input: InputParameters): UmlClassDiagram =
  UmlClassDiagram(parseClasses(input.classesTsv))



