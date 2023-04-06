package org.uml2owl

sealed trait UmlClassDiagramElement

case class UmlClass(curie: String,
                    name: Option[String],
                    definition: Option[String],
                    parentIds: Set[String])
  extends UmlClassDiagramElement

case class UmlAttribute()
  extends UmlClassDiagramElement

case class UmlClassDiagram(umlClasses: Map[String, UmlClass])
object UmlClassDiagram:
  def apply() = new UmlClassDiagram(Map())