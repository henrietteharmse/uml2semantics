package org.uml2owl

import com.typesafe.scalalogging.Logger

import java.io.File
sealed trait UmlClassDiagramElement
case class UmlIdentity (curie: String,
                        name: String
                       ):
  def identity: String =
    if this.curie.nonEmpty then
      this.curie
    else this.name

  def asIRI(prefix: String): String =
    prefix + '#' + identity

end UmlIdentity

object UmlIdentity:
  private val logger = Logger[UmlIdentity]
  private def isBothCurieAndNameEmpty(curie: Option[String], name: Option[String]): Boolean =
    curie.isEmpty && name.isEmpty ||
      curie.isDefined && curie.get.isEmpty && name.isEmpty ||
      name.isDefined && name.get.isEmpty && curie.isEmpty ||
      curie.isDefined && curie.get.isEmpty && name.isDefined && name.get.isEmpty

  def apply(curie: Option[String], name: Option[String]): UmlIdentity =
    logger.trace(s"isBothCurieAndNameEmpty(curie, name)=${isBothCurieAndNameEmpty(curie, name)}")
    if isBothCurieAndNameEmpty(curie, name) then
      logger.trace("Error: Both curie and name are empty.")
      throw new IllegalArgumentException("Both curie and name are empty.")
    else new UmlIdentity(curie.get, name.get)

case class UmlClass(id: UmlIdentity,
                    definition: Option[String],
                    parentIds: Set[String])
  extends UmlClassDiagramElement:
  def definitionIsNonEmpty: Boolean =
    this.definition.isDefined && this.definition.get.nonEmpty

case class UmlAttribute()
  extends UmlClassDiagramElement

case class UmlClassDiagram(owlOntologyFile: File, ontologyIRI: String, umlClasses: Map[String, UmlClass])
object UmlClassDiagram:
  def apply(owlOntologyFile: File) = new UmlClassDiagram(owlOntologyFile,"http://uml2owl.com", Map())