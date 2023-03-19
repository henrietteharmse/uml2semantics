package org.uml2owl

trait UMLClassDiagramElement
case class UMLClass(curie: String, name: String, definition: String, parentCuries: String*) extends UMLClassDiagramElement

