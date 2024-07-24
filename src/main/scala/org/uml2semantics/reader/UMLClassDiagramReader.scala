package org.uml2semantics.reader

import org.uml2semantics.InputParameters
import org.uml2semantics.model.UMLClassDiagram

trait UMLClassDiagramReader:
  def parseUMLClassDiagram(input: InputParameters): Option[UMLClassDiagram]