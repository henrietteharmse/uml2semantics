package org.uml2semantics.reader

import org.uml2semantics.InputParameters

trait UMLClassDiagramReader:
  def parseUMLClassDiagram(input: InputParameters): Unit