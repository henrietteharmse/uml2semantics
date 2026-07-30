package org.uml2semantics.reader

import org.uml2semantics.InputParameters
import org.uml2semantics.diagnostics.Diagnostic

trait UMLClassDiagramReader:
  /**
   * Parses one input format, populating the model caches as a side effect, and returns
   * everything that could not be honoured. A reader never aborts on a bad row: it records a
   * diagnostic, skips the row and carries on, so one malformed row cannot cost the modeller
   * the rest of the file.
   */
  def parseUMLClassDiagram(input: InputParameters): Seq[Diagnostic]
