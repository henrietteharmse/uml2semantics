package org.uml2semantics.writer

import com.typesafe.scalalogging.Logger
import org.uml2semantics.model.{UMLAttribute, UMLClass}

import java.io.File

class UML2SHACLWriter(shapeGraphIRI: String,
                      shapeGraphFile: File,
                      classes: Set[UMLClass],
                      attributes: Set[UMLAttribute]):

  private val logger = Logger("UML2SHACLWriter")
