package org.uml2semantics.model

import com.typesafe.scalalogging.Logger
import org.uml2semantics.inline.Code
import sourcecode.Text

import java.io.File
import scala.:+
import scala.annotation.{tailrec, targetName}
import scala.collection.mutable


case class OntologyIRI(ontologyIRI: String)
