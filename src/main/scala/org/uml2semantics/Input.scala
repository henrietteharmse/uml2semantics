package org.uml2semantics

import com.typesafe.scalalogging.Logger
import org.uml2semantics.Precedence.TSV
import org.uml2semantics.inline.Code
import org.uml2semantics.model.PrefixNamespace

import java.io.File

case class InputParameters(classesTsv: Option[File] = None,
                           attributesTsv: Option[File] = None,
                           enumerationsTsv: Option[File] = None,
                           enumerationValuesTsv: Option[File] = None,
                           xmiFile: Option[File] = None,
                           precedence: String = "TSV",
                           owlOntologyFile: Option[File] = None,
                           ontologyIRI: String = "https://uml2semantics.com/ontology",
                           ontologyPrefix: String = "uml2ont:https://uml2semantics.com/ontology/",
                           prefixes: Seq[String] = PrefixNamespace.predefinedPrefixNamespacesAsStrings())

enum Precedence:
  case TSV
  case XMI

object Precedence:
  private val logger = Logger[this.type]

  def apply(precedence: String): Precedence =
    logger.debug(s"precedence=$precedence, ${Code.source}")
    precedence.toUpperCase match
      case "XMI" => XMI
      case _ => TSV

