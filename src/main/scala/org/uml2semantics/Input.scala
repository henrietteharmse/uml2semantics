package org.uml2semantics

import org.uml2semantics.model.PrefixNamespace

import java.io.File

case class InputParameters(classesTsv: Option[File] = None,
                           attributesTsv: Option[File] = None,
                           enumerationsTsv: Option[File] = None,
                           enumerationValuesTsv: Option[File] = None,
                           xmiFile: Option[File] = None,
                           owlOntologyFile: Option[File] = None,
                           ontologyIRI: String = "https://uml2semantics.com/ontology",
                           ontologyPrefix: String = "uml2ont:https://uml2semantics.com/ontology/",
                           prefixes: Seq[String] = PrefixNamespace.predefinedPrefixNamespacesAsStrings())

