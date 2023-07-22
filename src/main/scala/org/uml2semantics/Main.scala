package org.uml2semantics

import com.typesafe.scalalogging.Logger
import org.uml2semantics.inline.Code
import org.uml2semantics.model.{OntologyIRI, PrefixNamespace}
import org.uml2semantics.owl.UmlToOWLWriter
import scopt.OParser

import java.io.File

case class InputParameters(classesTsv: Option[File] = None,
                           attributesTsv: Option[File] = None,
                           associationsTsv: Option[File] = None,
                           associationsByRoleTsv: Option[File] = None,
                           enumerationsTsv: Option[File] = None,
                           owlOntologyFile: Option[File] = None,
                           ontologyIRI: String = "https://uml2semantics.com/ontology",
                           ontologyPrefix: String = "uml2ont:https://uml2semantics.com/ontology/",
                           prefixes: Seq[String] = PrefixNamespace.predefinedPrefixNamespacesAsStrings())


val builder = OParser.builder[InputParameters]
val argParser =
  import builder.*
  OParser.sequence(
    programName("uml2semantics"),
    //    head("uml2owl","v0.0.1"),
    opt[Option[File]]('c', "classes")
      .required()
      .valueName("<csv-classes-file>")
      .action((a, c) => c.copy(classesTsv = a))
      .validate(o =>
        if (o.exists(f => f.exists()) || o.isEmpty) success
        else failure(s"The file \"${o.get}\" does not exist.")
      )
      .text("A TSV file containing UML class information"),
    opt[Option[File]]('a', "attributes")
      .valueName("<csv-attributes-file>")
      .action((a, c) => c.copy(attributesTsv = a))
      .text("A TSV file containing UML class attribute information"),
    opt[Option[File]]('o', "ontology")
      .required()
      .valueName("<owl-ontology-file>")
      .action((a, c) => c.copy(owlOntologyFile = a))
      .text("The OWL ontology file that will be written out."),
    opt[String]('i', "ontologyIRI")
      .required()
      .withFallback(() => "https://uml2semantics.com/ontology")
      .valueName("<ontology-iri>")
      .action((a, c) => c.copy(ontologyIRI = a))
      .text("The IRI of the ontology, e.g.: https://example.com/ontology"),
    opt[String]('p', "ontologyPrefix").required()
      .withFallback(() => "uml2ont:https://uml2semantics.com/ontology/")
      .valueName("<ontology-prefix>")
      .action((a, c) => c.copy(ontologyPrefix = a))
      .text("The prefix to use with your ontology, e.g.: ex:https://example.com/ontology/"),
    opt[Seq[String]]('x', "prefixes")
      .withFallback(() => PrefixNamespace.predefinedPrefixNamespacesAsStrings())
      .valueName("<prefixname:prefix>,<prefixname:prefix>...")
      .action((a, c) => c.copy(prefixes = a))
      .text("A list of all the prefixes used in the UML class representation")
  )


@main def uml2owl(arguments: String*): Unit =
  val logger = Logger[this.type]
  logger.info("Start")
  logger.debug(s"arguments = $arguments ${Code.source}")
  OParser.parse(argParser, arguments, InputParameters()) match
    case Some(input) =>
      PrefixNamespace.cachePrefixes(input.prefixes)
      PrefixNamespace.cachePrefix(input.ontologyPrefix)
      val umlClassDiagram = parseUMLClassDiagram(input)
      val owlWriter = new UmlToOWLWriter(umlClassDiagram)
      owlWriter.generateOWL match
        case Left(exceptionMsg) => println(s"An exception occurred:$exceptionMsg")
        case Right(warnings) =>
          if warnings.nonEmpty then
            logger.warn("During processing of the UMLClassdiagram the following potential problem were found ${Code.sourceDetail}:")
            warnings.foreach(w => println(s"$w"))
    case _ => logger.error("Unexpected case ${Code.sourceDetail}")
  logger.info("Done")
