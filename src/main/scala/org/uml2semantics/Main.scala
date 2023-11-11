package org.uml2semantics

import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.inline.Code
import org.uml2semantics.model.{OntologyIRI, PrefixNamespace}
import org.uml2semantics.reader.parseUMLClassDiagram
import org.uml2semantics.owl.UML2OWLWriter
import scopt.OParser

import java.io.File


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
    opt[Option[File]]('e', "enumerations")
      .valueName("<csv-enumerations-file>")
      .action((a, c) => c.copy(enumerationsTsv = a))
      .validate(o =>
        if (o.exists(f => f.exists()) || o.isEmpty) success
        else failure(s"The file \"${o.get}\" does not exist.")
      )
      .text("A TSV file containing UML enumerations"),
    opt[Option[File]]('n', "enumeration values")
      .valueName("<csv-enumeration-values-file>")
      .action((a, c) => c.copy(enumerationsValuesTsv = a))
      .text("A TSV file containing UML enumeration values"),
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
      .text("A list of all the prefixes used in the UML class representation separated by commas.")
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
      val owlWriter = new UML2OWLWriter(umlClassDiagram)
      owlWriter.generateOWL match
        case Left(exceptionMsg) => println(s"An exception occurred:$exceptionMsg")
        case Right(warnings) =>
          if warnings.nonEmpty then
            logger.warn("During processing of the UMLClassdiagram the following potential problem were found ${Code.sourceDetail}:")
            warnings.foreach(w => println(s"$w"))
    case _ => logger.error("Unexpected case ${Code.sourceDetail}")
  logger.info("Done")
