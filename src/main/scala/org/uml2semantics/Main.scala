package org.uml2semantics

import com.typesafe.scalalogging.Logger
import org.uml2semantics.diagnostics.DiagnosticSeverity
import org.uml2semantics.inline.Code
import org.uml2semantics.model.PrefixNamespace
import scopt.OParser

import java.io.File

val builder = OParser.builder[InputParameters]
val argParser =
  import builder.*
  OParser.sequence(
    programName("uml2semantics"),
    opt[Option[File]]('c', "classes")
      .valueName("<tsv-classes-file>")
      .action((a, c) => c.copy(classesTsv = a))
      .validate(o =>
        if (o.exists(f => f.exists()) || o.isEmpty) success
        else failure(s"The file \"${o.get}\" does not exist.")
      )
      .text("A TSV file containing UML class information"),
    opt[Option[File]]('a', "attributes")
      .valueName("<tsv-attributes-file>")
      .action((a, c) => c.copy(attributesTsv = a))
      .validate(o =>
        if (o.exists(f => f.exists()) || o.isEmpty) success
        else failure(s"The file \"${o.get}\" does not exist.")
      )
      .text("A TSV file containing UML class attribute information"),
    opt[Option[File]]('e', "enumerations")
      .valueName("<tsv-enumerations-file>")
      .action((a, c) => c.copy(enumerationsTsv = a))
      .validate(o =>
        if (o.exists(f => f.exists()) || o.isEmpty) success
        else failure(s"The file \"${o.get}\" does not exist.")
      )
      .text("A TSV file containing UML enumerations"),
    opt[Option[File]]('n', "enumeration-values")
      .valueName("<tsv-enumeration-values-file>")
      .action((a, c) => c.copy(enumerationValuesTsv = a))
      .validate(o =>
        if (o.exists(f => f.exists()) || o.isEmpty) success
        else failure(s"The file \"${o.get}\" does not exist.")
      )
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
    opt[Option[File]]('m', "xmi")
      .valueName("<xmi-file>")
      .action((a, c) => c.copy(xmiFile = a))
      .text("A file adhering to the XMI specification."),
    opt[String]('p', "ontologyPrefix").required()
      .withFallback(() => "uml2ont:https://uml2semantics.com/ontology/")
      .valueName("<ontology-prefix>")
      .action((a, c) => c.copy(ontologyPrefix = a))
      .text("The prefix to use with your ontology, e.g.: ex:https://example.com/ontology/"),
    opt[Seq[String]]('x', "prefixes")
      .withFallback(() => PrefixNamespace.predefinedPrefixNamespacesAsStrings())
      .valueName("<prefixname:prefix>,<prefixname:prefix>...")
      .action((a, c) => c.copy(prefixes = a))
      .text("A list of all the prefixes used in the UML class representation separated by commas."),
    opt[Unit]("no-global-disjointness")
      .action((_, c) => c.copy(globalDisjointness = false))
      .text("Disables the default behavior of making all classes that are not part of a generalization set " +
        "mutually disjoint. Use this when the model is not meant to assume that classes denote disjoint " +
        "sets at all; multiple inheritance alone does not need it, since classes that share a subclass " +
        "are left out of the global axiom anyway."),
    opt[String]("overrides")
      .valueName("<XMI> | <TSV>")
      .validate(o =>
        if (Set("XMI", "TSV").contains(o.toUpperCase)) success
        else failure(s"Invalid value '$o' for --overrides. Valid values are XMI and TSV.")
      )
      .action((a, c) => c.copy(overrides = a))
      .text("Indicates whether the XMI file should be used to override the TSV file or vice versa. " +
        "Use <XMI> to indicate that the XMI file should be used to override the TSV file " +
        "and <TSV> to indicate that the TSV file should be used to override the XMI file. " +
        "If not specified, the TSV file will be used to override the XMI file." +
        "This configuration only applies when both an XMI file and a TSV file are provided.")
  )


/**
 * Parses command line arguments and runs the conversion, returning its result. Returns None when
 * the arguments themselves could not be parsed, in which case scopt has already reported why.
 *
 * Both the CLI and the test suite enter here, so tests exercise argument handling rather than
 * assuming it.
 */
def convertFromArguments(arguments: Seq[String]): Option[ConversionResult] =
  OParser.parse(argParser, arguments, InputParameters()).map(convert)

@main def uml2owl(arguments: String*): Unit =
  val logger = Logger[this.type]
  convertFromArguments(arguments) match
    case Some(result) =>
      result.diagnostics.foreach(diagnostic =>
        if diagnostic.severity == DiagnosticSeverity.Error then logger.error(diagnostic.toString)
        else logger.warn(diagnostic.toString))
    case None => logger.error(s"Could not parse the provided arguments. ${Code.sourceDetail}")
  logger.info("Done")