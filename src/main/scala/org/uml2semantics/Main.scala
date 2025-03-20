package org.uml2semantics

import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.Overrides.XMI
import org.uml2semantics.inline.Code
import org.uml2semantics.model.PrefixNamespace
import org.uml2semantics.reader.{TSVReader, XMIReader}
//import org.uml2semantics.reader.{TSVReader, XMIReader}
import org.uml2semantics.writer.UML2OWLWriter
import scopt.OParser

// The cache import here is used for debugging purposes only
import org.uml2semantics.model.cache.{ClassIdentityBuilderCache, ClassBuilderCache}
import org.uml2semantics.model.cache.{AttributeIdentityBuilderCache, AttributeBuilderCache}

import java.io.File

val builder = OParser.builder[InputParameters]
val argParser =
  import builder.*
  OParser.sequence(
    programName("uml2semantics"),
    opt[Option[File]]('c', "classes")
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
      .validate(o =>
        if (o.exists(f => f.exists()) || o.isEmpty) success
        else failure(s"The file \"${o.get}\" does not exist.")
      )
      .text("A TSV file containing UML class attribute information"),
    opt[Option[File]]('e', "enumerations")
      .valueName("<csv-enumerations-file>")
      .action((a, c) => c.copy(enumerationsTsv = a))
      .validate(o =>
        if (o.exists(f => f.exists()) || o.isEmpty) success
        else failure(s"The file \"${o.get}\" does not exist.")
      )
      .text("A TSV file containing UML enumerations"),
    opt[Option[File]]('n', "enumeration-values")
      .valueName("<csv-enumeration-values-file>")
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
    opt[Option[File]]('m', "xmi file")
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
    opt[String]("overrides")
      .valueName("<XMI> | <TSV>")
      .action((a, c) => c.copy(overrides = a))
      .text("Indicates whether the XMI file should be used to override the TSV file or vice versa. " +
        "Use <XMI> to indicate that the XMI file should be used to override the TSV file " +
        "and <TSV> to indicate that the TSV file should be used to override the XMI file. " +
        "If not specified, the TSV file will be used to override the XMI file." +
        "This configuration only applies when both an XMI file and a TSV file are provided.")
  )


@main def uml2owl(arguments: String*): Unit =
  val logger = Logger[this.type]
  val options = OParser.parse(argParser, arguments, InputParameters())
  OParser.parse(argParser, arguments, InputParameters()) match
    case Some(input) =>
      logger.debug(s"Some input=$input at ${Code.source}")

      PrefixNamespace.cachePrefixes(input.prefixes)
      PrefixNamespace.cachePrefix(input.ontologyPrefix)

//      classesTsv: Option[File]
//      attributesTsv: Option[File]
//      enumerationsTsv: Option[File]
//      enumerationValuesTsv: Option[File]
//      xmiFile: Option[File]



//      TSVReader.parseUMLClassDiagram(input)

      XMIReader.parseUMLClassDiagram(input)
      val k = 0

      if input.owlOntologyFile.isDefined then
        val owlWriter = new UML2OWLWriter(input.ontologyIRI, 
          input.owlOntologyFile.get, 
          ClassBuilderCache.getClasses,
          AttributeBuilderCache.getAttributes)
        owlWriter.generateOWL
//      owlWriter.generateOWL match
//        case Left(exceptionMsg) => println(s"An exception occurred:$exceptionMsg")
//        case Right(warnings) =>
//          if warnings.nonEmpty then
//            logger.warn("During processing of the UMLClassdiagram the following potential problem were found ${Code.sourceDetail}:")
//            warnings.foreach(w => println(s"$w"))
    case _ => logger.error("Unexpected case ${Code.sourceDetail}")
  logger.info("Done")
