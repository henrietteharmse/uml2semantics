package org.uml2owl

import scopt.OParser

import java.io.File

case class InputParameters(classesTsv: Option[File] = None,
                           attributesTsv: Option[File] = None,
                           associationsTsv: Option[File] = None,
                           associationsByRoleTsv: Option[File] = None,
                           enumerationsTsv: Option[File] = None,
                           owlOntologyFile: Option[File] = None,
                           ontologyIRI: String = "https://uml2owl.com/ontology",
                           ontologyPrefix: String = "ex:https://owl2uml.com/ontology#")



val builder = OParser.builder[InputParameters]
val argParser =
  import builder._
  OParser.sequence(
    programName("uml2owl"),
//    head("uml2owl","v0.0.1"),
    opt[Option[File]]('c', "classes")
        .required()
        .valueName("<csv-classes-file>")
        .action((a, c) => c.copy(classesTsv = a))
        .validate(o =>
            if (o.exists(f => f.exists()) || o.isEmpty) success
            else failure (s"The file \"${o.get}\" does not exist.")
        )
        .text("A TSV file containing UML class information"),
    opt[Option[File]]('t', "attributes")
        .valueName("<csv-attributes-file>")
        .action((a, c) => c.copy(associationsTsv = a))
        .text("A TSV file containing UML class attribute information"),
    opt[Option[File]]('o', "ontology")
      .required()
      .valueName("<owl-ontology-file>")
      .action((a, c) => c.copy(owlOntologyFile = a))
      .text("The OWL ontology file that will be written out."),
    opt[String]('i', "ontologyIRI")
      .valueName("<ontology-iri>")
      .action((a, c) => c.copy(ontologyIRI = a))
      .text("The IRI of the ontology, e.g.: https://example.com/ontology"),
    opt[String]('p', "ontologyPrefix")
      .valueName("<ontology-prefix>")
      .action((a, c) => c.copy(ontologyPrefix = a))
      .text("The prefix to use with your ontology, e.g.: ex:https://example.com/ontology#")
  )


@main def uml2owl (arguments: String*): Unit =
  OParser.parse(argParser, arguments, InputParameters()) match
    case Some(input) =>
//      println("Process classes: input = " + _)
      val umlClassDiagram = parseUMLClassDiagram(input)
      val owlWriter = new OWLWriter(umlClassDiagram)
      owlWriter.generateOWL match
        case Left(exceptionMsg) => println(s"An exception occurred:$exceptionMsg")
        case Right(warnings) =>
          if warnings.nonEmpty then
            println("During processing of the UMLClassdiagram the following potential problem were found:")
            warnings.foreach(w => println(s"$w"))
    case _ =>
//      UmlClassDiagram()
  println("Done")
