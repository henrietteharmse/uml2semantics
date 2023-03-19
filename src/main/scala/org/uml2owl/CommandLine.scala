package org.uml2owl

import scopt.OParser

case class InputParameters(classesCsv: String = "",
                           attributesCsv: String = "",
                           associationsCsv: String = "",
                           associationsByRoleCsv: String = "",
                           enumerationsCsv: String = "")



val builder = OParser.builder[InputParameters]
val argParser =
  import builder._
  OParser.sequence(
    programName("uml2owl"),
    head("uml2owl","v0.0.1"),
    opt[String]('c', "classes")
        .required()
        .valueName("<csv_file>")
        .action((clz, c) => c.copy(classesCsv = clz))
        .text("A CSV file containing UML class information"),
    opt[String]('t', "attributes")
        .valueName("<csv_file>")
        .action((t, c) => c.copy(associationsCsv = t))
        .text("A CSV file containing UML class attribute information"),
  )

@main def uml2owl (arguments: String*): Unit =
  for argument <- arguments do
    println(argument)
  OParser.parse(argParser, arguments, InputParameters()) match
    case Some(input) =>
      println("Process classes")
    case _ =>
      println(OParser.usage(argParser))

