package org.uml2semantics

import com.typesafe.scalalogging.Logger
import org.uml2semantics.diagnostics.{Diagnostic, DiagnosticCode, DiagnosticSeverity, Diagnostics}
import org.uml2semantics.inline.Code
import org.uml2semantics.model.PrefixNamespace
import org.uml2semantics.model.cache.{AttributeBuilderCache, ClassBuilderCache}
import org.uml2semantics.reader.{TSVReader, XMIReader}
import org.uml2semantics.writer.UML2OWLWriter

import java.io.File

/**
 * The outcome of one conversion: the ontology file that was written, if it was, and everything
 * that could not be honoured along the way.
 */
case class ConversionResult(owlOntologyFile: Option[File], diagnostics: Seq[Diagnostic]):
  def errors: Seq[Diagnostic] = diagnostics.filter(_.severity == DiagnosticSeverity.Error)

  def warnings: Seq[Diagnostic] = diagnostics.filter(_.severity == DiagnosticSeverity.Warning)

  def hasErrors: Boolean = errors.nonEmpty

  def codes: Seq[DiagnosticCode] = diagnostics.map(_.code)

private val conversionLogger = Logger("Conversion")

/**
 * Runs the conversion described by `input`, returning what was produced and what went wrong.
 *
 * This is the seam the CLI and the test suite share: `uml2owl` is argument parsing and logging
 * on top of this function and nothing else. Diagnostics are returned rather than only logged,
 * because a caller that cannot see what was rejected cannot react to it — and neither can a test.
 */
def convert(input: InputParameters): ConversionResult =
  conversionLogger.debug(s"input=$input at ${Code.source}")
  val diagnostics = new Diagnostics

  PrefixNamespace.cachePrefixes(input.prefixes)
  PrefixNamespace.cachePrefix(input.ontologyPrefix)

  if input.enumerationsTsv.isDefined || input.enumerationValuesTsv.isDefined then
    diagnostics.warn(DiagnosticCode.EnumerationInputIgnored,
      "Enumeration TSV files (-e, -n) are not supported yet and were ignored.")

  val hasTsvInput = input.classesTsv.isDefined || input.attributesTsv.isDefined
  val hasXmiInput = input.xmiFile.isDefined

  // The overriding source is parsed last, so its values win during cache enrichment.
  (hasTsvInput, hasXmiInput) match
    case (true, false) =>
      conversionLogger.info("Processing TSV input files")
      diagnostics.addAll(TSVReader.parseUMLClassDiagram(input))

    case (false, true) =>
      conversionLogger.info("Processing XMI input file")
      diagnostics.addAll(XMIReader.parseUMLClassDiagram(input))

    case (true, true) =>
      Overrides(input.overrides) match
        case Overrides.XMI =>
          conversionLogger.info("Processing TSV input files first, then XMI will override")
          diagnostics.addAll(TSVReader.parseUMLClassDiagram(input))
          diagnostics.addAll(XMIReader.parseUMLClassDiagram(input))
        case Overrides.TSV =>
          conversionLogger.info("Processing XMI input file first, then TSV will override")
          diagnostics.addAll(XMIReader.parseUMLClassDiagram(input))
          diagnostics.addAll(TSVReader.parseUMLClassDiagram(input))

    case (false, false) =>
      diagnostics.error(DiagnosticCode.NoInputSupplied,
        "No input files specified. Provide either TSV files (-c, -a) or an XMI file (-m).")

  val writtenFile = input.owlOntologyFile.flatMap { owlOntologyFile =>
    val owlWriter = new UML2OWLWriter(input.ontologyIRI,
      owlOntologyFile,
      ClassBuilderCache.getClasses,
      AttributeBuilderCache.getAttributes,
      input.globalDisjointness)
    val writerDiagnostics = owlWriter.generateOWL
    diagnostics.addAll(writerDiagnostics)
    Option.when(!writerDiagnostics.exists(_.code == DiagnosticCode.OntologySaveFailed))(owlOntologyFile)
  }

  ConversionResult(writtenFile, diagnostics.toSeq)
