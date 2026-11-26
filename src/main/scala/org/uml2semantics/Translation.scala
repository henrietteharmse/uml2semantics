package org.uml2semantics

import com.typesafe.scalalogging.Logger
import org.uml2semantics.diagnostics.{Diagnostic, DiagnosticCode, DiagnosticSeverity, Diagnostics}
import org.uml2semantics.inline.Code
import org.uml2semantics.model.PrefixNamespace
import org.uml2semantics.model.cache.{AttributeCache, ClassCache, EnumerationCache}
import org.uml2semantics.reader.{ReaderHelper, TSVReader, XMIReader}
import org.uml2semantics.writer.UML2OWLWriter

import java.io.File

/**
 * The outcome of one translation: the ontology file that was written, if it was, and everything
 * that could not be honoured along the way.
 */
case class TranslationResult(owlOntologyFile: Option[File], diagnostics: Seq[Diagnostic]):
  def errors: Seq[Diagnostic] = diagnostics.filter(_.severity == DiagnosticSeverity.Error)

  def warnings: Seq[Diagnostic] = diagnostics.filter(_.severity == DiagnosticSeverity.Warning)

  def hasErrors: Boolean = errors.nonEmpty

  def codes: Seq[DiagnosticCode] = diagnostics.map(_.code)

private val translationLogger = Logger("Translation")

/**
 * Runs the translation described by `input`, returning what was produced and what went wrong.
 *
 * This is the seam the CLI and the test suite share: `uml2owl` is argument parsing and logging
 * on top of this function and nothing else. Diagnostics are returned rather than only logged,
 * because a caller that cannot see what was rejected cannot react to it — and neither can a test.
 */
def translate(input: InputParameters): TranslationResult =
  translationLogger.debug(s"input=$input at ${Code.source}")
  val diagnostics = new Diagnostics

  PrefixNamespace.cachePrefixes(input.prefixes)
  PrefixNamespace.cachePrefix(input.ontologyPrefix)

  val hasTsvInput = input.classesTsv.isDefined || input.attributesTsv.isDefined ||
    input.enumerationsTsv.isDefined || input.enumerationValuesTsv.isDefined
  val hasXmiInput = input.xmiFile.isDefined

  // The overriding source is parsed last, so its values win during cache enrichment.
  (hasTsvInput, hasXmiInput) match
    case (true, false) =>
      translationLogger.info("Processing TSV input files")
      diagnostics.addAll(TSVReader.parseUMLClassDiagram(input))

    case (false, true) =>
      translationLogger.info("Processing XMI input file")
      diagnostics.addAll(XMIReader.parseUMLClassDiagram(input))

    case (true, true) =>
      Overrides(input.overrides) match
        case Overrides.XMI =>
          translationLogger.info("Processing TSV input files first, then XMI will override")
          diagnostics.addAll(TSVReader.parseUMLClassDiagram(input))
          diagnostics.addAll(XMIReader.parseUMLClassDiagram(input))
        case Overrides.TSV =>
          translationLogger.info("Processing XMI input file first, then TSV will override")
          diagnostics.addAll(XMIReader.parseUMLClassDiagram(input))
          diagnostics.addAll(TSVReader.parseUMLClassDiagram(input))

    case (false, false) =>
      diagnostics.error(DiagnosticCode.NoInputSupplied,
        "No input files specified. Provide either TSV files (-c, -a, -e, -n) or an XMI file (-m).")

  // Judged here and not by a reader, because one source may declare an enumeration and the other
  // give it its values.
  ReaderHelper.reportEnumerationsWithoutValues(diagnostics)

  val writtenFile = input.owlOntologyFile.flatMap { owlOntologyFile =>
    val owlWriter = new UML2OWLWriter(input.ontologyIRI,
      owlOntologyFile,
      ClassCache.getClasses,
      AttributeCache.getAttributes,
      EnumerationCache.getEnumerations,
      input.globalDisjointness)
    val writerDiagnostics = owlWriter.generateOWL
    diagnostics.addAll(writerDiagnostics)
    Option.when(!writerDiagnostics.exists(_.code == DiagnosticCode.OntologySaveFailed))(owlOntologyFile)
  }

  TranslationResult(writtenFile, diagnostics.toSeq)
