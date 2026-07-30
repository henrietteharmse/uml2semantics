package org.uml2semantics.diagnostics

import scala.collection.mutable

/**
 * Where in an input document a diagnostic originated. Row numbers are file line numbers,
 * so the header of a TSV file is row 1 and its first data row is row 2 — the numbering a
 * modeller sees in an editor.
 */
case class SourceLocation(file: String, row: Option[Int] = None):
  override def toString: String = row match
    case Some(rowNumber) => s"$file row $rowNumber"
    case None => file

enum DiagnosticSeverity:
  /** The input was salvaged, but a default was substituted or something was ignored. */
  case Warning
  /** Something the modeller wrote could not be honoured and was dropped. */
  case Error

/**
 * The stable identity of a diagnostic. Callers — including tests — match on the code,
 * never on the message, so messages stay free to improve.
 */
enum DiagnosticCode:
  /** A cardinality cell could not be parsed and defaulted to 1. */
  case UnparsableCardinality
  /** Max cardinality is below min, so the multiplicity is unsatisfiable. */
  case MaxBelowMin
  /** Neither a name nor a curie was supplied. */
  case BlankIdentity
  /** An attribute was given no type at all. */
  case BlankAttributeType
  /** A name contained a colon, which is reserved for curies. */
  case NameContainsColon
  /** An identifier reads as a curie, but its prefix is not configured via -p or -x. */
  case UnresolvableCurie
  /** A multiplicity of the form [*, n] — an unbounded minimum is meaningless. */
  case UnboundedMinMultiplicity
  /** Enumeration TSV input was supplied, but enumeration parsing is not implemented. */
  case EnumerationInputIgnored
  /** No input file was supplied at all. */
  case NoInputSupplied
  /** An axiom could not be added to the ontology. */
  case AxiomAdditionFailed
  /** The ontology could not be written to its output file. */
  case OntologySaveFailed
  /** A row failed for a reason with no more specific code. */
  case RowRejected

case class Diagnostic(severity: DiagnosticSeverity,
                      code: DiagnosticCode,
                      message: String,
                      location: Option[SourceLocation] = None):
  override def toString: String =
    val where = location.map(location => s" [$location]").getOrElse("")
    s"$severity $code$where: $message"

/**
 * Accumulates the diagnostics produced by one reader or writer run. Readers and the writer
 * return their diagnostics rather than only logging them, so that a caller — the CLI, or a
 * test — can react to what went wrong. This is a deliberate exception to the rule that
 * readers communicate solely through the model caches: an error channel that lives only in
 * the log cannot be asserted on.
 */
final class Diagnostics:
  private val entries = mutable.ArrayBuffer[Diagnostic]()

  def add(diagnostic: Diagnostic): Unit = entries += diagnostic

  def addAll(diagnostics: IterableOnce[Diagnostic]): Unit = entries ++= diagnostics

  def warn(code: DiagnosticCode, message: String, location: Option[SourceLocation] = None): Unit =
    add(Diagnostic(DiagnosticSeverity.Warning, code, message, location))

  def error(code: DiagnosticCode, message: String, location: Option[SourceLocation] = None): Unit =
    add(Diagnostic(DiagnosticSeverity.Error, code, message, location))

  def toSeq: Seq[Diagnostic] = entries.toSeq

object Diagnostics:
  /** A sink that throws its diagnostics away, for call sites with nothing to report to. */
  def discarding: Diagnostics = new Diagnostics

/**
 * Thrown by model builders when input cannot be turned into a model object. Carries the
 * diagnostic code so that the reader catching it can report precisely what was wrong,
 * instead of degrading every failure to a generic rejected row.
 */
final class UMLModelException(val code: DiagnosticCode, message: String) extends RuntimeException(message)
