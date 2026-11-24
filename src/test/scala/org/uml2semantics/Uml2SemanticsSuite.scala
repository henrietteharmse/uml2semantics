package org.uml2semantics

import munit.FunSuite
import org.semanticweb.owlapi.model.OWLOntology
import org.uml2semantics.diagnostics.{Diagnostic, DiagnosticCode, DiagnosticSeverity}
import org.uml2semantics.model.PrefixNamespace
import org.uml2semantics.model.cache.{AttributeCache, AttributeIdentityCache, ClassBuilderCache, ClassIdentityBuilderCache, EnumerationCache}
import org.uml2semantics.utility.OntologyAssertions

import java.io.File

/**
 * Base suite for everything that drives a translation.
 *
 * It owns two things that every suite needs and no suite should repeat: resetting the global model
 * caches between tests, and the convention that binds a *case* on disk to a test. Centralising the
 * reset means a cache added to the model later is reset everywhere at once, rather than being
 * forgotten in whichever suites were written before it existed.
 */
abstract class Uml2SemanticsSuite extends FunSuite:

  /** Every case is translated under the same ontology IRI and prefix, so goldens are comparable. */
  protected val ontologyIRI = "https://uml2semantics.com/test/ontology"
  protected val ontologyPrefixIRI = "https://uml2semantics.com/test/ontology/"
  protected val ontologyPrefix = s"t:$ontologyPrefixIRI"

  private val casesRoot = new File("src/test/resources/cases")
  private val outputRoot = new File("test-output/cases")

  override def beforeEach(context: BeforeEach): Unit =
    ClassIdentityBuilderCache.clear()
    ClassBuilderCache.clear()
    AttributeIdentityCache.clear()
    AttributeCache.clear()
    EnumerationCache.clear()
    PrefixNamespace.clear()

  /**
   * Runs one case and asserts its ontology matches its golden.
   *
   * `caseName` is a path under `src/test/resources/cases`, e.g. `tsv/multiplicity/exact-one`.
   * Whichever of `<case>.classes.tsv`, `<case>.attributes.tsv`, `<case>.enumerations.tsv` and
   * `<case>.enumeration-values.tsv` exist are passed as input; the golden is `<case>.ofn`. The
   * translation goes through argument parsing, so cases exercise the CLI surface rather than
   * assuming it.
   *
   * Returns the result so a case can go on to assert the diagnostics it expects.
   */
  protected def runCase(caseName: String,
                        extraArguments: Seq[String] = Seq.empty,
                        prefix: String = ontologyPrefix): TranslationResult =
    val casePath = new File(casesRoot, caseName)
    val caseDirectory = casePath.getParentFile
    val filePrefix = casePath.getName

    val classesTsv = new File(caseDirectory, s"$filePrefix.classes.tsv")
    val attributesTsv = new File(caseDirectory, s"$filePrefix.attributes.tsv")
    val enumerationsTsv = new File(caseDirectory, s"$filePrefix.enumerations.tsv")
    val enumerationValuesTsv = new File(caseDirectory, s"$filePrefix.enumeration-values.tsv")
    val golden = new File(caseDirectory, s"$filePrefix.ofn")

    val inputFiles = Seq("-c" -> classesTsv, "-a" -> attributesTsv,
      "-e" -> enumerationsTsv, "-n" -> enumerationValuesTsv).filter((_, file) => file.exists())

    assert(inputFiles.nonEmpty, s"Case '$caseName' has no input file; expected at least one of " +
      s"$classesTsv, $attributesTsv, $enumerationsTsv or $enumerationValuesTsv")
    assert(golden.exists(), s"Case '$caseName' has no golden; expected $golden")

    val outputFile = new File(outputRoot, s"$caseName.rdf")
    outputFile.getParentFile.mkdirs()

    val arguments =
      Seq("-i", ontologyIRI, "-p", prefix, "-o", outputFile.getPath) ++
        inputFiles.flatMap((flag, file) => Seq(flag, file.getPath)) ++
        extraArguments

    val result = translateFromArguments(arguments).getOrElse(
      fail(s"Case '$caseName': arguments could not be parsed: ${arguments.mkString(" ")}"))

    assert(result.owlOntologyFile.isDefined, s"Case '$caseName' wrote no ontology: ${result.errors.mkString("\n")}")

    OntologyAssertions.assertOntologiesEqual(
      OntologyAssertions.loadOntology(outputFile),
      OntologyAssertions.loadOntology(golden))

    result

  /**
   * Asserts the exact set of diagnostics a case produced, as severity/code pairs. Exact rather than
   * containment, so a case notices diagnostics nobody asked for. Messages are never matched, so
   * their wording stays free to improve.
   */
  protected def assertDiagnostics(result: TranslationResult,
                                  expected: (DiagnosticSeverity, DiagnosticCode)*): Unit =
    val actual = result.diagnostics.map(diagnostic => (diagnostic.severity, diagnostic.code))
    assertEquals(actual.sortBy(_.toString()), expected.toSeq.sortBy(_.toString()),
      s"Diagnostics did not match. Actual diagnostics were:\n${result.diagnostics.mkString("\n")}")

  /** Asserts a case produced a clean run. */
  protected def assertNoDiagnostics(result: TranslationResult): Unit =
    assertDiagnostics(result)
