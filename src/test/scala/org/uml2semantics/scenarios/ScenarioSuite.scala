package org.uml2semantics.scenarios

import org.uml2semantics.utility.OntologyAssertions
import org.uml2semantics.{Uml2SemanticsSuite, translateFromArguments}

import java.io.File

/**
 * Checks every expected ontology of every scenario against what the tool writes today.
 *
 * A scenario is a directory under `src/test/resources/scenarios`, holding one model in up to two
 * encodings:
 *
 *  - Each `expected/<model>.owl` is the translation of the Enterprise Architect export
 *    `input/xmi/sparx/<model>.xml`, and gets one test.
 *  - `expected/tsv/<scenario>.owl` is the translation of the TSV files under `input/tsv`, and gets
 *    one test in every scenario that has that directory.
 *
 * The tests are found on disk and not listed here, so an expected ontology cannot be added without
 * being checked, and TSV files cannot be added without an expected ontology.
 *
 * A scenario shows that the behaviours the cases isolate compose in a realistic model. When one of
 * these tests fails after a deliberate change to the translation, the expected file is written
 * again by the tool, with the arguments used here, and the difference is reviewed.
 */
class ScenarioSuite extends Uml2SemanticsSuite:

  /** What the XMI expected ontologies were written with. They use the default ontology prefix. */
  private val xmiArguments = Seq("-i", "https://uml2semantics.com/examples/employer")

  /** What the TSV expected ontologies were written with. The TSV files use the `emp` prefix in curies. */
  private val tsvArguments = Seq(
    "-i", "http://uml2semantics.org/examples/employer/v.0.1",
    "-p", "emp:http://uml2semantics.org/examples/employer#")

  /** The TSV files of a scenario, by the argument each is passed with. */
  private val tsvFiles = Seq("-c" -> "Classes.tsv", "-a" -> "Attributes.tsv",
    "-e" -> "Enumerations.tsv", "-n" -> "EnumerationNamedValues.tsv")

  private val scenariosRoot = new File("src/test/resources/scenarios")
  private val outputRoot = new File("test-output/scenarios")

  private def filesIn(directory: File): Seq[File] =
    Option(directory.listFiles()).map(_.toSeq).getOrElse(Seq.empty).sortBy(_.getName)

  private val scenarios: Seq[File] = filesIn(scenariosRoot).filter(_.isDirectory)

  private val xmiExpectedOntologies: Seq[(File, File)] =
    for
      scenario <- scenarios
      expected <- filesIn(new File(scenario, "expected")) if expected.getName.endsWith(".owl")
    yield (scenario, expected)

  private val scenariosWithTsv: Seq[File] = scenarios.filter(new File(_, "input/tsv").isDirectory)

  /** Translates `inputArguments` to `outputFile` and asserts a clean run that matches `expected`. */
  private def assertTranslatesTo(expected: File, outputFile: File, inputArguments: Seq[String]): Unit =
    outputFile.getParentFile.mkdirs()

    val arguments = inputArguments ++ Seq("-o", outputFile.getPath)
    val result = translateFromArguments(arguments).getOrElse(
      fail(s"Arguments could not be parsed: ${arguments.mkString(" ")}"))

    assert(result.owlOntologyFile.isDefined, s"No ontology was written: ${result.errors.mkString("\n")}")
    assertNoDiagnostics(result)

    OntologyAssertions.assertOntologiesEqual(
      OntologyAssertions.loadOntology(outputFile),
      OntologyAssertions.loadOntology(expected))

  test("there are expected ontologies to check") {
    assert(xmiExpectedOntologies.nonEmpty, s"Found no expected/*.owl under $scenariosRoot")
    assert(scenariosWithTsv.nonEmpty, s"Found no input/tsv under $scenariosRoot")
  }

  xmiExpectedOntologies.foreach { (scenario, expected) =>
    val model = expected.getName.stripSuffix(".owl")

    test(s"${scenario.getName}: $model translates to its expected ontology") {
      val xmiExport = new File(scenario, s"input/xmi/sparx/$model.xml")
      assert(xmiExport.exists(), s"$expected has no export to be translated from; expected $xmiExport")

      assertTranslatesTo(expected, new File(outputRoot, s"${scenario.getName}/$model.owl"),
        Seq("-m", xmiExport.getPath) ++ xmiArguments)
    }
  }

  scenariosWithTsv.foreach { scenario =>
    test(s"${scenario.getName}: its TSV files translate to their expected ontology") {
      val expected = new File(scenario, s"expected/tsv/${scenario.getName}.owl")
      assert(expected.exists(), s"The TSV files of ${scenario.getName} have no expected ontology; expected $expected")

      val inputFiles = tsvFiles.map((flag, name) => flag -> new File(scenario, s"input/tsv/$name")).filter(_._2.exists())
      assert(inputFiles.nonEmpty, s"${scenario.getName}/input/tsv has none of ${tsvFiles.map(_._2).mkString(", ")}")

      assertTranslatesTo(expected, new File(outputRoot, s"${scenario.getName}/tsv/${scenario.getName}.owl"),
        inputFiles.flatMap((flag, file) => Seq(flag, file.getPath)) ++ tsvArguments)
    }
  }
