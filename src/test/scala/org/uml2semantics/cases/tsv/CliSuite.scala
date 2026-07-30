package org.uml2semantics.cases.tsv

import org.uml2semantics.diagnostics.DiagnosticCode.{EnumerationInputIgnored, NoInputSupplied}
import org.uml2semantics.diagnostics.DiagnosticSeverity.{Error, Warning}
import org.uml2semantics.{Uml2SemanticsSuite, convertFromArguments}

import java.io.File

/**
 * Cases for the command line surface itself, rather than for what a file contains.
 *
 * That -c alone and -a alone both work is pinned by IdentitySuite and by the class-not-declared
 * case in AttributeTypeSuite. That -i sets the ontology IRI is pinned by every case, because
 * ontology IRIs are compared as part of every golden.
 */
class CliSuite extends Uml2SemanticsSuite:

  private val casesDirectory = "src/test/resources/cases/tsv/cli"

  test("enumeration input is warned about and ignored") {
    val result = runCase("tsv/cli/enumerations-ignored", Seq(
      "-e", s"$casesDirectory/enumerations-ignored.enumerations.tsv",
      "-n", s"$casesDirectory/enumerations-ignored.enumeration-values.tsv"))
    assertDiagnostics(result, Warning -> EnumerationInputIgnored)
  }

  test("supplying no input at all is an error, not a silent empty ontology") {
    val outputFile = new File("test-output/cases/tsv/cli/no-input.rdf")
    outputFile.getParentFile.mkdirs()

    val result = convertFromArguments(
      Seq("-i", ontologyIRI, "-p", ontologyPrefix, "-o", outputFile.getPath)).getOrElse(
      fail("Arguments should have parsed"))

    assertDiagnostics(result, Error -> NoInputSupplied)
  }

  test("arguments that do not parse yield no conversion at all") {
    // -o is required, so omitting it fails before any reader runs.
    assertEquals(convertFromArguments(Seq("-i", ontologyIRI, "-p", ontologyPrefix)), None)
  }
