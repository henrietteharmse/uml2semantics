package org.uml2semantics.cases.tsv

import org.uml2semantics.diagnostics.DiagnosticCode.NoInputSupplied
import org.uml2semantics.diagnostics.DiagnosticSeverity.Error
import org.uml2semantics.{Uml2SemanticsSuite, translateFromArguments}

import java.io.File

/**
 * Cases for the command line surface itself, rather than for what a file contains.
 *
 * That -c alone and -a alone both work is pinned by IdentitySuite and by the class-not-declared
 * case in AttributeTypeSuite; that -e alone and -n alone both work is pinned by the without-values
 * and built-on-the-fly cases in EnumerationSuite. That -i sets the ontology IRI is pinned by every case, because
 * ontology IRIs are compared as part of every golden.
 */
class CliSuite extends Uml2SemanticsSuite:

  test("supplying no input at all is an error, not a silent empty ontology") {
    val outputFile = new File("test-output/cases/tsv/cli/no-input.rdf")
    outputFile.getParentFile.mkdirs()

    val result = translateFromArguments(
      Seq("-i", ontologyIRI, "-p", ontologyPrefix, "-o", outputFile.getPath)).getOrElse(
      fail("Arguments should have parsed"))

    assertDiagnostics(result, Error -> NoInputSupplied)
  }

  test("arguments that do not parse yield no translation at all") {
    // -o is required, so omitting it fails before any reader runs.
    assertEquals(translateFromArguments(Seq("-i", ontologyIRI, "-p", ontologyPrefix)), None)
  }
