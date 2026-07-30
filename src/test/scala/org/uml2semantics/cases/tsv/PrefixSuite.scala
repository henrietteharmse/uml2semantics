package org.uml2semantics.cases.tsv

import org.uml2semantics.Uml2SemanticsSuite
import org.uml2semantics.diagnostics.DiagnosticCode.UnresolvableCurie
import org.uml2semantics.diagnostics.DiagnosticSeverity.Error

/**
 * Cases for prefix registration and for how the shape of the ontology prefix IRI changes minting.
 *
 * That the predefined prefixes are available with no -x flag at all is pinned by every case in the
 * other groups that uses xsd:string.
 */
class PrefixSuite extends Uml2SemanticsSuite:

  test("a prefix IRI ending in # swaps the minting separators round") {
    assertNoDiagnostics(
      runCase("tsv/prefixes/hash-prefix", prefix = "t:https://uml2semantics.com/test/ontology#"))
  }

  test("a prefix IRI with no trailing separator has one inserted") {
    assertNoDiagnostics(
      runCase("tsv/prefixes/no-separator-prefix", prefix = "t:https://uml2semantics.com/test/ontology"))
  }

  test("supplying -x does not hide the predefined prefixes") {
    assertNoDiagnostics(
      runCase("tsv/prefixes/extra-prefix-keeps-predefined", Seq("-x", "schema:https://schema.org/")))
  }

  test("an attribute type whose prefix was never configured drops the row") {
    val result = runCase("tsv/prefixes/unregistered-prefix-in-type")
    assertDiagnostics(result, Error -> UnresolvableCurie)
  }
