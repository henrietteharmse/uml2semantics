package org.uml2semantics.cases.tsv

import org.uml2semantics.Uml2SemanticsSuite
import org.uml2semantics.diagnostics.DiagnosticCode.{BlankIdentity, NameContainsColon, UnresolvableCurie}
import org.uml2semantics.diagnostics.DiagnosticSeverity.Error

/**
 * Cases for how a class is identified, how its IRI is minted, and what happens to a row whose
 * identity cannot be read. Every case here has a single surviving class, which also pins the rule
 * that global disjointness is skipped below two eligible classes.
 */
class IdentitySuite extends Uml2SemanticsSuite:

  test("a name-only class mints its IRI under the ontology prefix") {
    assertNoDiagnostics(runCase("tsv/identity/name-only"))
  }

  test("a curie-only class expands its prefix and labels itself with the curie") {
    assertNoDiagnostics(runCase("tsv/identity/curie-only"))
  }

  test("a class with both halves takes its IRI from the curie and its label from the name") {
    assertNoDiagnostics(runCase("tsv/identity/name-and-curie"))
  }

  test("a blank definition produces no comment annotation") {
    assertNoDiagnostics(runCase("tsv/identity/no-definition"))
  }

  test("a name containing a colon is rejected and parsing continues") {
    val result = runCase("tsv/identity/name-with-colon")
    assertDiagnostics(result, Error -> NameContainsColon)
  }

  test("a row with neither name nor curie is rejected and parsing continues") {
    val result = runCase("tsv/identity/blank-identity")
    assertDiagnostics(result, Error -> BlankIdentity)
  }

  test("a curie with an unconfigured prefix is rejected and parsing continues") {
    val result = runCase("tsv/identity/unresolvable-curie")
    assertDiagnostics(result, Error -> UnresolvableCurie)
  }
