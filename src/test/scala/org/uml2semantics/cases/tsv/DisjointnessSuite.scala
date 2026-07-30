package org.uml2semantics.cases.tsv

import org.uml2semantics.Uml2SemanticsSuite
import org.uml2semantics.diagnostics.{DiagnosticCode, DiagnosticSeverity}

/**
 * Cases for the default disjointness assumption and the flag that switches it off.
 *
 * The case where fewer than two classes are eligible, and the axiom is skipped, is pinned by every
 * single-class case in IdentitySuite rather than repeated here.
 */
class DisjointnessSuite extends Uml2SemanticsSuite:

  test("two unrelated classes are declared mutually disjoint") {
    assertNoDiagnostics(runCase("tsv/disjointness/two-unrelated-classes"))
  }

  test("--no-global-disjointness removes the axiom entirely") {
    assertNoDiagnostics(
      runCase("tsv/disjointness/flag-disables", Seq("--no-global-disjointness")))
  }

  test("a generalization set's parent stays in the global axiom while its children are excluded") {
    assertNoDiagnostics(runCase("tsv/disjointness/parent-retained"))
  }

  test("a class named only by an attribute row counts towards disjointness like any other") {
    assertNoDiagnostics(runCase("tsv/disjointness/reference-only-class"))
  }

  test("two ancestors of one class are dropped from the global axiom rather than made disjoint") {
    assertDiagnostics(runCase("tsv/disjointness/multiple-parents-relaxed"),
      (DiagnosticSeverity.Warning, DiagnosticCode.GlobalDisjointnessRelaxed))
  }

  test("relaxation is confined to the ancestors that would contradict each other") {
    assertDiagnostics(runCase("tsv/disjointness/unrelated-classes-survive-relaxation"),
      (DiagnosticSeverity.Warning, DiagnosticCode.GlobalDisjointnessRelaxed))
  }

  test("a chain of single inheritance is not relaxed however deep it runs") {
    assertNoDiagnostics(runCase("tsv/disjointness/deep-single-inheritance-retained"))
  }

  test("a grandparent contradicting a parent is relaxed, not just direct parents") {
    assertDiagnostics(runCase("tsv/disjointness/indirect-ancestor-relaxed"),
      (DiagnosticSeverity.Warning, DiagnosticCode.GlobalDisjointnessRelaxed))
  }
