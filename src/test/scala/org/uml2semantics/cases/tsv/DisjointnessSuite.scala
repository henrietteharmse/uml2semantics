package org.uml2semantics.cases.tsv

import org.uml2semantics.Uml2SemanticsSuite

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

  test("multiple inheritance under the default makes the child unsatisfiable") {
    assertNoDiagnostics(runCase("tsv/disjointness/multiple-parents-unsatisfiable"))
  }
