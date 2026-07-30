package org.uml2semantics.cases.tsv

import org.uml2semantics.Uml2SemanticsSuite

/**
 * Cases for the Parents column.
 *
 * The TSV format has no covering or disjointness columns, so every generalization set it produces
 * is {incomplete, overlapping} and translates to subclass axioms alone. The four constraint
 * combinations are reachable only from XMI, and are therefore absent from this matrix by
 * construction rather than by omission.
 */
class GeneralizationSuite extends Uml2SemanticsSuite:

  test("a single parent yields a subclass axiom and no disjointness") {
    assertNoDiagnostics(runCase("tsv/generalization/single-parent"))
  }

  test("siblings in one generalization set are not made disjoint") {
    assertNoDiagnostics(runCase("tsv/generalization/two-children"))
  }

  test("a pipe-separated Parents cell yields one subclass axiom per parent") {
    assertNoDiagnostics(
      runCase("tsv/generalization/multiple-parents", Seq("--no-global-disjointness")))
  }

  test("a parent may be declared below its child") {
    assertNoDiagnostics(runCase("tsv/generalization/parent-declared-later"))
  }

  test("a parent with no row of its own is built on the fly") {
    assertNoDiagnostics(runCase("tsv/generalization/parent-not-declared"))
  }

  test("a three-level hierarchy asserts each link and infers nothing") {
    assertNoDiagnostics(runCase("tsv/generalization/three-level"))
  }

  test("a parent may be referenced by curie") {
    assertNoDiagnostics(runCase("tsv/generalization/parent-by-curie"))
  }
