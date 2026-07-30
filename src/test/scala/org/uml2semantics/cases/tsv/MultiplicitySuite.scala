package org.uml2semantics.cases.tsv

import org.uml2semantics.Uml2SemanticsSuite
import org.uml2semantics.diagnostics.DiagnosticCode.{MaxBelowMin, UnboundedMinMultiplicity, UnparsableCardinality}
import org.uml2semantics.diagnostics.DiagnosticSeverity.{Error, Warning}

/**
 * Cases for translating a multiplicity into a cardinality restriction.
 *
 * These use data properties throughout, so the restrictions range over the datatype. That object
 * property restrictions instead range over owl:Thing is pinned by AttributeTypeSuite.
 */
class MultiplicitySuite extends Uml2SemanticsSuite:

  test("[1,1] becomes an exact cardinality") {
    assertNoDiagnostics(runCase("tsv/multiplicity/exact-one"))
  }

  test("[2,2] becomes an exact cardinality") {
    assertNoDiagnostics(runCase("tsv/multiplicity/exact-two"))
  }

  test("[0,0] becomes a max cardinality of zero rather than an exact cardinality") {
    assertNoDiagnostics(runCase("tsv/multiplicity/zero-zero"))
  }

  test("[1,5] becomes the intersection of a min and a max restriction") {
    assertNoDiagnostics(runCase("tsv/multiplicity/range"))
  }

  test("[0,3] becomes a max restriction only") {
    assertNoDiagnostics(runCase("tsv/multiplicity/zero-to-three"))
  }

  test("[2,*] becomes a min restriction only") {
    assertNoDiagnostics(runCase("tsv/multiplicity/two-to-unbounded"))
  }

  test("[0,*] produces no restriction") {
    assertNoDiagnostics(runCase("tsv/multiplicity/zero-to-unbounded"))
  }

  test("[*,*] produces no restriction") {
    assertNoDiagnostics(runCase("tsv/multiplicity/unbounded-both"))
  }

  test("[*,3] asserts no restriction and keeps the attribute") {
    val result = runCase("tsv/multiplicity/unbounded-min")
    assertDiagnostics(result, Error -> UnboundedMinMultiplicity)
  }

  test("blank bounds default to an exact cardinality of one") {
    assertNoDiagnostics(runCase("tsv/multiplicity/blank-bounds"))
  }

  test("an unparsable min warns and defaults to one, keeping the attribute") {
    val result = runCase("tsv/multiplicity/unparsable-min")
    assertDiagnostics(result, Warning -> UnparsableCardinality)
  }

  test("an unparsable max defaults to one, breaking the invariant, and only the restriction is lost") {
    val result = runCase("tsv/multiplicity/unparsable-max")
    assertDiagnostics(result, Warning -> UnparsableCardinality, Error -> MaxBelowMin)
  }

  test("an explicitly inverted multiplicity asserts no restriction rather than being repaired") {
    val result = runCase("tsv/multiplicity/max-below-min")
    assertDiagnostics(result, Error -> MaxBelowMin)
  }

  test("a negative cardinality is unparsable and defaults to one") {
    val result = runCase("tsv/multiplicity/negative-min")
    assertDiagnostics(result, Warning -> UnparsableCardinality)
  }
