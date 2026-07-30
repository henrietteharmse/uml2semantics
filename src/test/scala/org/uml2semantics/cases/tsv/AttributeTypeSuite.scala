package org.uml2semantics.cases.tsv

import org.uml2semantics.Uml2SemanticsSuite
import org.uml2semantics.diagnostics.DiagnosticCode.BlankAttributeType
import org.uml2semantics.diagnostics.DiagnosticSeverity.Warning

/**
 * Cases for the four-step attribute-type resolution order: supported datatype, then a class
 * already in the diagram, then a curie denoting an external class, then a placeholder class built
 * on the fly.
 */
class AttributeTypeSuite extends Uml2SemanticsSuite:

  test("a supported datatype yields a data property ranging over that datatype") {
    assertNoDiagnostics(runCase("tsv/attribute-type/datatype"))
  }

  test("a class in the diagram yields an object property restricted over owl:Thing") {
    assertNoDiagnostics(runCase("tsv/attribute-type/class-in-diagram"))
  }

  test("a curie with a configured prefix yields an object property onto an external class") {
    assertNoDiagnostics(
      runCase("tsv/attribute-type/external-curie", Seq("-x", "schema:https://schema.org/")))
  }

  test("an unrecognised type becomes a placeholder class built on the fly") {
    assertNoDiagnostics(runCase("tsv/attribute-type/unknown-type-becomes-class"))
  }

  test("a blank type yields an untyped object property and warns") {
    val result = runCase("tsv/attribute-type/blank-type")
    assertDiagnostics(result, Warning -> BlankAttributeType)
  }

  test("an attribute curie supplies the property IRI while the name supplies the label") {
    assertNoDiagnostics(runCase("tsv/attribute-type/attribute-with-curie"))
  }

  test("an attribute may name a class that was never declared") {
    assertNoDiagnostics(runCase("tsv/attribute-type/class-not-declared"))
  }
