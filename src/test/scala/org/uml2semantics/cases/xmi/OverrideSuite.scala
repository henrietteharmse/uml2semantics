package org.uml2semantics.cases.xmi

import org.uml2semantics.Uml2SemanticsSuite
import org.uml2semantics.diagnostics.DiagnosticCode.UnresolvableCurie
import org.uml2semantics.diagnostics.DiagnosticSeverity.Error

/**
 * Cases for a row of a TSV file that names a class or an enumeration the XMI file already has.
 *
 * TSV overrides XMI by default, which means the row has the last word on what it states. What it
 * leaves blank it does not state, so the class keeps what the diagram gave it. Every case reads the
 * same diagram: `Person` with a definition and the children `Employee` and `Employer`.
 */
class OverrideSuite extends Uml2SemanticsSuite:

  private val examplePrefix = Seq("-x", "ex:https://example.org/vocabulary/")
  private val xmiOverrides = Seq("--overrides", "XMI")

  test("a row giving only the name of an XMI class keeps its definition and its children") {
    assertNoDiagnostics(runCase("xmi/override/row-by-name-keeps-definition-and-children"))
  }

  test("a row giving a definition replaces the definition of an XMI class and keeps its children") {
    assertNoDiagnostics(runCase("xmi/override/row-gives-definition"))
  }

  test("a row giving a curie moves an XMI class to that IRI and keeps its definition and its children") {
    assertNoDiagnostics(runCase("xmi/override/row-gives-curie", examplePrefix))
  }

  test("a row whose curie cannot be expanded is rejected and leaves the XMI class as it was read") {
    val result = runCase("xmi/override/row-with-unresolvable-curie")
    assertDiagnostics(result, Error -> UnresolvableCurie)
  }

  // --- Children ------------------------------------------------------------------------------
  //
  // The children of a class are added to, whichever source gives them and in whichever order the
  // sources are read. Neither source can take a child away that the other gave.

  test("a TSV child of an XMI class is added to its XMI children") {
    assertNoDiagnostics(runCase("xmi/override/child-added-to-children"))
  }

  test("a TSV child of an XMI class is added to its XMI children when TSV also has a row for the class") {
    assertNoDiagnostics(runCase("xmi/override/child-added-beside-row-for-parent"))
  }

  test("with XMI overriding, the XMI children of a class are added to its TSV child") {
    assertNoDiagnostics(runCase("xmi/override/child-added-when-xmi-overrides", xmiOverrides))
  }

  test("a row giving a child of an XMI class a curie moves its generalization to that IRI") {
    assertNoDiagnostics(runCase("xmi/override/child-given-curie", examplePrefix))
  }

  test("a row giving a child in an XMI generalization set a curie moves the set's axioms to that IRI") {
    assertNoDiagnostics(runCase("xmi/override/child-of-generalization-set-given-curie", examplePrefix))
  }

  // --- Types ---------------------------------------------------------------------------------
  //
  // A property ranges over the class or enumeration that is its type, under the IRI that classifier
  // ends up with.

  test("a row giving the target of an XMI association a curie moves the range of its property") {
    assertNoDiagnostics(runCase("xmi/override/association-target-given-curie", examplePrefix))
  }

  test("a row giving the type of an XMI attribute a curie moves the range of its property") {
    assertNoDiagnostics(runCase("xmi/override/attribute-type-given-curie", examplePrefix))
  }

  test("a row giving an XMI enumeration a curie moves the range of an attribute typed by it") {
    assertNoDiagnostics(runCase("xmi/override/attribute-enumeration-type-given-curie", examplePrefix))
  }
