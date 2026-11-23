package org.uml2semantics.cases.tsv

import org.uml2semantics.diagnostics.DiagnosticCode.{BlankIdentity, ClassifierKindConflict, EnumerationWithoutValues, GlobalDisjointnessRelaxed, NameContainsColon, RowRejected, UnresolvableCurie}
import org.uml2semantics.diagnostics.DiagnosticSeverity.{Error, Warning}
import org.uml2semantics.model.cache.{AttributeCache, EnumerationCache}
import org.uml2semantics.model.{UMLAttributeType, UMLEnumerationType}
import org.uml2semantics.Uml2SemanticsSuite

/**
 * Cases for enumerations: reading them and their values from TSV, following
 * `docs/specs/tsv-enumerations.md`, and translating them to OWL.
 *
 * A case's golden pins the translation. Most cases also assert the enumerations the reader left in
 * the model. That is not redundant: the ontology cannot show which half of an identity a row
 * supplied, or that two rows became one value, once only the resulting IRI and label are written.
 */
class EnumerationSuite extends Uml2SemanticsSuite:

  /** What a case expects of one value. A blank half or definition means "not given". */
  private case class Value(name: String = "", curie: String = "", definition: String = "")

  /** What a case expects of one enumeration. A blank half or definition means "not given". */
  private case class Enumeration(name: String = "", curie: String = "", definition: String = "",
                                 values: Set[Value] = Set.empty)

  private val examplePrefix = Seq("-x", "ex:https://example.org/vocabulary/")

  private def assertEnumerations(expected: Enumeration*): Unit =
    val actual = EnumerationCache.getEnumerations.map(enumeration =>
      Enumeration(
        enumeration.identity.nameOption.map(_.name).getOrElse(""),
        enumeration.identity.curieOption.map(_.curie).getOrElse(""),
        enumeration.definition.map(_.definition).getOrElse(""),
        enumeration.values.map(value =>
          Value(
            value.identity.nameOption.map(_.name).getOrElse(""),
            value.identity.curieOption.map(_.curie).getOrElse(""),
            value.definition.map(_.definition).getOrElse("")))))
    assertEquals(actual, expected.toSet)

  private def typeOfAttribute(label: String): Option[UMLAttributeType] =
    AttributeCache.getAttributes.find(_.getLabel == label).flatMap(_.typeOfAttribute)

  // --- The two files -------------------------------------------------------------------------

  test("an enumeration and its values are read with their definitions") {
    val result = runCase("tsv/enumeration/declared-with-values")
    assertNoDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", definition = "The frequency at which an employee is paid.", values = Set(
        Value("Daily", definition = "The employee is paid at the end of each day."),
        Value("Weekly", definition = "The employee is paid at the end of each week."),
        Value("Monthly", definition = "The employee is paid on the last day of the month."))))
  }

  test("an enumeration and its values may be identified by curie alone") {
    val result = runCase("tsv/enumeration/curie-only", examplePrefix)
    assertNoDiagnostics(result)
    assertEnumerations(
      Enumeration(curie = "ex:PaymentSchedule", values = Set(Value(curie = "ex:Daily"), Value(curie = "ex:Weekly"))))
  }

  // --- Identity (rules 1 to 4) ---------------------------------------------------------------

  test("a value row may name its enumeration by either half, in any column order") {
    val result = runCase("tsv/enumeration/either-half-names-the-enumeration", examplePrefix)
    assertNoDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", "ex:SalarySchedule", "Defines the frequency of payments.", Set(
        Value(curie = "ex:Daily"), Value("Weekly"), Value(curie = "ex:Monthly"))))
  }

  test("a value is scoped by its enumeration, and rows of different enumerations may interleave") {
    val result = runCase("tsv/enumeration/values-scoped-by-enumeration")
    assertNoDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", values = Set(
        Value("Weekly"), Value("Other", definition = "Any other payment schedule."))),
      Enumeration("ContractType", values = Set(
        Value("Permanent"), Value("Other", definition = "Any other contract type."))))
  }

  test("repeated rows enrich an enumeration and its values instead of duplicating them") {
    val result = runCase("tsv/enumeration/repeated-rows-enrich", examplePrefix)
    assertNoDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", "ex:SalarySchedule", "The frequency of payments.", Set(
        Value("Daily", "ex:Daily", "Paid daily."), Value("Weekly", definition = "Paid weekly."))))
  }

  test("a row carrying both halves ties a name-only and a curie-only enumeration together") {
    val result = runCase("tsv/enumeration/full-row-ties-the-halves", examplePrefix)
    assertNoDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", "ex:SalarySchedule", "The frequency of payments.", Set(
        Value("Daily"), Value("Weekly"))))
  }

  // --- Completeness (rules 6 and 7) ----------------------------------------------------------

  test("a value row builds its enumeration on the fly when it was never declared") {
    val result = runCase("tsv/enumeration/built-on-the-fly")
    assertNoDiagnostics(result)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"), Value("Weekly"))))
  }

  test("an enumeration without values is accepted and warned about") {
    val result = runCase("tsv/enumeration/without-values")
    assertDiagnostics(result, Warning -> EnumerationWithoutValues)
    assertEnumerations(
      Enumeration("PaymentSchedule", definition = "The frequency at which an employee is paid."))
  }

  test("an enumeration with one value is closed by a one-of but declares nothing different") {
    val result = runCase("tsv/enumeration/single-value")
    assertNoDiagnostics(result)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"))))
  }

  // --- Rows that cannot be honoured ----------------------------------------------------------

  test("an enumeration with neither name nor curie is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/enumeration-blank-identity")
    assertDiagnostics(result, Error -> BlankIdentity)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"))))
  }

  test("an enumeration name containing a colon is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/enumeration-name-with-colon")
    assertDiagnostics(result, Error -> NameContainsColon)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"))))
  }

  test("an enumeration curie with an unconfigured prefix is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/enumeration-unresolvable-curie")
    assertDiagnostics(result, Error -> UnresolvableCurie)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"))))
  }

  test("a value with neither name nor curie is rejected and leaves no enumeration behind") {
    val result = runCase("tsv/enumeration/value-blank-identity")
    assertDiagnostics(result, Error -> BlankIdentity)
    assertEnumerations(Enumeration("ContractType", values = Set(Value("Permanent"))))
  }

  test("a value row with a blank enumeration is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/value-blank-enumeration")
    assertDiagnostics(result, Error -> BlankIdentity)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Weekly"))))
  }

  test("a value name containing a colon is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/value-name-with-colon")
    assertDiagnostics(result, Error -> NameContainsColon)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Weekly"))))
  }

  test("a value curie with an unconfigured prefix is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/value-unresolvable-curie")
    assertDiagnostics(result, Error -> UnresolvableCurie)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Weekly"))))
  }

  test("a value row naming its enumeration by an unresolvable curie is rejected") {
    val result = runCase("tsv/enumeration/value-unresolvable-enumeration-curie")
    assertDiagnostics(result, Error -> UnresolvableCurie)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Weekly"))))
  }

  test("a header missing a required column rejects every row") {
    val result = runCase("tsv/enumeration/missing-column")
    assertDiagnostics(result, Error -> RowRejected, Error -> RowRejected)
    assertEnumerations()
  }

  // --- One namespace for classes and enumerations (rules 5 and 12) ---------------------------

  test("an enumeration sharing a name with a class is rejected") {
    val result = runCase("tsv/enumeration/enumeration-clashes-with-class")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("an enumeration sharing a curie with a class is rejected") {
    val result = runCase("tsv/enumeration/enumeration-clashes-with-class-curie")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("a value row cannot build an enumeration under the name of a class") {
    val result = runCase("tsv/enumeration/value-names-a-class")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("a class built on the fly as a parent wins over an enumeration of the same name") {
    val result = runCase("tsv/enumeration/parent-is-enumeration")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("an attribute row owned by an enumeration is rejected and builds no class") {
    val result = runCase("tsv/enumeration/attribute-owner-is-enumeration")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  // --- Enumerations as attribute types (rule 9) ----------------------------------------------

  test("an attribute type naming an enumeration yields an object property ranging over its class") {
    val result = runCase("tsv/enumeration/attribute-typed-by-enumeration")
    assertNoDiagnostics(result)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"))))
    assert(typeOfAttribute("paymentSchedule").exists(_.isInstanceOf[UMLEnumerationType]),
      s"Expected an enumeration type, found ${typeOfAttribute("paymentSchedule")}")
  }

  test("an attribute type may use an enumeration's curie, and still ranges over the enumeration's class") {
    val result = runCase("tsv/enumeration/attribute-typed-by-enumeration-curie", examplePrefix)
    assertNoDiagnostics(result)
    assertEnumerations(Enumeration("PaymentSchedule", "ex:SalarySchedule", values = Set(Value("Daily"))))
    assert(typeOfAttribute("paymentSchedule").exists(_.isInstanceOf[UMLEnumerationType]),
      s"Expected an enumeration type, found ${typeOfAttribute("paymentSchedule")}")
  }

  // --- IRI minting ---------------------------------------------------------------------------

  test("a prefix IRI ending in # swaps the separators of a value's IRI round") {
    assertNoDiagnostics(
      runCase("tsv/enumeration/hash-prefix", prefix = "t:https://uml2semantics.com/test/ontology#"))
  }

  test("a prefix IRI with no trailing separator has one inserted before the enumeration") {
    assertNoDiagnostics(
      runCase("tsv/enumeration/no-separator-prefix", prefix = "t:https://uml2semantics.com/test/ontology"))
  }

  test("a name-only value of a curie-only enumeration is qualified by the curie text") {
    assertNoDiagnostics(runCase("tsv/enumeration/name-only-value-of-curie-only-enumeration", examplePrefix))
  }

  // --- Global disjointness -------------------------------------------------------------------

  test("enumerations are disjoint from the root classes and from each other") {
    assertNoDiagnostics(runCase("tsv/enumeration/globally-disjoint"))
  }

  test("--no-global-disjointness also leaves enumerations out of any disjointness axiom") {
    assertNoDiagnostics(runCase("tsv/enumeration/no-global-disjointness", Seq("--no-global-disjointness")))
  }

  test("enumerations sharing a value are left out of global disjointness, with a warning") {
    val result = runCase("tsv/enumeration/shared-value-curie", examplePrefix)
    assertDiagnostics(result, Warning -> GlobalDisjointnessRelaxed)
  }
