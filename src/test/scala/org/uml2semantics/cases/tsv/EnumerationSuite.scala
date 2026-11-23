package org.uml2semantics.cases.tsv

import org.uml2semantics.diagnostics.DiagnosticCode.{BlankIdentity, ClassifierKindConflict, EnumerationWithoutValues, EnumerationsNotTranslated, NameContainsColon, RowRejected, UnresolvableCurie}
import org.uml2semantics.diagnostics.DiagnosticSeverity.{Error, Warning}
import org.uml2semantics.diagnostics.{DiagnosticCode, DiagnosticSeverity}
import org.uml2semantics.model.cache.{AttributeBuilderCache, EnumerationCache}
import org.uml2semantics.model.{UMLAttributeType, UMLEnumerationType}
import org.uml2semantics.{ConversionResult, Uml2SemanticsSuite}

/**
 * Cases for reading enumerations and their values from TSV, following
 * `docs/specs/tsv-enumerations.md`.
 *
 * Enumerations are read into the model but not translated to OWL yet. A golden can therefore only
 * show that an enumeration leaves the ontology untouched, so every case here also asserts the
 * enumerations the reader left in the model, which is where the behaviour under test actually is.
 * For the same reason every run carries the `EnumerationsNotTranslated` warning;
 * `assertEnumerationDiagnostics` adds it so a case lists only the diagnostics it is about.
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

  private def assertEnumerationDiagnostics(result: ConversionResult,
                                           expected: (DiagnosticSeverity, DiagnosticCode)*): Unit =
    assertDiagnostics(result, (Warning -> EnumerationsNotTranslated) +: expected*)

  private def typeOfAttribute(label: String): Option[UMLAttributeType] =
    AttributeBuilderCache.getAttributes.find(_.getLabel == label).flatMap(_.typeOfAttribute)

  // --- The two files -------------------------------------------------------------------------

  test("an enumeration and its values are read with their definitions") {
    val result = runCase("tsv/enumeration/declared-with-values")
    assertEnumerationDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", definition = "The frequency at which an employee is paid.", values = Set(
        Value("Daily", definition = "The employee is paid at the end of each day."),
        Value("Weekly", definition = "The employee is paid at the end of each week."),
        Value("Monthly", definition = "The employee is paid on the last day of the month."))))
  }

  test("an enumeration and its values may be identified by curie alone") {
    val result = runCase("tsv/enumeration/curie-only", examplePrefix)
    assertEnumerationDiagnostics(result)
    assertEnumerations(
      Enumeration(curie = "ex:PaymentSchedule", values = Set(Value(curie = "ex:Daily"), Value(curie = "ex:Weekly"))))
  }

  // --- Identity (rules 1 to 4) ---------------------------------------------------------------

  test("a value row may name its enumeration by either half, in any column order") {
    val result = runCase("tsv/enumeration/either-half-names-the-enumeration", examplePrefix)
    assertEnumerationDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", "ex:SalarySchedule", "Defines the frequency of payments.", Set(
        Value(curie = "ex:Daily"), Value("Weekly"), Value(curie = "ex:Monthly"))))
  }

  test("a value is scoped by its enumeration, and rows of different enumerations may interleave") {
    val result = runCase("tsv/enumeration/values-scoped-by-enumeration")
    assertEnumerationDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", values = Set(
        Value("Weekly"), Value("Other", definition = "Any other payment schedule."))),
      Enumeration("ContractType", values = Set(
        Value("Permanent"), Value("Other", definition = "Any other contract type."))))
  }

  test("repeated rows enrich an enumeration and its values instead of duplicating them") {
    val result = runCase("tsv/enumeration/repeated-rows-enrich", examplePrefix)
    assertEnumerationDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", "ex:SalarySchedule", "The frequency of payments.", Set(
        Value("Daily", "ex:Daily", "Paid daily."), Value("Weekly", definition = "Paid weekly."))))
  }

  test("a row carrying both halves ties a name-only and a curie-only enumeration together") {
    val result = runCase("tsv/enumeration/full-row-ties-the-halves", examplePrefix)
    assertEnumerationDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", "ex:SalarySchedule", "The frequency of payments.", Set(
        Value("Daily"), Value("Weekly"))))
  }

  // --- Completeness (rules 6 and 7) ----------------------------------------------------------

  test("a value row builds its enumeration on the fly when it was never declared") {
    val result = runCase("tsv/enumeration/built-on-the-fly")
    assertEnumerationDiagnostics(result)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"), Value("Weekly"))))
  }

  test("an enumeration without values is accepted and warned about") {
    val result = runCase("tsv/enumeration/without-values")
    assertEnumerationDiagnostics(result, Warning -> EnumerationWithoutValues)
    assertEnumerations(
      Enumeration("PaymentSchedule", definition = "The frequency at which an employee is paid."))
  }

  // --- Rows that cannot be honoured ----------------------------------------------------------

  test("an enumeration with neither name nor curie is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/enumeration-blank-identity")
    assertEnumerationDiagnostics(result, Error -> BlankIdentity)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"))))
  }

  test("an enumeration name containing a colon is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/enumeration-name-with-colon")
    assertEnumerationDiagnostics(result, Error -> NameContainsColon)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"))))
  }

  test("an enumeration curie with an unconfigured prefix is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/enumeration-unresolvable-curie")
    assertEnumerationDiagnostics(result, Error -> UnresolvableCurie)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"))))
  }

  test("a value with neither name nor curie is rejected and leaves no enumeration behind") {
    val result = runCase("tsv/enumeration/value-blank-identity")
    assertEnumerationDiagnostics(result, Error -> BlankIdentity)
    assertEnumerations(Enumeration("ContractType", values = Set(Value("Permanent"))))
  }

  test("a value row with a blank enumeration is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/value-blank-enumeration")
    assertEnumerationDiagnostics(result, Error -> BlankIdentity)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Weekly"))))
  }

  test("a value name containing a colon is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/value-name-with-colon")
    assertEnumerationDiagnostics(result, Error -> NameContainsColon)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Weekly"))))
  }

  test("a value curie with an unconfigured prefix is rejected and parsing continues") {
    val result = runCase("tsv/enumeration/value-unresolvable-curie")
    assertEnumerationDiagnostics(result, Error -> UnresolvableCurie)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Weekly"))))
  }

  test("a value row naming its enumeration by an unresolvable curie is rejected") {
    val result = runCase("tsv/enumeration/value-unresolvable-enumeration-curie")
    assertEnumerationDiagnostics(result, Error -> UnresolvableCurie)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Weekly"))))
  }

  test("a header missing a required column rejects every row") {
    val result = runCase("tsv/enumeration/missing-column")
    assertEnumerationDiagnostics(result, Error -> RowRejected, Error -> RowRejected)
    assertEnumerations()
  }

  // --- One namespace for classes and enumerations (rules 5 and 12) ---------------------------

  test("an enumeration sharing a name with a class is rejected") {
    val result = runCase("tsv/enumeration/enumeration-clashes-with-class")
    assertEnumerationDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("an enumeration sharing a curie with a class is rejected") {
    val result = runCase("tsv/enumeration/enumeration-clashes-with-class-curie")
    assertEnumerationDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("a value row cannot build an enumeration under the name of a class") {
    val result = runCase("tsv/enumeration/value-names-a-class")
    assertEnumerationDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("a class built on the fly as a parent wins over an enumeration of the same name") {
    val result = runCase("tsv/enumeration/parent-is-enumeration")
    assertEnumerationDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("an attribute row owned by an enumeration is rejected and builds no class") {
    val result = runCase("tsv/enumeration/attribute-owner-is-enumeration")
    assertEnumerationDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  // --- Enumerations as attribute types (rule 9) ----------------------------------------------

  test("an attribute type naming an enumeration resolves to it rather than to a placeholder class") {
    val result = runCase("tsv/enumeration/attribute-typed-by-enumeration")
    assertEnumerationDiagnostics(result)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"))))
    assert(typeOfAttribute("paymentSchedule").exists(_.isInstanceOf[UMLEnumerationType]),
      s"Expected an enumeration type, found ${typeOfAttribute("paymentSchedule")}")
  }

  test("an attribute type may use an enumeration's curie, which wins over reading it as an external class") {
    val result = runCase("tsv/enumeration/attribute-typed-by-enumeration-curie", examplePrefix)
    assertEnumerationDiagnostics(result)
    assertEnumerations(Enumeration("PaymentSchedule", "ex:SalarySchedule", values = Set(Value("Daily"))))
    assert(typeOfAttribute("paymentSchedule").exists(_.isInstanceOf[UMLEnumerationType]),
      s"Expected an enumeration type, found ${typeOfAttribute("paymentSchedule")}")
  }
