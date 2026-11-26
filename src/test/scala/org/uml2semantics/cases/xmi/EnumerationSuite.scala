package org.uml2semantics.cases.xmi

import org.uml2semantics.Uml2SemanticsSuite
import org.uml2semantics.cases.EnumerationAssertions
import org.uml2semantics.diagnostics.DiagnosticCode.{BlankIdentity, ClassifierKindConflict, EnumerationValueIRIConflict, EnumerationWithoutValues, UnresolvableCurie}
import org.uml2semantics.diagnostics.DiagnosticSeverity.{Error, Warning}
import org.uml2semantics.model.UMLEnumerationType

/**
 * Cases for reading enumerations and their values from XMI, following
 * `docs/specs/xmi-enumerations.md` rule by rule.
 *
 * The translation of an enumeration is pinned by the TSV enumeration group. The goldens here show
 * that an enumeration read from XMI comes out the same, and the model assertions show what the
 * reader left in the model where the ontology cannot.
 */
class EnumerationSuite extends Uml2SemanticsSuite with EnumerationAssertions:

  private val examplePrefix = Seq("-x", "ex:https://example.org/vocabulary/")
  private val xmiOverrides = Seq("--overrides", "XMI")

  // --- What is read --------------------------------------------------------------------------

  test("an enumeration and its literals are read with their definitions") {
    val result = runCase("xmi/enumeration/declared-with-values")
    assertNoDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", definition = "The frequency at which an employee is paid.", values = Set(
        Value("Daily", definition = "The employee is paid at the end of each day."),
        Value("Weekly"), Value("Monthly"))))
  }

  // --- Identity (rules 1 to 5) ---------------------------------------------------------------

  test("a name containing a colon is read as a curie, for an enumeration and for a literal") {
    val result = runCase("xmi/enumeration/named-by-curie", examplePrefix)
    assertNoDiagnostics(result)
    assertEnumerations(
      Enumeration(curie = "ex:PaymentSchedule", values = Set(Value(curie = "ex:Daily"), Value("Weekly"))))
  }

  test("an enumeration named by an unresolvable curie is rejected with its values") {
    val result = runCase("xmi/enumeration/enumeration-unresolvable-curie")
    assertDiagnostics(result, Error -> UnresolvableCurie)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("a literal named by an unresolvable curie is rejected and its enumeration is kept") {
    val result = runCase("xmi/enumeration/value-unresolvable-curie")
    assertDiagnostics(result, Error -> UnresolvableCurie)
    assertEnumerations(Enumeration("Size", values = Set(Value("Large"))))
  }

  test("an enumeration without a name is rejected with its values") {
    val result = runCase("xmi/enumeration/enumeration-without-name")
    assertDiagnostics(result, Error -> BlankIdentity)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("a literal with a blank name is rejected and its enumeration is kept") {
    val result = runCase("xmi/enumeration/value-with-blank-name")
    assertDiagnostics(result, Error -> BlankIdentity)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("enumerations of the same name in different packages are one enumeration") {
    val result = runCase("xmi/enumeration/same-name-in-two-packages")
    assertNoDiagnostics(result)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"), Value("Large"))))
  }

  test("a value is scoped by its enumeration") {
    val result = runCase("xmi/enumeration/values-scoped-by-enumeration")
    assertNoDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", values = Set(Value("Weekly"), Value("Other"))),
      Enumeration("ContractType", values = Set(Value("Permanent"), Value("Other"))))
  }

  test("a literal name repeated within one enumeration is one value") {
    val result = runCase("xmi/enumeration/repeated-literal")
    assertNoDiagnostics(result)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small", definition = "The smallest size."))))
  }

  test("an enumeration with the name of a class is rejected with its values") {
    val result = runCase("xmi/enumeration/enumeration-clashes-with-class")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  // --- Completeness and reading order (rules 6 to 9) -----------------------------------------

  test("an enumeration without literals is accepted and warned about") {
    val result = runCase("xmi/enumeration/without-values")
    assertDiagnostics(result, Warning -> EnumerationWithoutValues)
    assertEnumerations(
      Enumeration("PaymentSchedule", definition = "The frequency at which an employee is paid."))
  }

  test("an attribute type naming an enumeration resolves to it whatever the document order") {
    val result = runCase("xmi/enumeration/attribute-typed-by-enumeration")
    assertNoDiagnostics(result)
    assert(typeOfAttribute("paymentSchedule").exists(_.isInstanceOf[UMLEnumerationType]),
      s"Expected an enumeration type, found ${typeOfAttribute("paymentSchedule")}")
  }

  // --- Not read (rules 12, 13 and 15) --------------------------------------------------------

  test("an attribute owned by an enumeration is rejected") {
    val result = runCase("xmi/enumeration/enumeration-owns-attribute")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  test("a class that specialises an enumeration loses the generalization, not the class") {
    val result = runCase("xmi/enumeration/class-specialises-enumeration")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  test("an enumeration that specialises a class loses the generalization, not the enumeration") {
    val result = runCase("xmi/enumeration/enumeration-specialises-class")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  test("an enumeration nested in a class is not read") {
    val result = runCase("xmi/enumeration/nested-enumeration")
    assertNoDiagnostics(result)
    assertEnumerations()
  }

  // --- The IRI of a value (rules 16 to 19) ---------------------------------------------------

  test("a literal name that is not safe in an IRI keeps its label and gets underscores in its IRI") {
    val result = runCase("xmi/enumeration/unsafe-value-names")
    assertNoDiagnostics(result)
    assertEnumerations(Enumeration("Signal", values = Set(
      Value("Hp 0"), Value("Sh 2/Sh 0"), Value("Präzisionshalt hochpräzise"), Value("AE-E"), Value("12"),
      Value("v1.2_a~b"), Value("(+)"))))
  }

  test("a literal whose IRI another literal of its enumeration already has is rejected") {
    val result = runCase("xmi/enumeration/value-iri-conflict")
    assertDiagnostics(result, Error -> EnumerationValueIRIConflict)
    assertEnumerations(Enumeration("Signal", values = Set(Value("Hp 0"), Value("Hp 1"))))
  }

  // --- Combining XMI with TSV ----------------------------------------------------------------

  test("TSV gives an XMI enumeration a curie, a definition and further values") {
    val result = runCase("xmi/enumeration/tsv-gives-curie-and-definition", examplePrefix)
    assertNoDiagnostics(result)
    assertEnumerations(
      Enumeration("PaymentSchedule", "ex:SalarySchedule", "The frequency at which an employee is paid.", Set(
        Value("Daily", "ex:Daily", "Paid at the end of each day."),
        Value("Weekly"),
        Value("Monthly", definition = "Paid on the last day of the month."))))
  }

  test("an XMI enumeration given its values by TSV is not reported as having none") {
    val result = runCase("xmi/enumeration/tsv-gives-values")
    assertNoDiagnostics(result)
    assertEnumerations(Enumeration("PaymentSchedule", values = Set(Value("Daily"))))
  }

  test("a TSV class row naming an XMI enumeration is rejected") {
    val result = runCase("xmi/enumeration/tsv-class-names-xmi-enumeration")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  test("a TSV Parents cell naming an XMI enumeration is rejected and the row's class is kept") {
    val result = runCase("xmi/enumeration/tsv-parent-names-xmi-enumeration")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  test("a TSV attribute row owned by an XMI enumeration is rejected") {
    val result = runCase("xmi/enumeration/tsv-attribute-owner-is-xmi-enumeration")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  test("with XMI overriding, an XMI enumeration naming a TSV class is rejected") {
    val result = runCase("xmi/enumeration/xmi-enumeration-names-tsv-class", xmiOverrides)
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Size", values = Set(Value("Small"))))
  }

  test("with XMI overriding, an XMI class naming a TSV enumeration is rejected") {
    val result = runCase("xmi/enumeration/xmi-class-names-tsv-enumeration", xmiOverrides)
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  test("a TSV attribute type naming an XMI enumeration resolves to it") {
    val result = runCase("xmi/enumeration/tsv-attribute-uses-xmi-enumeration")
    assertNoDiagnostics(result)
    assert(typeOfAttribute("paymentSchedule").exists(_.isInstanceOf[UMLEnumerationType]),
      s"Expected an enumeration type, found ${typeOfAttribute("paymentSchedule")}")
  }
