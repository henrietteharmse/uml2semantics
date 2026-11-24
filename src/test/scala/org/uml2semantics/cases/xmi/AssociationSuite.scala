package org.uml2semantics.cases.xmi

import org.uml2semantics.cases.EnumerationAssertions
import org.uml2semantics.diagnostics.DiagnosticCode.{ClassifierKindConflict, MaxBelowMin, RowRejected, UnboundedMinMultiplicity, UnnamedAssociation, UnparsableCardinality, UnresolvableCurie}
import org.uml2semantics.diagnostics.DiagnosticSeverity.{Error, Warning}
import org.uml2semantics.model.cache.AttributeCache
import org.uml2semantics.model.{UMLCardinality, UMLClassType, UMLEnumerationType, UMLInfiniteCardinality, UMLNonNegativeCardinality}
import org.uml2semantics.{TranslationResult, Uml2SemanticsSuite, translateFromArguments}

import java.io.File

/**
 * Cases for reading associations from XMI, following `docs/specs/xmi-associations.md` rule by rule.
 *
 * An association becomes an attribute and nothing else, so a golden here is the golden the
 * equivalent row of a TSV attributes file would have: an object property with a domain, a range
 * and a cardinality restriction.
 */
class AssociationSuite extends Uml2SemanticsSuite with EnumerationAssertions:

  private val examplePrefix = Seq("-x", "ex:https://example.org/vocabulary/")

  // --- Name and direction (rules 1 to 7) -----------------------------------------------------

  test("a named association becomes an attribute of its source, typed by its target") {
    assertNoDiagnostics(runCase("xmi/association/named"))
  }

  test("a multiplicity on the source end changes neither the direction nor the attribute") {
    assertNoDiagnostics(runCase("xmi/association/multiplicity-on-source-end"))
  }

  test("a role name on the target end takes precedence over the association name") {
    assertNoDiagnostics(runCase("xmi/association/target-role-name"))
  }

  test("a role name on the source end gives a second attribute in the opposite direction") {
    assertNoDiagnostics(runCase("xmi/association/source-role-name"))
  }

  test("a role name on the source end is enough to name an otherwise unnamed association") {
    assertNoDiagnostics(runCase("xmi/association/source-role-name-only"))
  }

  test("an association name containing a colon is read as a curie") {
    assertNoDiagnostics(runCase("xmi/association/named-by-curie", examplePrefix))
  }

  test("an association named by an unresolvable curie is rejected and its classes are kept") {
    val result = runCase("xmi/association/unresolvable-curie")
    assertDiagnostics(result, Error -> UnresolvableCurie)
  }

  test("navigability is not read") {
    assertNoDiagnostics(runCase("xmi/association/navigability-ignored"))
  }

  // --- Unnamed associations (rules 8 to 10) --------------------------------------------------

  test("an unnamed association with a supporting attribute adds nothing and is not reported") {
    assertNoDiagnostics(runCase("xmi/association/unnamed-with-supporting-attribute"))
  }

  test("a supporting attribute may belong to the class at the target end") {
    assertNoDiagnostics(runCase("xmi/association/unnamed-with-supporting-attribute-at-target"))
  }

  test("an unnamed association without a supporting attribute is reported and writes nothing") {
    val result = runCase("xmi/association/unnamed-without-supporting-attribute")
    assertDiagnostics(result, Error -> UnnamedAssociation)
  }

  // --- What an association becomes (rules 12 to 17) ------------------------------------------

  test("an association drawn from an enumeration is rejected") {
    val result = runCase("xmi/association/from-enumeration")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  test("a role name on the source end of an association drawn to an enumeration is rejected") {
    val result = runCase("xmi/association/source-role-at-enumeration")
    assertDiagnostics(result, Error -> ClassifierKindConflict)
    assertEnumerations(Enumeration("Colour", values = Set(Value("Red"))))
  }

  test("an association ending at an enumeration gives an attribute typed by the enumeration") {
    val result = runCase("xmi/association/to-enumeration")
    assertNoDiagnostics(result)
    assert(typeOfAttribute("paymentSchedule").exists(_.isInstanceOf[UMLEnumerationType]),
      s"Expected an enumeration type, found ${typeOfAttribute("paymentSchedule")}")
  }

  test("an association with an end that is neither a class nor an enumeration is rejected") {
    val result = runCase("xmi/association/end-is-not-a-classifier")
    assertDiagnostics(result, Error -> RowRejected)
  }

  test("n, m..n and * are read as [n,n], [m,n] and [0,*]") {
    assertNoDiagnostics(runCase("xmi/association/multiplicity-forms"))
  }

  test("an unparsable multiplicity warns once and is taken to be 1") {
    val result = runCase("xmi/association/multiplicity-unparsable")
    assertDiagnostics(result, Warning -> UnparsableCardinality)
  }

  test("a multiplicity whose maximum is below its minimum asserts no restriction") {
    val result = runCase("xmi/association/multiplicity-max-below-min")
    assertDiagnostics(result, Error -> MaxBelowMin)
  }

  test("a multiplicity with an unbounded minimum asserts no restriction") {
    val result = runCase("xmi/association/multiplicity-unbounded-min")
    assertDiagnostics(result, Error -> UnboundedMinMultiplicity)
  }

  test("an association from a class to itself gives an attribute typed by its own class") {
    assertNoDiagnostics(runCase("xmi/association/self-association"))
  }

  test("a named association replaces an attribute of the same class and name") {
    assertNoDiagnostics(runCase("xmi/association/same-name-as-attribute"))
  }

  // --- Not read (rule 19) --------------------------------------------------------------------

  test("an aggregation is not read") {
    assertNoDiagnostics(runCase("xmi/association/aggregation-not-read"))
  }

  // --- Combining XMI with TSV ----------------------------------------------------------------

  test("a TSV attribute row gives an association a curie without renaming it in the diagram") {
    assertNoDiagnostics(runCase("xmi/association/tsv-gives-curie", examplePrefix))
  }

  // --- The examples of the specification -----------------------------------------------------
  //
  // These read the Enterprise Architect exports under `scenarios` and assert the attributes the
  // specification says their associations become. They assert the model and not an ontology, so
  // they hold whatever ontology IRI and prefix a scenario's expected file is written with.

  /** An attribute typed by a class or an enumeration: in these examples, one made by an association. */
  private case class Association(owner: String, name: String, typeLabel: String, multiplicity: String)

  private def show(cardinality: UMLCardinality): String = cardinality match
    case UMLNonNegativeCardinality(nonNegativeInteger) => nonNegativeInteger.toString
    case UMLInfiniteCardinality(_) => "*"

  private def assertAssociations(expected: Association*): Unit =
    val actual = AttributeCache.getAttributes.flatMap(attribute =>
      attribute.typeOfAttribute.collect {
        case UMLClassType(umlClass) => umlClass.getLabel
        case UMLEnumerationType(enumerationIdentity) => enumerationIdentity.getLabel
      }.map(typeLabel =>
        Association(attribute.attributeIdentity.classIdentity.getLabel, attribute.getLabel, typeLabel,
          s"[${show(attribute.multiplicity.min)},${show(attribute.multiplicity.max)}]")))
    assertEquals(actual, expected.toSet)

  private def translateExample(xmiFile: String): TranslationResult =
    val outputFile = new File("test-output/examples", new File(xmiFile).getName.replace(".xml", ".rdf"))
    outputFile.getParentFile.mkdirs()
    translateFromArguments(Seq("-i", ontologyIRI, "-p", ontologyPrefix, "-o", outputFile.getPath,
      "-m", s"src/test/resources/scenarios/$xmiFile")).getOrElse(fail(s"Could not translate $xmiFile"))

  private val isEmployedBy = Association("Employee", "isEmployedBy", "Employer", "[1,1]")
  private val employs = Association("Employer", "employs", "Employee", "[1,*]")

  test("the initial example gives its four associations and its enumeration") {
    val result = translateExample("simple-employer/input/xmi/sparx/SimpleEmployerWithEnumExample.xml")
    assertNoDiagnostics(result)
    assertAssociations(isEmployedBy, employs,
      Association("Employee", "renumeration", "Renumeration", "[1,1]"),
      Association("Renumeration", "paymentSchedule", "PaymentSchedule", "[1,1]"))
    assertEnumerations(
      Enumeration("PaymentSchedule", values = Set(Value("Daily"), Value("Weekly"), Value("Monthly"))))
  }

  test("the simple employer example gives its two associations") {
    val result = translateExample("simple-employer/input/xmi/sparx/SimpleEmployerExample.xml")
    assertNoDiagnostics(result)
    assertAssociations(isEmployedBy, employs)
  }

  Seq("WithoutGeneralizationSet", "WithGeneralizationSet-CompleteDisjoint",
    "WithGeneralizationSet-CompleteOverlapping", "WithGeneralizationSet-IncompleteDisjoint",
    "WithGeneralizationSet-IncompleteOverlapping").foreach { variant =>
    test(s"the employer generalization scenario gives the same two associations: $variant") {
      val result = translateExample(s"employer-generalization/input/xmi/sparx/$variant.xml")
      assertNoDiagnostics(result)
      assertAssociations(isEmployedBy, employs)
    }
  }
