package org.uml2semantics.cases

import munit.Assertions
import org.uml2semantics.model.UMLAttributeType
import org.uml2semantics.model.cache.{AttributeCache, EnumerationCache}

/**
 * Model assertions about enumerations, shared by the enumeration groups of every input format. An
 * enumeration means the same thing whichever input it came from, so the suites describe what they
 * expect in the same terms.
 */
trait EnumerationAssertions extends Assertions:

  /** What a case expects of one value. A blank half or definition means "not given". */
  protected case class Value(name: String = "", curie: String = "", definition: String = "")

  /** What a case expects of one enumeration. A blank half or definition means "not given". */
  protected case class Enumeration(name: String = "", curie: String = "", definition: String = "",
                                   values: Set[Value] = Set.empty)

  protected def assertEnumerations(expected: Enumeration*): Unit =
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

  protected def typeOfAttribute(label: String): Option[UMLAttributeType] =
    AttributeCache.getAttributes.find(_.getLabel == label).flatMap(_.typeOfAttribute)
