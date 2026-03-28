package org.uml2semantics.utility

import munit.Assertions
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology

import java.io.{File, FileInputStream}
import scala.jdk.CollectionConverters.*

object OntologyAssertions extends Assertions:

  def loadOntology(file: File): OWLOntology =
    val manager = OWLManager.createOWLOntologyManager()
    manager.loadOntologyFromOntologyDocument(new FileInputStream(file))

  def assertOntologiesEqual(generated: OWLOntology, expected: OWLOntology): Unit =
    assertEquals(
      generated.getOntologyID.getOntologyIRI,
      expected.getOntologyID.getOntologyIRI,
      "Ontology IRIs do not match"
    )

    assertSetsEqual(
      generated.getClassesInSignature.asScala.toSet,
      expected.getClassesInSignature.asScala.toSet,
      "OWL classes"
    )

    assertSetsEqual(
      generated.getObjectPropertiesInSignature.asScala.toSet,
      expected.getObjectPropertiesInSignature.asScala.toSet,
      "object properties"
    )

    assertSetsEqual(
      generated.getDataPropertiesInSignature.asScala.toSet,
      expected.getDataPropertiesInSignature.asScala.toSet,
      "data properties"
    )

    assertSetsEqual(
      generated.getAnnotationPropertiesInSignature.asScala.toSet,
      expected.getAnnotationPropertiesInSignature.asScala.toSet,
      "annotation properties"
    )

    assertSetsEqual(
      generated.getAxioms.asScala.toSet,
      expected.getAxioms.asScala.toSet,
      "axioms"
    )

  private def assertSetsEqual[A](generated: Set[A], expected: Set[A], label: String): Unit =
    val missing = expected -- generated
    val extra = generated -- expected
    assert(missing.isEmpty, s"Missing $label:\n${missing.mkString("\n")}")
    assert(extra.isEmpty, s"Extra $label:\n${extra.mkString("\n")}")
