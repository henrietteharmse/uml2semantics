package org.uml2semantics

import munit.FunSuite
import org.uml2semantics.model.PrefixNamespace
import org.uml2semantics.model.cache.{AttributeBuilderCache, AttributeIdentityBuilderCache, ClassBuilderCache, ClassIdentityBuilderCache}
import org.uml2semantics.utility.OntologyAssertions

import java.io.File

class SimpleEmployerIntegrationTest extends FunSuite:

  override def beforeEach(context: BeforeEach): Unit =
    ClassIdentityBuilderCache.clear()
    ClassBuilderCache.clear()
    AttributeIdentityBuilderCache.clear()
    AttributeBuilderCache.clear()
    PrefixNamespace.clear()

  test("TSV-to-OWL pipeline produces semantically equivalent ontology for SimpleEmployerExample") {
    val outputDir = new File("test-output/simple-employer")
    outputDir.mkdirs()

    val outputFile = new File(outputDir, "SimpleEmployerExample.rdf")
    val expectedFile = new File("src/test/resources/scenarios/simple-employer/expected/SimpleEmployerExample.rdf")

    uml2owl(
      "-c", "src/test/resources/scenarios/simple-employer/input/tsv/Classes.tsv",
      "-a", "src/test/resources/scenarios/simple-employer/input/tsv/Attributes.tsv",
      "-p", "emp:http://uml2semantics.org/examples/employer#",
      "-i", "http://uml2semantics.org/examples/employer/v.0.2",
      "-o", outputFile.getPath
    )

    assert(outputFile.exists(), s"Generated file does not exist: ${outputFile.getPath}")

    val generatedOntology = OntologyAssertions.loadOntology(outputFile)
    val expectedOntology = OntologyAssertions.loadOntology(expectedFile)

    OntologyAssertions.assertOntologiesEqual(generatedOntology, expectedOntology)
  }
