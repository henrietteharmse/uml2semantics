package org.uml2owl

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddAxiom, IRI, OWLAnnotation, OWLAxiom, OWLClass, OWLSubClassOfAxiom}
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat

import java.io.File
import scala.collection.mutable

val rdfsPrefix = "http://www.w3.org/2000/01/rdf-schema#"
val rdfsComment = rdfsPrefix + "comment"
val rdfsLabel = rdfsPrefix + "label"
val rdfsCommentIRI = IRI.create(rdfsComment)
val rdfsLabelIRI = IRI.create(rdfsLabel)


class OWLWriter(val umlClassDiagram: UmlClassDiagram):
  private val logger = Logger(this.getClass)
  private val manager = OWLManager.createOWLOntologyManager
  logger.trace(s"umlClassDiagram.owlOntologyFile=${umlClassDiagram.owlOntologyFile}")
  private val ontology = manager.createOntology(IRI.create(umlClassDiagram.ontologyIRI))
  private val dataFactory = manager.getOWLDataFactory

  private def createDefinitionAnnotation(owlClass: OWLClass, definition: String): Unit =
    val definitionAnnotation = dataFactory.getOWLAnnotation(
      dataFactory.getOWLAnnotationProperty(rdfsCommentIRI),
      dataFactory.getOWLLiteral(definition))
    val owlClassDefinitionAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(
      owlClass.getIRI, definitionAnnotation)
    manager.addAxiom(ontology, owlClassDefinitionAnnotationAxiom)

  private def createLabelAnnotation(owlClass: OWLClass, label: String): Unit =
    val definitionAnnotation = dataFactory.getOWLAnnotation(
      dataFactory.getOWLAnnotationProperty(rdfsLabelIRI),
      dataFactory.getOWLLiteral(label))
    val owlClassLabelAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(
      owlClass.getIRI, definitionAnnotation)
    manager.addAxiom(ontology, owlClassLabelAnnotationAxiom)

  private def createAndAnnotateOWLClass(umlClass: UmlClass): OWLClass =
    val owlClass = dataFactory.getOWLClass(umlClass.id.asIRI(umlClassDiagram.ontologyIRI))
    if umlClass.definition.nonEmpty then createDefinitionAnnotation(owlClass, umlClass.definition.get)
    if umlClass.id.name.nonEmpty then createLabelAnnotation(owlClass, umlClass.id.name)

    owlClass
  end createAndAnnotateOWLClass


  def generateOWL: Unit =
    umlClassDiagram.umlClasses.keySet.foreach(id => {
      val umlClass = umlClassDiagram.umlClasses.get(id)
      val owlClass = createAndAnnotateOWLClass(umlClass.get)

      if umlClass.get.parentIds.isEmpty then
        logger.trace("ParentIds is EMPTY")
        manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass, dataFactory.getOWLThing))
      else
        umlClass.get.parentIds.foreach(parentClassId => {
          if parentClassId.isEmpty then
            manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass, dataFactory.getOWLThing))
          else
            logger.trace(s"parentId=$parentClassId")
            manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass,
              dataFactory.getOWLClass(umlClassDiagram.ontologyIRI + "#" + parentClassId)))
      })
    })
    manager.saveOntology(ontology, new RDFXMLDocumentFormat(), IRI.create(umlClassDiagram.owlOntologyFile))
  end generateOWL

end OWLWriter
