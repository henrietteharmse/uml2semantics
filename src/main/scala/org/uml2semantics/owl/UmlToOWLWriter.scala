package org.uml2semantics.owl

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddAxiom, IRI, OWLAnnotation, OWLAxiom, OWLClass, OWLOntologyStorageException, OWLSubClassOfAxiom}
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model.parameters.ChangeApplied.SUCCESSFULLY
import org.uml2semantics.model.{UmlClass, UmlClassDiagram}

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try


class UmlToOWLWriter(val umlClassDiagram: UmlClassDiagram):
  private val logger = Logger(this.getClass)
  private val manager = OWLManager.createOWLOntologyManager
  logger.trace(s"umlClassDiagram.owlOntologyFile=${umlClassDiagram.owlOntologyFile}")
  private val ontology = manager.createOntology(IRI.create(umlClassDiagram.ontologyIRI.ontologyIRI))
  private val dataFactory = manager.getOWLDataFactory

  private def createDefinitionAnnotation(owlClass: OWLClass, definition: String): ChangeApplied =
    logger.trace(s"definition=$definition")
    val owlClassDefinitionAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlClass.getIRI,
      dataFactory.getOWLAnnotation(dataFactory.getRDFSComment(), dataFactory.getOWLLiteral(definition)))
    manager.addAxiom(ontology, owlClassDefinitionAnnotationAxiom)

  private def createLabelAnnotation(owlClass: OWLClass, label: String): ChangeApplied =
    val owlClassLabelAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlClass.getIRI,
      dataFactory.getOWLAnnotation(dataFactory.getRDFSLabel(), dataFactory.getOWLLiteral(label)))
    manager.addAxiom(ontology, owlClassLabelAnnotationAxiom)

  private def createAndAnnotateOWLClass(umlClass: UmlClass): OWLClass =
    val owlClass = dataFactory.getOWLClass(umlClass.classIdentity.classIRI.iri)
    createDefinitionAnnotation(owlClass, umlClass.classDefinition.definition)
    createLabelAnnotation(owlClass, umlClass.classIdentity.classIRI.classId.id)
    owlClass

  private def generateOWLForClasses: ListBuffer[String] =
    logger.trace("generateOWLForClasses")
    val errorMessages = new ListBuffer[String]()
    umlClassDiagram.umlClasses.mapOfUmlClasses.keySet.foreach(id => {
      val umlClassOption = umlClassDiagram.umlClasses.mapOfUmlClasses.get(id)
      if umlClassOption.isDefined then
        val umlClass = umlClassOption.get
        val owlClass = createAndAnnotateOWLClass(umlClass)

        if umlClass.classParentIds.setOfParentIds.isEmpty then
          logger.trace("ParentIds is EMPTY")
          val addAxiomChangeApplied =  manager.addAxiom(
            ontology, dataFactory.getOWLSubClassOfAxiom(owlClass, dataFactory.getOWLThing))
          if addAxiomChangeApplied != SUCCESSFULLY then
            errorMessages.addOne(s"Could not add axiom ${owlClass.getIRI} subClassOf owl:Thing")
        else
          umlClass.classParentIds.setOfParentIds.foreach(parentClassId => {
            logger.trace(s"parentId=${parentClassId.id}")
            val addAxiomChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(
              owlClass, dataFactory.getOWLClass(umlClassDiagram.ontologyPrefix.ontologyPrefix + parentClassId.id)))
            if addAxiomChangeApplied != SUCCESSFULLY then
              errorMessages.addOne(s"Could not add axiom ${owlClass.getIRI} subClassOf ${parentClassId.id}")
          })
    })
    errorMessages
  end generateOWLForClasses

  def generateOWL: Either[String, ListBuffer[String]] =
    logger.trace("generateOWL")
    val errorMessages = new ListBuffer[String]()
    errorMessages.appendAll(generateOWLForClasses)
    try
      manager.saveOntology(ontology, new RDFXMLDocumentFormat(), IRI.create(umlClassDiagram.owlOntologyFile))
      Right(errorMessages)
    catch
      case e: OWLOntologyStorageException => Left(e.getMessage)
  end generateOWL

end UmlToOWLWriter
