package org.uml2owl

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddAxiom, IRI, OWLAnnotation, OWLAxiom, OWLClass, OWLOntologyStorageException, OWLSubClassOfAxiom}
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model.parameters.ChangeApplied.SUCCESSFULLY

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

case class ErrorResponse(errorMsg:String, task: String)

class OWLWriter(val umlClassDiagram: UmlClassDiagram):
  private val logger = Logger(this.getClass)
  private val manager = OWLManager.createOWLOntologyManager
  logger.trace(s"umlClassDiagram.owlOntologyFile=${umlClassDiagram.owlOntologyFile}")
  private val ontology = manager.createOntology(IRI.create(umlClassDiagram.ontologyIRI))
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
    val owlClass = dataFactory.getOWLClass(umlClass.id.asIRI(umlClassDiagram.ontologyIRI))
    if umlClass.definitionIsNonEmpty then createDefinitionAnnotation(owlClass, umlClass.definition.get)
    if umlClass.id.name.nonEmpty then createLabelAnnotation(owlClass, umlClass.id.name)
    owlClass

  end createAndAnnotateOWLClass

  private def generateOWLForClasses(): ListBuffer[String] =
    val errorMessages = new ListBuffer[String]()
    umlClassDiagram.umlClasses.keySet.foreach(id => {
      val umlClass = umlClassDiagram.umlClasses.get(id)
      val owlClass = createAndAnnotateOWLClass(umlClass.get)

      if umlClass.get.parentIds.isEmpty then
        logger.trace("ParentIds is EMPTY")
        if manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass, dataFactory.getOWLThing)) != SUCCESSFULLY
        then errorMessages.addOne(s"Could not add axiom ${owlClass.getIRI} subClassOf owl:Thing")
        else
          umlClass.get.parentIds.foreach(parentClassId => {
            if parentClassId.isEmpty then
              if manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass, dataFactory.getOWLThing)) != SUCCESSFULLY
              then errorMessages.addOne(s"Could not add axiom ${owlClass.getIRI} subClassOf owl:Thing")
              else
                logger.trace(s"parentId=$parentClassId")
              if manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass,
                dataFactory.getOWLClass(umlClassDiagram.ontologyIRI + "#" + parentClassId))) != SUCCESSFULLY
              then errorMessages.addOne(s"Could not add axiom ${owlClass.getIRI} subClassOf $parentClassId")
          })
    })
    errorMessages
  end generateOWLForClasses




  def generateOWL: Either[String, ListBuffer[String]] =
    val errorMessages = new ListBuffer[String]()
    errorMessages.appendAll(generateOWLForClasses())
    try
      manager.saveOntology(ontology, new RDFXMLDocumentFormat(), IRI.create(umlClassDiagram.owlOntologyFile))
      Right(errorMessages)
    catch
      case e: OWLOntologyStorageException => Left(e.getMessage)
  end generateOWL



end OWLWriter
