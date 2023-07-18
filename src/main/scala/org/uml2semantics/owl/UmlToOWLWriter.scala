package org.uml2semantics.owl

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddAxiom, IRI, OWLAnnotation, OWLAxiom, OWLClass, OWLDataProperty, OWLNamedObject, OWLObjectProperty, OWLOntologyStorageException, OWLProperty, OWLSubClassOfAxiom, OWLDatatype}
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model.parameters.ChangeApplied.SUCCESSFULLY
import org.uml2semantics.model.XMLDataType.getIRI
import org.uml2semantics.model.{UmlClass, UmlClassAttribute, UmlClassAttributeIRI, UmlClassAttributeIdentity, UmlClassDiagram, UmlClassIdentity, UmlClassIdentityType, UmlXMLDataType, XMLDataType}

import scala.collection.mutable.{ArrayBuffer, ListBuffer, Seq}
import java.io.File
import scala.util.Try


class UmlToOWLWriter(val umlClassDiagram: UmlClassDiagram):
  private val logger = Logger(this.getClass)
  private val manager = OWLManager.createOWLOntologyManager
  logger.trace(s"umlClassDiagram.owlOntologyFile=${umlClassDiagram.owlOntologyFile}")
  private val ontology = manager.createOntology(IRI.create(umlClassDiagram.ontologyIRI.ontologyIRI))
  private val dataFactory = manager.getOWLDataFactory

  private def createDefinitionAnnotation(owlNamedObject: OWLNamedObject, definition: String): ChangeApplied =
    logger.trace(s"definition=$definition")
    val owlClassDefinitionAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
      dataFactory.getOWLAnnotation(dataFactory.getRDFSComment(), dataFactory.getOWLLiteral(definition)))
    manager.addAxiom(ontology, owlClassDefinitionAnnotationAxiom)

  private def createLabelAnnotation(owlNamedObject: OWLNamedObject, label: String): ChangeApplied =
    val owlLabelAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
      dataFactory.getOWLAnnotation(dataFactory.getRDFSLabel(), dataFactory.getOWLLiteral(label)))
    manager.addAxiom(ontology, owlLabelAnnotationAxiom)

  private def createAndAnnotateOWLClass(umlClass: UmlClass): OWLClass =
    val owlClass = dataFactory.getOWLClass(umlClass.classIdentity.classIRI.iri)
    createDefinitionAnnotation(owlClass, umlClass.classDefinition.definition)
    createLabelAnnotation(owlClass, umlClass.classIdentity.classLabel)
    owlClass



  private def createAndAnnotateOWLProperty(umlClassAttribute: UmlClassAttribute): OWLProperty =
    val umlClassIdentity: UmlClassIdentity = UmlClassIdentity.findClassId(umlClassAttribute.attributeIdentity.classId.id).get
    val domain = dataFactory.getOWLClass(IRI.create(umlClassIdentity.classIRI.iri))
    var owlProperty: OWLProperty = null
    umlClassAttribute.typeOfAttribute match
      case UmlXMLDataType(attributeType) =>
        val dataProperty = dataFactory.getOWLDataProperty(umlClassAttribute.attributeIdentity.attributeIRI.iri)
        manager.addAxiom(ontology, dataFactory.getOWLDataPropertyDomainAxiom(dataProperty, domain))
        val dataType: OWLDatatype = dataFactory.getOWLDatatype(getIRI(attributeType))
        manager.addAxiom(ontology, dataFactory.getOWLDataPropertyRangeAxiom(dataProperty, dataType))
//        umlClassAttribute.multiplicity.min match
//          case
        owlProperty = dataProperty
      case UmlClassIdentityType(attributeType) =>
        val objectProperty = dataFactory.getOWLObjectProperty(umlClassAttribute.attributeIdentity.attributeIRI.iri)
        manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, domain))
        manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyRangeAxiom(objectProperty,
          dataFactory.getOWLClass(umlClassIdentity.classIRI.iri)))
        owlProperty = objectProperty

    createLabelAnnotation(owlProperty, umlClassAttribute.attributeIdentity.attributeLabel)
    createDefinitionAnnotation(owlProperty, umlClassAttribute.definition.definition)
    owlProperty


  private def processUmlClasses: Seq[String] =
    logger.trace("processUmlClasses")
    val errorMessages: ArrayBuffer[String] = new ArrayBuffer[String]()
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
            val parentClassIdentityOption = UmlClassIdentity.findClassId(parentClassId.id)
            if parentClassIdentityOption.isDefined then
              val addAxiomChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(
                owlClass, dataFactory.getOWLClass(parentClassIdentityOption.get.classIRI.iri)))
              if addAxiomChangeApplied != SUCCESSFULLY then
                errorMessages.addOne(s"Could not add axiom ${owlClass.getIRI} subClassOf ${parentClassId.id}")
          })
    })
    errorMessages
  end processUmlClasses

  private def processClassAttributes: Seq[String] =
    logger.trace("processUmlClasses")
    val errorMessages: ArrayBuffer[String] = new ArrayBuffer[String]()
    umlClassDiagram.umlClassAttributes.mapOfUmlClassAttributes.keySet.foreach(id => {
      val umlClassAttributeOption = umlClassDiagram.umlClassAttributes.mapOfUmlClassAttributes.get(id)
      if umlClassAttributeOption.isDefined then
        val umlClassAttribute = umlClassAttributeOption.get
        val owlProperty = createAndAnnotateOWLProperty(umlClassAttribute)
    })
    errorMessages
  end processClassAttributes


  def generateOWL: Either[String, ListBuffer[String]] =
    logger.trace("generateOWL")
    val errorMessages = new ListBuffer[String]()
    errorMessages.appendAll(processUmlClasses)
    try
      manager.saveOntology(ontology, new RDFXMLDocumentFormat(), IRI.create(umlClassDiagram.owlOntologyFile))
      Right(errorMessages)
    catch
      case e: OWLOntologyStorageException => Left(e.getMessage)
  end generateOWL

end UmlToOWLWriter
