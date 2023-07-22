package org.uml2semantics.owl

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model.parameters.ChangeApplied.{NO_OPERATION, SUCCESSFULLY}
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.vocab.OWL2Datatype
import org.uml2semantics.model.XMLDataType.getIRI
import org.uml2semantics.model.*
import org.uml2semantics.inline.Code

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Seq}
import scala.util.Try


class UmlToOWLWriter(val umlClassDiagram: UmlClassDiagram):
  private val logger = Logger(this.getClass)
  private val manager = OWLManager.createOWLOntologyManager
  private val ontology = manager.createOntology(IRI.create(umlClassDiagram.ontologyIRI.ontologyIRI))
  private val dataFactory = manager.getOWLDataFactory

  private def createDefinitionAnnotation(owlNamedObject: OWLNamedObject, definition: String,
                                         errorMessages: mutable.Seq[String]): Unit =
    logger.debug(s"createDefinitionAnnotation: owlNamedObject=$owlNamedObject, definition=$definition, errorMessages=$errorMessages ${Code.source}")
    if definition.nonEmpty then
      val owlClassDefinitionAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
        dataFactory.getOWLAnnotation(dataFactory.getRDFSComment(), dataFactory.getOWLLiteral(definition)))
      if manager.addAxiom(ontology, owlClassDefinitionAnnotationAxiom) != SUCCESSFULLY then
        errorMessages :+ s"Could not add definition=$definition for ${owlNamedObject.getIRI}"


  private def createLabelAnnotation(owlNamedObject: OWLNamedObject, label: String, errorMessages: mutable.Seq[String]): Unit =
    logger.debug(s"createLabelAnnotation: owlNamedObject=$owlNamedObject, label=$label, errorMessages=$errorMessages ${Code.source}")
    if label.nonEmpty then
      val owlLabelAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
        dataFactory.getOWLAnnotation(dataFactory.getRDFSLabel(), dataFactory.getOWLLiteral(label)))
      if manager.addAxiom(ontology, owlLabelAnnotationAxiom) != SUCCESSFULLY then
        errorMessages :+ s"Could not add label=$label for ${owlNamedObject.getIRI}"


  private def createAndAnnotateOWLClass(umlClass: UmlClass, errorMessages: mutable.Seq[String]): OWLClass =
    logger.debug(s"createAndAnnotateOWLClass: umlClass=$umlClass, errorMessages=$errorMessages ${Code.source}")
    val owlClass = dataFactory.getOWLClass(umlClass.classIdentity.classIRI.iri)
    createDefinitionAnnotation(owlClass, umlClass.classDefinition.definition, errorMessages)
    createLabelAnnotation(owlClass, umlClass.classIdentity.classLabel, errorMessages)
    owlClass


  /**
   * Creates OWL a data- or object property depending on whether the type of the UML class attribute is a UML primitive
   * or UML class.
   *
   * @param umlClassAttribute
   * @param errorMessages
   * @return A sequence of error messages if any errors were encountered. If no errors occurred, an empty sequence is returned.
   */
  private def createAndAnnotateOWLProperty(umlClassAttribute: UmlClassAttribute, errorMessages: mutable.Seq[String]): OWLProperty =
    logger.debug(s"createAndAnnotateOWLProperty: umlClassAttribute=$umlClassAttribute, errorMessages=$errorMessages ${Code.source}")
    val umlClassIdentity: UmlClassIdentity = UmlClassIdentity.findClassId(umlClassAttribute.attributeIdentity.classId.id).get
    val domain = dataFactory.getOWLClass(IRI.create(umlClassIdentity.classIRI.iri))
    var owlProperty: OWLProperty = null
    umlClassAttribute.typeOfAttribute match
      case UmlXMLDataType(attributeType) =>
        owlProperty = createOWLDataProperty(umlClassAttribute, errorMessages, domain, attributeType)
      case UmlClassIdentityType(attributeType) =>
        owlProperty = createOWLObjectProperty(umlClassAttribute, errorMessages, domain, attributeType)

    createLabelAnnotation(owlProperty, umlClassAttribute.attributeIdentity.attributeLabel, errorMessages)
    createDefinitionAnnotation(owlProperty, umlClassAttribute.definition.definition, errorMessages)
    owlProperty


  /**
   * Creates an OWL object property with domain, range and cardinalities corresponding to the semantics of the UML class
   * attribute.
   *
   * @param umlClassAttribute
   * @param errorMessages
   * @param umlClassIdentity
   * @param domain
   * @return The OWL object property that was created.
   */
  private def createOWLObjectProperty(umlClassAttribute: UmlClassAttribute, errorMessages: mutable.Seq[String],
                                      domain: OWLClass, umlClassIdentity: UmlClassIdentity): OWLObjectProperty =
    logger.debug(s"createOWLObjectProperty: umlClassAttribute=$umlClassAttribute, errorMessages = $errorMessages, " +
      s"domain=$domain, umlClassIdentity=$umlClassIdentity ${Code.source}")
    val objectProperty = dataFactory.getOWLObjectProperty(umlClassAttribute.attributeIdentity.attributeIRI.iri)
    val domainChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, domain))
    if domainChangeApplied != SUCCESSFULLY then
      errorMessages :+ s"Domain axiom for object property ${objectProperty.getIRI} could not be added for " +
        s"${umlClassAttribute.attributeIdentity.attributeIRI.iri}"
    val rangeChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyRangeAxiom(objectProperty,
      dataFactory.getOWLClass(umlClassIdentity.classIRI.iri)))
    if rangeChangeApplied != SUCCESSFULLY then
      errorMessages :+ s"Range axiom for data property ${objectProperty.getIRI} could not be added for " +
        s"${umlClassAttribute.attributeIdentity.attributeIRI.iri}"
    umlClassAttribute.multiplicity match
      case UmlMultiplicity(UmlNonNegativeCardinality(minCardinality), UmlNonNegativeCardinality(maxCardinality)) =>
        logger.trace(s"minCardinality=$minCardinality ${Code.source}")
        val axiom =
          if minCardinality == maxCardinality && minCardinality != 0 then
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectExactCardinality(maxCardinality, objectProperty,
              dataFactory.getOWLClass(umlClassIdentity.classIRI.iri)))
          else if minCardinality > 0 then
            dataFactory.getOWLSubClassOfAxiom(domain,
              dataFactory.getOWLObjectIntersectionOf(
                dataFactory.getOWLObjectMinCardinality(minCardinality, objectProperty,
                  dataFactory.getOWLClass(umlClassIdentity.classIRI.iri)),
                dataFactory.getOWLObjectMaxCardinality(maxCardinality, objectProperty,
                  dataFactory.getOWLClass(umlClassIdentity.classIRI.iri))))
          else
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectMaxCardinality(maxCardinality, objectProperty,
              dataFactory.getOWLClass(umlClassIdentity.classIRI.iri)))
        if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
          errorMessages :+ s"Could not add subclass axiom representing cardinalities [$minCardinality, $maxCardinality] for " +
            s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UmlMultiplicity(UmlNonNegativeCardinality(minCardinality), UmlInfiniteCardinality(_)) =>
        if minCardinality > 0 then
          val axiom = dataFactory.getOWLSubClassOfAxiom(domain,
            dataFactory.getOWLObjectMinCardinality(minCardinality, objectProperty,
              dataFactory.getOWLClass(umlClassIdentity.classIRI.iri)))
          if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
            errorMessages :+ s"Could not add subclass axiom representing cardinalities for [$minCardinality, *]" +
              s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UmlMultiplicity(UmlInfiniteCardinality(_), UmlNonNegativeCardinality(maxCardinality)) =>
        errorMessages :+ s"Multiplicity error found with multiplicity = [*, $maxCardinality] for attribute = " +
          s"${umlClassAttribute.attributeIdentity.attributeLabel}."
      case UmlMultiplicity(UmlInfiniteCardinality(_), UmlInfiniteCardinality(_)) =>
    objectProperty

  /**
   * Creates an OWL data property with domain, range and cardinalities corresponding to the semantics of the UML class
   * attribute.
   *
   * @param umlClassAttribute
   * @param errorMessages
   * @param domain
   * @param attributeType
   * @return The OWL data property that was created.
   */
  private def createOWLDataProperty(umlClassAttribute: UmlClassAttribute, errorMessages: mutable.Seq[String],
                                    domain: OWLClass, attributeType: XMLDataType): OWLDataProperty =
    logger.debug(s"createOWLDataProperty: umlClassAttribute=$umlClassAttribute, errorMessages=$errorMessages, " +
      s"domain=$domain, attributeType=$attributeType ${Code.source}")
    val dataProperty = dataFactory.getOWLDataProperty(umlClassAttribute.attributeIdentity.attributeIRI.iri)
    val domainChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLDataPropertyDomainAxiom(dataProperty, domain))
    if domainChangeApplied != SUCCESSFULLY then
      errorMessages :+ s"Domain axiom for data property ${dataProperty.getIRI} could not be added for " +
        s"${umlClassAttribute.attributeIdentity.attributeIRI.iri}"
    val dataType: OWL2Datatype = OWL2Datatype.getDatatype(IRI.create(getIRI(attributeType)))
    val rangeChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLDataPropertyRangeAxiom(dataProperty, dataType))
    if rangeChangeApplied != SUCCESSFULLY then
      errorMessages :+ s"Range axiom for data property ${dataProperty.getIRI} could not be added for " +
        s"${umlClassAttribute.attributeIdentity.attributeIRI.iri}"
    umlClassAttribute.multiplicity match
      case UmlMultiplicity(UmlNonNegativeCardinality(minCardinality), UmlNonNegativeCardinality(maxCardinality)) =>
        val axiom =
          if minCardinality == maxCardinality && minCardinality != 0 then
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLDataExactCardinality(maxCardinality, dataProperty, dataType))
          else if minCardinality > 0 then
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectIntersectionOf(
              dataFactory.getOWLDataMinCardinality(minCardinality, dataProperty, dataType),
              dataFactory.getOWLDataMaxCardinality(maxCardinality, dataProperty, dataType)))
          else
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLDataMaxCardinality(maxCardinality, dataProperty, dataType))
        if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
          errorMessages :+ s"Could not add subclass axiom representing cardinalities [$minCardinality, $maxCardinality] for " +
            s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UmlMultiplicity(UmlNonNegativeCardinality(minCardinality), UmlInfiniteCardinality(_)) =>
        if minCardinality > 0 then
          val axiom = dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLDataMinCardinality(minCardinality, dataProperty, dataType))
          if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
            errorMessages :+ s"Could not add subclass axiom representing cardinalities for [$minCardinality, *]" +
              s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UmlMultiplicity(UmlInfiniteCardinality(_), UmlNonNegativeCardinality(maxCardinality)) =>
        errorMessages :+ s"Multiplicity error found with multiplicity = [*, $maxCardinality] for attribute = " +
          s"${umlClassAttribute.attributeIdentity.attributeLabel}."
      case UmlMultiplicity(UmlInfiniteCardinality(_), UmlInfiniteCardinality(_)) =>
    dataProperty

  private def processUmlClasses: mutable.Seq[String] =
    logger.info("processUmlClasses: Start")
    val errorMessages: mutable.Seq[String] = new ArrayBuffer[String]()
    umlClassDiagram.umlClasses.mapOfUmlClasses.keySet.foreach(id => {
      val umlClassOption = umlClassDiagram.umlClasses.mapOfUmlClasses.get(id)
      if umlClassOption.isDefined then
        val umlClass = umlClassOption.get
        val owlClass = createAndAnnotateOWLClass(umlClass, errorMessages)
        if umlClass.classParentIds.setOfParentIds.isEmpty then
          logger.trace("ParentIds is EMPTY")
          if manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass, dataFactory.getOWLThing)) != SUCCESSFULLY then
            errorMessages :+ s"Could not add axiom ${owlClass.getIRI} subClassOf owl:Thing"
        else
          umlClass.classParentIds.setOfParentIds.foreach(parentClassId => {
            logger.trace(s"parentId=${parentClassId.id}")
            val parentClassIdentityOption = UmlClassIdentity.findClassId(parentClassId.id)
            if parentClassIdentityOption.isDefined then
              if manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass,
                dataFactory.getOWLClass(parentClassIdentityOption.get.classIRI.iri))) != SUCCESSFULLY then
                  errorMessages :+ s"Could not add axiom ${owlClass.getIRI} subClassOf ${parentClassId.id}"
            })
    })
    logger.info("processUmlClasses: Done")
    errorMessages
  end processUmlClasses

  private def processClassAttributes: mutable.Seq[String] =
    logger.info("processClassAttributes: Start")
    val errorMessages: ArrayBuffer[String] = new ArrayBuffer[String]()
    umlClassDiagram.umlClassAttributes.mapOfUmlClassAttributes.keySet.foreach(id => {
      val umlClassAttributeOption = umlClassDiagram.umlClassAttributes.mapOfUmlClassAttributes.get(id)
      if umlClassAttributeOption.isDefined then
        val umlClassAttribute = umlClassAttributeOption.get
        val owlProperty = createAndAnnotateOWLProperty(umlClassAttribute, errorMessages)
    })
    logger.info("processClassAttributes: Done")
    errorMessages
  end processClassAttributes


  def generateOWL: Either[String, ListBuffer[String]] =
    logger.info("generateOWL: Start")
    val errorMessages = new ListBuffer[String]()
    errorMessages.appendAll(processUmlClasses)
    errorMessages.appendAll(processClassAttributes)
    try
      manager.saveOntology(ontology, new RDFXMLDocumentFormat(), IRI.create(umlClassDiagram.owlOntologyFile))
      logger.info("generateOWL: Done")
      Right(errorMessages)
    catch
      case e: OWLOntologyStorageException =>
        logger.info("generateOWL: Done")
        Left(e.getMessage)
  end generateOWL

end UmlToOWLWriter
