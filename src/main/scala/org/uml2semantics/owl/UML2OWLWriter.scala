package org.uml2semantics.owl

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model.parameters.ChangeApplied.{NO_OPERATION, SUCCESSFULLY}
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.vocab.OWL2Datatype
import org.uml2semantics.model.SupportedDataType.getIRI
import org.uml2semantics.model.*
import org.uml2semantics.inline.Code

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Seq}
import scala.util.Try

import scala.jdk.CollectionConverters.*

class UML2OWLWriter(val umlClassDiagram: UMLClassDiagram):
  private val logger = Logger(this.getClass)
  private val manager = OWLManager.createOWLOntologyManager
  private val ontology = manager.createOntology(IRI.create(umlClassDiagram.ontologyIRI.ontologyIRI))
  private val dataFactory = manager.getOWLDataFactory

  private def createDefinitionAnnotation(owlNamedObject: OWLNamedObject,
                                         definition: String,
                                         errorMessages: mutable.Seq[String]): Unit =
    logger.debug(s"createDefinitionAnnotation: owlNamedObject=$owlNamedObject, definition=$definition, errorMessages=$errorMessages ${Code.source}")
    if definition.nonEmpty then
      val owlClassDefinitionAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
        dataFactory.getOWLAnnotation(dataFactory.getRDFSComment(), dataFactory.getOWLLiteral(definition)))
      if manager.addAxiom(ontology, owlClassDefinitionAnnotationAxiom) != SUCCESSFULLY then
        errorMessages :+ s"Could not add definition=$definition for ${owlNamedObject.getIRI}"


  private def createLabelAnnotation(owlNamedObject: OWLNamedObject,
                                    label: String,
                                    errorMessages: mutable.Seq[String]): Unit =
    logger.debug(s"createLabelAnnotation: owlNamedObject=$owlNamedObject, label=$label, errorMessages=$errorMessages ${Code.source}")
    if label.nonEmpty then
      val owlLabelAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
        dataFactory.getOWLAnnotation(dataFactory.getRDFSLabel(), dataFactory.getOWLLiteral(label)))
      if manager.addAxiom(ontology, owlLabelAnnotationAxiom) != SUCCESSFULLY then
        errorMessages :+ s"Could not add label=$label for ${owlNamedObject.getIRI}"


  private def createAndAnnotateOWLClass(umlClass: UMLClass,
                                        errorMessages: mutable.Seq[String],
                                        hasParent: Boolean = false): OWLClass =
    logger.debug(s"createAndAnnotateOWLClass: umlClass=$umlClass, errorMessages=$errorMessages ${Code.source}")
    val owlClass = dataFactory.getOWLClass(umlClass.classIdentity.classIRI.iri)
    if !hasParent then
      if manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass, dataFactory.getOWLThing)) != SUCCESSFULLY then
        errorMessages :+ s"Could not add axiom ${owlClass.getIRI} subClassOf owl:Thing"
    createDefinitionAnnotation(owlClass, umlClass.classDefinition.definition, errorMessages)
    createLabelAnnotation(owlClass, umlClass.classIdentity.classLabel, errorMessages)
    owlClass

  private def createAndAnnotateOWLClass(umlEnumeration: UMLEnumeration,
                                        errorMessages: mutable.Seq[String]): OWLClass =
    logger.debug(s"createAndAnnotateOWLEnumeration: umlEnumeration=$umlEnumeration, errorMessages=$errorMessages ${Code.source}")
    val owlClass = dataFactory.getOWLClass(umlEnumeration.enumerationIdentity.enumerationIRI.iri)
    if manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass, dataFactory.getOWLThing)) != SUCCESSFULLY then
      errorMessages :+ s"Could not add axiom ${owlClass.getIRI} subClassOf owl:Thing"
    createDefinitionAnnotation(owlClass, umlEnumeration.definition.definition, errorMessages)
    createLabelAnnotation(owlClass, umlEnumeration.enumerationIdentity.enumerationLabel, errorMessages)
    owlClass

  /**
   * Creates OWL a data- or object property depending on whether the type of the UML class attribute is a UML primitive
   * or UML class.
   *
   * @param umlClassAttribute
   * @param errorMessages
   * @return A sequence of error messages if any errors were encountered. If no errors occurred, an empty sequence is returned.
   */
  private def createAndAnnotateOWLProperty(umlClassAttribute: UMLClassAttribute,
                                           errorMessages: mutable.Seq[String]): OWLProperty =
    logger.debug(s"createAndAnnotateOWLProperty: umlClassAttribute=$umlClassAttribute, errorMessages=$errorMessages ${Code.source}")
    val umlClassIdentity: UMLClassIdentity = UMLClassIdentity.findClassNamedElement(umlClassAttribute.attributeIdentity.classNamedElement.getName).get
    val domain = dataFactory.getOWLClass(IRI.create(umlClassIdentity.classIRI.iri))
    var owlProperty: OWLProperty = null
    umlClassAttribute.typeOfAttribute match
      case UMLXMLDataType(attributeType) =>
        owlProperty = createOWLDataProperty(umlClassAttribute, errorMessages, domain, attributeType)
      case UMLClassIdentityType(attributeType) =>
        owlProperty = createOWLObjectProperty(umlClassAttribute, errorMessages, domain, attributeType)
      case UMLEnumerationIdentityType(attributeType) =>
        owlProperty = createOWLObjectProperty(umlClassAttribute, errorMessages, domain, attributeType)
      case CurieBasedUMLClassAttributeType(attributeType) =>
        owlProperty = createOWLObjectPropertyBasedOnConfiguredPrefix(umlClassAttribute, errorMessages, domain, attributeType)
      case UndefinedUMLClassAttributeType =>
        owlProperty = createOWLObjectProperty(umlClassAttribute, errorMessages, domain)

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
  private def createOWLObjectProperty(umlClassAttribute: UMLClassAttribute,
                                      errorMessages: mutable.Seq[String],
                                      domain: OWLClass,
                                      umlClassIdentity: UMLClassIdentity): OWLObjectProperty =
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
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLNonNegativeCardinality(maxCardinality)) =>
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
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLInfiniteCardinality(_)) =>
        if minCardinality > 0 then
          val axiom = dataFactory.getOWLSubClassOfAxiom(domain,
            dataFactory.getOWLObjectMinCardinality(minCardinality, objectProperty,
              dataFactory.getOWLClass(umlClassIdentity.classIRI.iri)))
          if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
            errorMessages :+ s"Could not add subclass axiom representing cardinalities for [$minCardinality, *]" +
              s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLNonNegativeCardinality(maxCardinality)) =>
        errorMessages :+ s"Multiplicity error found with multiplicity = [*, $maxCardinality] for attribute = " +
          s"${umlClassAttribute.attributeIdentity.attributeLabel}."
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) =>
    objectProperty

  private def createOWLObjectProperty(umlClassAttribute: UMLClassAttribute,
                                      errorMessages: mutable.Seq[String],
                                      domain: OWLClass,
                                      umlEnumerationIdentity: UMLEnumerationIdentity): OWLObjectProperty =
    logger.debug(s"createOWLObjectProperty: umlClassAttribute=$umlClassAttribute, errorMessages = $errorMessages, " +
      s"domain=$domain, umlEnumerationIdentity=$umlEnumerationIdentity ${Code.source}")
    val objectProperty = dataFactory.getOWLObjectProperty(umlClassAttribute.attributeIdentity.attributeIRI.iri)
    val domainChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, domain))
    if domainChangeApplied != SUCCESSFULLY then
      errorMessages :+ s"Domain axiom for object property ${objectProperty.getIRI} could not be added for " +
        s"${umlClassAttribute.attributeIdentity.attributeIRI.iri}"
    val rangeChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyRangeAxiom(objectProperty,
      dataFactory.getOWLClass(umlEnumerationIdentity.enumerationIRI.iri)))
    if rangeChangeApplied != SUCCESSFULLY then
      errorMessages :+ s"Range axiom for data property ${objectProperty.getIRI} could not be added for " +
        s"${umlClassAttribute.attributeIdentity.attributeIRI.iri}"
    umlClassAttribute.multiplicity match
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLNonNegativeCardinality(maxCardinality)) =>
        logger.trace(s"minCardinality=$minCardinality ${Code.source}")
        val axiom =
          if minCardinality == maxCardinality && minCardinality != 0 then
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectExactCardinality(maxCardinality, objectProperty,
              dataFactory.getOWLClass(umlEnumerationIdentity.enumerationIRI.iri)))
          else if minCardinality > 0 then
            dataFactory.getOWLSubClassOfAxiom(domain,
              dataFactory.getOWLObjectIntersectionOf(
                dataFactory.getOWLObjectMinCardinality(minCardinality, objectProperty,
                  dataFactory.getOWLClass(umlEnumerationIdentity.enumerationIRI.iri)),
                dataFactory.getOWLObjectMaxCardinality(maxCardinality, objectProperty,
                  dataFactory.getOWLClass(umlEnumerationIdentity.enumerationIRI.iri))))
          else
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectMaxCardinality(maxCardinality, objectProperty,
              dataFactory.getOWLClass(umlEnumerationIdentity.enumerationIRI.iri)))
        if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
          errorMessages :+ s"Could not add subclass axiom representing cardinalities [$minCardinality, $maxCardinality] for " +
            s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLInfiniteCardinality(_)) =>
        if minCardinality > 0 then
          val axiom = dataFactory.getOWLSubClassOfAxiom(domain,
            dataFactory.getOWLObjectMinCardinality(minCardinality, objectProperty,
              dataFactory.getOWLClass(umlEnumerationIdentity.enumerationIRI.iri)))
          if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
            errorMessages :+ s"Could not add subclass axiom representing cardinalities for [$minCardinality, *]" +
              s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLNonNegativeCardinality(maxCardinality)) =>
        errorMessages :+ s"Multiplicity error found with multiplicity = [*, $maxCardinality] for attribute = " +
          s"${umlClassAttribute.attributeIdentity.attributeLabel}."
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) =>
    objectProperty

  private def createOWLObjectProperty(umlClassAttribute: UMLClassAttribute,
                                      errorMessages: mutable.Seq[String],
                                      domain: OWLClass): OWLObjectProperty =
    logger.debug(s"createOWLObjectProperty: umlClassAttribute=$umlClassAttribute, errorMessages = $errorMessages, " +
      s"domain=$domain ${Code.source}")
    val objectProperty = dataFactory.getOWLObjectProperty(umlClassAttribute.attributeIdentity.attributeIRI.iri)
    val domainChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, domain))
    if domainChangeApplied != SUCCESSFULLY then
      errorMessages :+ s"Domain axiom for object property ${objectProperty.getIRI} could not be added for " +
        s"${umlClassAttribute.attributeIdentity.attributeIRI.iri}"

    umlClassAttribute.multiplicity match
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLNonNegativeCardinality(maxCardinality)) =>
        logger.trace(s"minCardinality=$minCardinality ${Code.source}")
        val axiom =
          if minCardinality == maxCardinality && minCardinality != 0 then
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectExactCardinality(maxCardinality, objectProperty,
              dataFactory.getOWLClass(dataFactory.getOWLThing)))
          else if minCardinality > 0 then
            dataFactory.getOWLSubClassOfAxiom(domain,
              dataFactory.getOWLObjectIntersectionOf(
                dataFactory.getOWLObjectMinCardinality(minCardinality, objectProperty,
                  dataFactory.getOWLClass(dataFactory.getOWLThing)),
                dataFactory.getOWLObjectMaxCardinality(maxCardinality, objectProperty,
                  dataFactory.getOWLClass(dataFactory.getOWLThing))))
          else
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectMaxCardinality(maxCardinality, objectProperty,
              dataFactory.getOWLClass(dataFactory.getOWLThing)))
        if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
          errorMessages :+ s"Could not add subclass axiom representing cardinalities [$minCardinality, $maxCardinality] for " +
            s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLInfiniteCardinality(_)) =>
        if minCardinality > 0 then
          val axiom = dataFactory.getOWLSubClassOfAxiom(domain,
            dataFactory.getOWLObjectMinCardinality(minCardinality, objectProperty,
              dataFactory.getOWLClass(dataFactory.getOWLThing)))
          if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
            errorMessages :+ s"Could not add subclass axiom representing cardinalities for [$minCardinality, *]" +
              s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLNonNegativeCardinality(maxCardinality)) =>
        errorMessages :+ s"Multiplicity error found with multiplicity = [*, $maxCardinality] for attribute = " +
          s"${umlClassAttribute.attributeIdentity.attributeLabel}."
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) =>
    objectProperty


  /**
   * Creates an OWL object property with domain, range and cardinalities corresponding to the semantics of the UML class
   * attribute.
   *
   * @param umlClassAttribute
   * @param errorMessages
   * @param typeBasedOnConfiguredPrefix
   * @param domain
   * @return The OWL object property that was created.
   */
  private def createOWLObjectPropertyBasedOnConfiguredPrefix(umlClassAttribute: UMLClassAttribute,
                                                             errorMessages: mutable.Seq[String],
                                                             domain: OWLClass,
                                                             typeBasedOnConfiguredPrefix: Curie): OWLObjectProperty =
    logger.debug(s"createOWLObjectProperty: umlClassAttribute=$umlClassAttribute, errorMessages = $errorMessages, " +
      s"domain=$domain, typeBasedOnConfiguredPrefix=$typeBasedOnConfiguredPrefix ${Code.source}")
    val objectProperty = dataFactory.getOWLObjectProperty(umlClassAttribute.attributeIdentity.attributeIRI.iri)
    val domainChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, domain))
    if domainChangeApplied != SUCCESSFULLY then
      errorMessages :+ s"Domain axiom for object property ${objectProperty.getIRI} could not be added for " +
        s"${umlClassAttribute.attributeIdentity.attributeIRI.iri}"
    val rangeChangeApplied = manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyRangeAxiom(objectProperty,
      dataFactory.getOWLClass(typeBasedOnConfiguredPrefix.toIRI)))
    if rangeChangeApplied != SUCCESSFULLY then
      errorMessages :+ s"Range axiom for data property ${objectProperty.getIRI} could not be added for " +
        s"${umlClassAttribute.attributeIdentity.attributeIRI.iri}"
    umlClassAttribute.multiplicity match
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLNonNegativeCardinality(maxCardinality)) =>
        logger.trace(s"minCardinality=$minCardinality ${Code.source}")
        val axiom =
          if minCardinality == maxCardinality && minCardinality != 0 then
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectExactCardinality(maxCardinality, objectProperty,
              dataFactory.getOWLClass(typeBasedOnConfiguredPrefix.toIRI)))
          else if minCardinality > 0 then
            dataFactory.getOWLSubClassOfAxiom(domain,
              dataFactory.getOWLObjectIntersectionOf(
                dataFactory.getOWLObjectMinCardinality(minCardinality, objectProperty,
                  dataFactory.getOWLClass(typeBasedOnConfiguredPrefix.toIRI)),
                dataFactory.getOWLObjectMaxCardinality(maxCardinality, objectProperty,
                  dataFactory.getOWLClass(typeBasedOnConfiguredPrefix.toIRI))))
          else
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectMaxCardinality(maxCardinality, objectProperty,
              dataFactory.getOWLClass(typeBasedOnConfiguredPrefix.toIRI)))
        if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
          errorMessages :+ s"Could not add subclass axiom representing cardinalities [$minCardinality, $maxCardinality] for " +
            s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLInfiniteCardinality(_)) =>
        if minCardinality > 0 then
          val axiom = dataFactory.getOWLSubClassOfAxiom(domain,
            dataFactory.getOWLObjectMinCardinality(minCardinality, objectProperty,
              dataFactory.getOWLClass(typeBasedOnConfiguredPrefix.toIRI)))
          if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
            errorMessages :+ s"Could not add subclass axiom representing cardinalities for [$minCardinality, *]" +
              s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLNonNegativeCardinality(maxCardinality)) =>
        errorMessages :+ s"Multiplicity error found with multiplicity = [*, $maxCardinality] for attribute = " +
          s"${umlClassAttribute.attributeIdentity.attributeLabel}."
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) =>
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
  private def createOWLDataProperty(umlClassAttribute: UMLClassAttribute,
                                    errorMessages: mutable.Seq[String],
                                    domain: OWLClass,
                                    attributeType: SupportedDataType): OWLDataProperty =
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
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLNonNegativeCardinality(maxCardinality)) =>
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
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLInfiniteCardinality(_)) =>
        if minCardinality > 0 then
          val axiom = dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLDataMinCardinality(minCardinality, dataProperty, dataType))
          if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
            errorMessages :+ s"Could not add subclass axiom representing cardinalities for [$minCardinality, *]" +
              s"${umlClassAttribute.attributeIdentity.attributeLabel}"
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLNonNegativeCardinality(maxCardinality)) =>
        errorMessages :+ s"Multiplicity error found with multiplicity = [*, $maxCardinality] for attribute = " +
          s"${umlClassAttribute.attributeIdentity.attributeLabel}."
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) =>
    dataProperty


  private def createAndAnnotateNamedIndividual(enumerationIdentity: UMLEnumerationIdentity,
                                               umlEnumerationValue: UMLEnumerationValue,
                                               errorMessages: mutable.Seq[String]): OWLNamedIndividual =
    logger.debug(s"createAndAnnotateNamedIndividual: umlEnumerationValue=$umlEnumerationValue, " +
      s"errorMessages=$errorMessages ${Code.source}")
    val owlClass = dataFactory.getOWLClass(enumerationIdentity.enumerationIRI.iri)
    val owlNamedIndividual = dataFactory.getOWLNamedIndividual(umlEnumerationValue.valueIdentity.valueIRI.iri)
    val axiom = dataFactory.getOWLClassAssertionAxiom(owlClass, owlNamedIndividual)
    if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not add class assertion for individual \'$owlNamedIndividual\' of type \'$owlClass\'"
    createDefinitionAnnotation(owlNamedIndividual, umlEnumerationValue.definition.definition, errorMessages)
    createLabelAnnotation(owlNamedIndividual, umlEnumerationValue.valueIdentity.valueLabel, errorMessages)
    owlNamedIndividual


  private def processUMLClasses: mutable.Seq[String] =
    logger.info("processUMLClasses: Start")
    var errorMessages: mutable.Seq[String] = new ArrayBuffer[String]()
    umlClassDiagram.umlClasses.umlClasses.keySet.foreach(id => {
      val umlClassOption = umlClassDiagram.umlClasses.umlClasses.get(id)
      if umlClassOption.isDefined then
        val umlClass = umlClassOption.get
        val owlClass = createAndAnnotateOWLClass(umlClass, errorMessages, !umlClass.classParentIds.setOfParentNamedElements.isEmpty)
        if umlClass.classParentIds.setOfParentNamedElements.isEmpty then
          logger.trace("ParentIds is EMPTY")
        else
          umlClass.classParentIds.setOfParentNamedElements.foreach(parentClassId => {
            logger.trace(s"parentId=${parentClassId.getName}")
            val parentClassIdentityOption = UMLClassIdentity.findClassNamedElement(parentClassId.getName)
            if parentClassIdentityOption.isDefined then
              if manager.addAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(owlClass,
                dataFactory.getOWLClass(parentClassIdentityOption.get.classIRI.iri))) != SUCCESSFULLY then
                  errorMessages :+ s"Could not add axiom ${owlClass.getIRI} subClassOf ${parentClassId.getName}"
            })
    })
    logger.info("processUMLClasses: Done")
    errorMessages
  end processUMLClasses

  private def processUMLClassAttributes: mutable.Seq[String] =
    logger.info("processUMLClassAttributes: Start")
    var errorMessages: ArrayBuffer[String] = new ArrayBuffer[String]()
    umlClassDiagram.umlClassAttributes.umlClassAttributes.keySet.foreach(id => {
      val umlClassAttributeOption = umlClassDiagram.umlClassAttributes.umlClassAttributes.get(id)
      if umlClassAttributeOption.isDefined then
        val umlClassAttribute = umlClassAttributeOption.get
        val owlProperty = createAndAnnotateOWLProperty(umlClassAttribute, errorMessages)
    })
    logger.info("processUMLClassAttributes: Done")
    errorMessages
  end processUMLClassAttributes


  private def processUMLEnumerations: mutable.Seq[String] =
    logger.info("processUMLEnumerations: Start")
    var errorMessages: mutable.Seq[String] = new ArrayBuffer[String]()
    umlClassDiagram.umlEnumerations.umlEnumerations.keySet.foreach(id => {
      val umlEnumerationOption = umlClassDiagram.umlEnumerations.umlEnumerations.get(id)
      if umlEnumerationOption.isDefined then {
        val umlEnumeration = umlEnumerationOption.get
        val owlClass = createAndAnnotateOWLClass(umlEnumeration, errorMessages)
        val umlEnumerationValueIdentitiesOption = UMLEnumeration.find(umlEnumeration.enumerationIdentity)
        if umlEnumerationValueIdentitiesOption.isDefined then
          val individuals: mutable.Set[OWLIndividual] = new mutable.HashSet[OWLIndividual]()
          umlEnumerationValueIdentitiesOption.get.foreach(v => {
            val individual = createAndAnnotateNamedIndividual(
              umlEnumeration.enumerationIdentity,
              UMLEnumerationValue.find(v).get,
              errorMessages)
            individuals += individual
          })
          val owlClassExpression = dataFactory.getOWLObjectOneOf(individuals.asJava)
          val owlEquivalentClassesAxiom: OWLEquivalentClassesAxiom =
            dataFactory.getOWLEquivalentClassesAxiom(owlClass, owlClassExpression)
          if (manager.addAxiom(ontology, owlEquivalentClassesAxiom) != SUCCESSFULLY) then
            errorMessages :+ s"Could not add axiom ${owlClass.getIRI} equivalent to ${owlClassExpression}"
      }
    })
    logger.info("processUMLEnumerations: Done")
    errorMessages
  end processUMLEnumerations

  def generateOWL: Either[String, ListBuffer[String]] =
    logger.info("generateOWL: Start")
    var errorMessages = new ListBuffer[String]()
    errorMessages.appendAll(processUMLClasses)
    errorMessages.appendAll(processUMLClassAttributes)
    errorMessages.appendAll(processUMLEnumerations)
    try
      manager.saveOntology(ontology, new RDFXMLDocumentFormat(), IRI.create(umlClassDiagram.owlOntologyFile))
      logger.info("generateOWL: Done")
      Right(errorMessages)
    catch
      case e: OWLOntologyStorageException =>
        logger.info("generateOWL: Done")
        Left(e.getMessage)
  end generateOWL

end UML2OWLWriter
