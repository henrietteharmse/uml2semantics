package org.uml2semantics.writer

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model.parameters.ChangeApplied.SUCCESSFULLY
import org.semanticweb.owlapi.vocab.OWL2Datatype
import org.uml2semantics.inline.Code
import org.uml2semantics.model.*
import org.uml2semantics.model.SupportedDataType.getIRI

import java.io.File
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Seq}
import scala.collection.{immutable, mutable}
import scala.jdk.CollectionConverters.*

class UML2OWLWriter(ontologyIRI: String,
                    owlOntologyFile: File,
                    classes: Set[UMLClass],
                    attributes: Set[UMLAttribute]):

  private val logger = Logger("UML2OWLWriter")
  private val manager = OWLManager.createOWLOntologyManager
  private val ontology = manager.createOntology(IRI.create(ontologyIRI))
  private val dataFactory = manager.getOWLDataFactory

  /**
   * All classes in a UML class diagram that form part of a generalization set.
   */
  private val allCompleteDisjointOWLClasses: mutable.Set[OWLClass] = mutable.Set[OWLClass]()
  private val allCompleteOverlappingOWLClasses: mutable.Set[OWLClass] = mutable.Set[OWLClass]()
  private val allIncompleteDisjointOWLClasses: mutable.Set[OWLClass] = mutable.Set[OWLClass]()
  private val allIncompleteOverlappingOWLClasses: mutable.Set[OWLClass] = mutable.Set[OWLClass]()

  /**
   * All classes in a UML class diagram are disjoint by default, unless they are part of a generalization set. For
   * generalization sets, disjointness are defined as part of generalization set and thus dealt with when the
   * generalization set is processed.
   */
  private val disjointClassesNotPartOfGeneralizationSets: mutable.Set[OWLClass] = mutable.Set[OWLClass]()

  /**
   * Creates an OWLClass for a UMLClass and annotates it with definition and label.
   * It also creates all children of this class.
   *
   *
   * @param umlClass
   * @param errorMessages
   * @return
   */
  private def createAndAnnotateOWLClass(umlClass: UMLClass,
                                        errorMessages: mutable.Seq[String]): OWLClass =
    logger.trace(s"createAndAnnotateOWLClass: umlClass=$umlClass, errorMessages=$errorMessages ${Code.source}")
    val owlClass = createOWLClass(umlClass.classIdentity, errorMessages)
    if !allCompleteDisjointOWLClasses.contains(owlClass) && !allCompleteOverlappingOWLClasses.contains(owlClass) &&
      !allIncompleteDisjointOWLClasses.contains(owlClass) &&  !allIncompleteOverlappingOWLClasses.contains(owlClass)
    then
      disjointClassesNotPartOfGeneralizationSets += owlClass

    umlClass.children.setOfGeneralizationSets.foreach { generalizationSet =>
      (generalizationSet.coveringConstraint, generalizationSet.disjointConstraint) match
        case (CoveringConstraint.Complete, DisjointConstraint.Disjoint) =>
          createCompleteDisjointChildren(owlClass, generalizationSet, errorMessages)
        case (CoveringConstraint.Complete, DisjointConstraint.Overlapping) =>
          createCompleteOverlappingChildren(owlClass, generalizationSet, errorMessages)
        case (CoveringConstraint.Incomplete, DisjointConstraint.Disjoint) =>
          createIncompleteDisjointChildren(owlClass, generalizationSet, errorMessages)
        case (CoveringConstraint.Incomplete, DisjointConstraint.Overlapping) =>
          createIncompleteOverlappingChildren(owlClass, generalizationSet, errorMessages)
    }

    umlClass.classDefinitionOption.foreach(classDefinition =>
      createDefinitionAnnotation(owlClass, classDefinition.definition, errorMessages))
    createLabelAnnotation(owlClass, umlClass.classIdentity.getLabel, errorMessages)
    logger.trace(s"createAndAnnotateOWLClass: Done. allIncompleteOverlappingOWLClasses=$allIncompleteOverlappingOWLClasses, " +
      s"disjointClassesNotPartOfGeneralizationSets=$disjointClassesNotPartOfGeneralizationSets ${Code.source}")
    owlClass

  private def createAndAnnotateOWLAttribute(umlAttribute: UMLAttribute,
                                    errorMessages: mutable.Seq[String]): mutable.Seq[String] =

    logger.trace(s"createAndAnnotateOWLAttribute: umlAttribute=$umlAttribute, errorMessages=$errorMessages ${Code.source}")
    val owlClass = dataFactory.getOWLClass(umlAttribute.attributeIdentity.classIdentity.getIRI)
    val owlProperty = umlAttribute.typeOfAttribute.map { attributeType =>
      attributeType match
        case UMLXMLDataType(attributeType) =>
          createDataProperty(umlAttribute, owlClass, attributeType, errorMessages)
        case CurieBasedUMLClassAttributeType(attributeType: UMLAttributeType) =>
          createObjectProperty(umlAttribute, owlClass, attributeType, errorMessages)
        case UMLClassType(attributeType) =>
          createObjectProperty(umlAttribute, owlClass, attributeType, errorMessages)
        case _ =>
          throw new IllegalArgumentException(s"Unsupported attribute type found for ${umlAttribute.attributeIdentity.getLabel}")
    }.getOrElse {
      createObjectProperty(umlAttribute, owlClass, errorMessages)
    }

    umlAttribute.definition.foreach(attributeDefinition =>
      createDefinitionAnnotation(owlProperty, attributeDefinition.definition, errorMessages))
    createLabelAnnotation(owlProperty, umlAttribute.getLabel, errorMessages)
    errorMessages


  private def createDataProperty(umlAttribute: UMLAttribute, domain: OWLClass, attributeType: SupportedDataType,
                                 errorMessages: mutable.Seq[String]): OWLDataProperty =

    val owlDataProperty = dataFactory.getOWLDataProperty(umlAttribute.getIRI)
    val owlDataPropertyDomainAxiom = dataFactory.getOWLDataPropertyDomainAxiom(owlDataProperty, domain)
    if manager.addAxiom(ontology, owlDataPropertyDomainAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not create domain axiom for $owlDataProperty with domain=$domain"
    val dataType: OWL2Datatype = OWL2Datatype.getDatatype(IRI.create(getIRI(attributeType)))
    val owlDataPropertyRangeAxiom = dataFactory.getOWLDataPropertyRangeAxiom(owlDataProperty, dataType)
    if manager.addAxiom(ontology, owlDataPropertyRangeAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not create range axiom for $owlDataProperty with range=$dataType"
    createDataExistentialRestrictions(umlAttribute, domain,  owlDataProperty, dataType, errorMessages)
    owlDataProperty

  private def createObjectProperty(umlAttribute: UMLAttribute, domain: OWLClass, attributeType: Curie,
                                   errorMessages: mutable.Seq[String]): OWLObjectProperty =
    val owlObjectProperty = dataFactory.getOWLObjectProperty(umlAttribute.getIRI)
    val owlObjectPropertyDomainAxiom = dataFactory.getOWLObjectPropertyDomainAxiom(owlObjectProperty, domain)
    if manager.addAxiom(ontology, owlObjectPropertyDomainAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not create domain axiom for $owlObjectProperty with domain=$domain"
    val owlRange: OWLClass = dataFactory.getOWLClass(attributeType.getIRI)
    val owlObjectPropertyRangeAxiom = dataFactory.getOWLObjectPropertyRangeAxiom(owlObjectProperty, owlRange)
    if manager.addAxiom(ontology, owlObjectPropertyRangeAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not create range axiom for $owlObjectProperty with range=$owlRange"
    createObjectExistentialRestrictions(umlAttribute, domain, owlObjectProperty, errorMessages)
    owlObjectProperty

  private def createObjectProperty(umlAttribute: UMLAttribute, domain: OWLClass, attributeType: UMLClass,
                                   errorMessages: mutable.Seq[String]): OWLObjectProperty =
    val owlObjectProperty = dataFactory.getOWLObjectProperty(umlAttribute.getIRI)
    val owlObjectPropertyDomainAxiom = dataFactory.getOWLObjectPropertyDomainAxiom(owlObjectProperty, domain)
    if manager.addAxiom(ontology, owlObjectPropertyDomainAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not create domain axiom for $owlObjectProperty with domain=$domain"
    val owlRange: OWLClass = dataFactory.getOWLClass(attributeType.getIRI)
    val owlObjectPropertyRangeAxiom = dataFactory.getOWLObjectPropertyRangeAxiom(owlObjectProperty, owlRange)
    if manager.addAxiom(ontology, owlObjectPropertyRangeAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not create range axiom for $owlObjectProperty with range=$owlRange"
    createObjectExistentialRestrictions(umlAttribute, domain, owlObjectProperty, errorMessages)
    owlObjectProperty

  private def createObjectProperty(umlAttribute: UMLAttribute, domain: OWLClass,
                                   errorMessages: mutable.Seq[String]): OWLObjectProperty =
    val owlObjectProperty = dataFactory.getOWLObjectProperty(umlAttribute.getIRI)
    val owlObjectPropertyDomainAxiom = dataFactory.getOWLObjectPropertyDomainAxiom(owlObjectProperty, domain)
    if manager.addAxiom(ontology, owlObjectPropertyDomainAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not create domain axiom for $owlObjectProperty with domain=$domain"
    createObjectExistentialRestrictions(umlAttribute, domain, owlObjectProperty, errorMessages)
    owlObjectProperty

  private def createObjectExistentialRestrictions(umlAttribute: UMLAttribute, domain: OWLClass,
                                                  owlObjectProperty: OWLObjectProperty,
                                                  errorMessages: mutable.Seq[String]): mutable.Seq[String] =

    umlAttribute.multiplicity match
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLNonNegativeCardinality(maxCardinality)) =>
        logger.trace(s"minCardinality=$minCardinality ${Code.source}")
        val axiom = (minCardinality, maxCardinality) match
          case (min, max) if min == max && min != 0 =>
            dataFactory.getOWLSubClassOfAxiom(domain,
              dataFactory.getOWLObjectExactCardinality(max, owlObjectProperty,
                dataFactory.getOWLClass(dataFactory.getOWLThing)))
          case (min, max) if min > 0 =>
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectIntersectionOf(
              dataFactory.getOWLObjectMinCardinality(min, owlObjectProperty, dataFactory.getOWLClass(dataFactory.getOWLThing)),
              dataFactory.getOWLObjectMaxCardinality(max, owlObjectProperty, dataFactory.getOWLClass(dataFactory.getOWLThing))))
          case (_, max) =>
            dataFactory.getOWLSubClassOfAxiom(domain,
              dataFactory.getOWLObjectMaxCardinality(max, owlObjectProperty,
                dataFactory.getOWLClass(dataFactory.getOWLThing)))
        if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
          errorMessages :+ s"Could not add subclass axiom representing cardinalities [$minCardinality, $maxCardinality] " +
            s"for ${umlAttribute.attributeIdentity.getLabel}"
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLInfiniteCardinality(_)) if minCardinality > 0 =>
        val axiom = dataFactory.getOWLSubClassOfAxiom(domain,
          dataFactory.getOWLObjectMinCardinality(minCardinality, owlObjectProperty,
            dataFactory.getOWLClass(dataFactory.getOWLThing)))
        if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
          errorMessages :+ s"Could not add subclass axiom representing cardinalities for [$minCardinality, *] " +
            s"${umlAttribute.attributeIdentity.getLabel}"
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLNonNegativeCardinality(maxCardinality)) =>
        errorMessages :+ s"Multiplicity error found with multiplicity = [*, $maxCardinality] for attribute = " +
          s"${umlAttribute.attributeIdentity.getLabel}."
      case UMLMultiplicity(UMLNonNegativeCardinality(_), UMLInfiniteCardinality(_)) =>
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) =>
    errorMessages


  private def createDataExistentialRestrictions(umlAttribute: UMLAttribute, domain: OWLClass,
                                                owlDataProperty: OWLDataProperty,
                                                dataType: OWL2Datatype,
                                                errorMessages: mutable.Seq[String]): mutable.Seq[String] =

    umlAttribute.multiplicity match
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLNonNegativeCardinality(maxCardinality)) =>
        logger.trace(s"minCardinality=$minCardinality ${Code.source}")
        val axiom = (minCardinality, maxCardinality) match
          case (min, max) if min == max && min != 0 =>
            dataFactory.getOWLSubClassOfAxiom(domain,
              dataFactory.getOWLDataExactCardinality(max, owlDataProperty, dataType))
          case (min, max) if min > 0 =>
            dataFactory.getOWLSubClassOfAxiom(domain, dataFactory.getOWLObjectIntersectionOf(
              dataFactory.getOWLDataMinCardinality(min, owlDataProperty, dataType),
              dataFactory.getOWLDataMaxCardinality(max, owlDataProperty, dataType)))
          case (_, max) =>
            dataFactory.getOWLSubClassOfAxiom(domain,
              dataFactory.getOWLDataMaxCardinality(max, owlDataProperty, dataType))
        if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
          errorMessages :+ s"Could not add subclass axiom representing cardinalities [$minCardinality, $maxCardinality] " +
            s"for ${umlAttribute.attributeIdentity.getLabel}"
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLInfiniteCardinality(_)) if minCardinality > 0 =>
        val axiom = dataFactory.getOWLSubClassOfAxiom(domain,
          dataFactory.getOWLDataMinCardinality(minCardinality, owlDataProperty, dataType))
        if manager.addAxiom(ontology, axiom) != SUCCESSFULLY then
          errorMessages :+ s"Could not add subclass axiom representing cardinalities for [$minCardinality, *] " +
            s"${umlAttribute.attributeIdentity.getLabel}"
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLNonNegativeCardinality(maxCardinality)) =>
        errorMessages :+ s"Multiplicity error found with multiplicity = [*, $maxCardinality] for attribute = " +
          s"${umlAttribute.attributeIdentity.getLabel}."
      case UMLMultiplicity(UMLNonNegativeCardinality(_), UMLInfiniteCardinality(_)) =>
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) =>

    errorMessages


  private def createCompleteDisjointChildren(owlClass: OWLClass, generalizationSet: UMLGeneralizationSet,
                                             errorMessages: Seq[String]) =
    logger.trace(s"createCompleteDisjointChildren: owlClass=$owlClass, generalizationSet=$generalizationSet, " +
      s"errorMessages=$errorMessages ${Code.source}")

    val completeDisjointClassesAsIRIs = mutable.Set[IRI]()
    val completeDisjointOWLClasses = mutable.Set[OWLClass]()
    generalizationSet.generalizationSet.foreach { child =>
      val childClass = createOWLClass(child, errorMessages)
      completeDisjointOWLClasses += childClass
      allCompleteDisjointOWLClasses += childClass
      disjointClassesNotPartOfGeneralizationSets -= childClass
      completeDisjointClassesAsIRIs += childClass.getIRI
      createOWLSubClassAxiom(childClass, owlClass, errorMessages)
    }
    createOWLDisjointUnionAxiom(owlClass, completeDisjointOWLClasses, errorMessages)


  private def createCompleteOverlappingChildren(owlClass: OWLClass, generalizationSet: UMLGeneralizationSet,
                                                errorMessages: Seq[String]) =
    logger.trace(s"createCompleteOverlappingChildren: owlClass=$owlClass, generalizationSet=$generalizationSet, " +
      s"errorMessages=$errorMessages ${Code.source}")
    val completeOverlappingClassesAsIRIs = mutable.Set[IRI]()
    val completeOverlappingOWLClasses = mutable.Set[OWLClass]()
    generalizationSet.generalizationSet.foreach { child =>
      val childClass: OWLClass = createOWLClass(child, errorMessages)
      completeOverlappingOWLClasses += childClass
      allCompleteOverlappingOWLClasses += childClass
      disjointClassesNotPartOfGeneralizationSets -= childClass
      completeOverlappingClassesAsIRIs += childClass.getIRI
      createOWLSubClassAxiom(childClass, owlClass, errorMessages)
    }
    createOWLEquivalentToUnionAxiom(owlClass, completeOverlappingOWLClasses, errorMessages)

  private def createIncompleteDisjointChildren(owlClass: OWLClass, generalizationSet: UMLGeneralizationSet,
                                               errorMessages: Seq[String]) =
    logger.trace(s"createIncompleteDisjointChildren: owlClass=$owlClass, generalizationSet=$generalizationSet, " +
      s"errorMessages=$errorMessages ${Code.source}")

    val incompleteDisjointClassesAsIRIs = mutable.Set[IRI]()
    val incompleteDisjointOWLClasses = mutable.Set[OWLClass]()
    generalizationSet.generalizationSet.foreach { child =>
      val childClass = createOWLClass(child, errorMessages)
      incompleteDisjointOWLClasses += childClass
      allIncompleteDisjointOWLClasses += childClass
      disjointClassesNotPartOfGeneralizationSets -= childClass
      incompleteDisjointClassesAsIRIs += childClass.getIRI
      createOWLSubClassAxiom(childClass, owlClass, errorMessages)
    }
    createOWLDisjointClassesAxiom(incompleteDisjointOWLClasses, errorMessages)


  private def createIncompleteOverlappingChildren(owlClass: OWLClass, generalizationSet: UMLGeneralizationSet,
                                                  errorMessages: Seq[String]): Unit =
    logger.trace(s"createIncompleteOverlappingChildren: owlClass=$owlClass, generalizationSet=$generalizationSet, " +
      s"errorMessages=$errorMessages ${Code.source}")

    val incompleteOverlappingClassesAsIRIs = mutable.Set[IRI]()
    generalizationSet.generalizationSet.foreach { child =>
      val childClass = createOWLClass(child, errorMessages)
      allIncompleteOverlappingOWLClasses += childClass
      disjointClassesNotPartOfGeneralizationSets -= childClass
      incompleteOverlappingClassesAsIRIs += childClass.getIRI
      createOWLSubClassAxiom(childClass, owlClass, errorMessages)
    }


  private def createDefinitionAnnotation(owlNamedObject: OWLNamedObject,
                                         definition: String,
                                         errorMessages: mutable.Seq[String]): Unit =
    logger.trace(s"createDefinitionAnnotation: owlNamedObject=$owlNamedObject, definition=$definition, " +
      s"errorMessages=$errorMessages ${Code.source}")
    if definition.nonEmpty then
      val owlClassDefinitionAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
        dataFactory.getOWLAnnotation(dataFactory.getRDFSComment(), dataFactory.getOWLLiteral(definition)))
      if manager.addAxiom(ontology, owlClassDefinitionAnnotationAxiom) != SUCCESSFULLY then
        errorMessages :+ s"Could not add definition=$definition for ${owlNamedObject.getIRI}"

  /**
   * Creates an RDFS label annotation if the label is not empty, otherwise uses the IRI of the OWLNamedObject
   * @param owlNamedObject
   * @param label
   * @param errorMessages
   * @return
   */
  private def createLabelAnnotation(owlNamedObject: OWLNamedObject,
                                    label: String,
                                    errorMessages: mutable.Seq[String]): OWLAnnotationAssertionAxiom =
    logger.trace(s"createLabelAnnotation: owlNamedObject=$owlNamedObject, label=$label, " +
      s"errorMessages=$errorMessages ${Code.source}")
    if label.nonEmpty then
      createOWLAnnotationAssertionAxiom(owlNamedObject, dataFactory.getRDFSLabel(), label, errorMessages)
    else
      createOWLAnnotationAssertionAxiom(owlNamedObject, dataFactory.getRDFSLabel(), owlNamedObject.getIRI.getIRIString, errorMessages)

  private def createOWLAnnotationAssertionAxiom(owlNamedObject: OWLNamedObject,
                                                owlAnnotationProperty: OWLAnnotationProperty,
                                                literal: String,
                                                errorMessages: mutable.Seq[String]): OWLAnnotationAssertionAxiom =
    logger.trace(s"createOWLAnnotationAssertionAxiom: owlNamedObject=$owlNamedObject, " +
      s"owlAnnotationProperty=$owlAnnotationProperty, literal=$literal, errorMessages=$errorMessages ${Code.source}")

    if literal.isBlank then
      throw new IllegalArgumentException("Literal cannot be blank")
    val owlAnnotationAssertionAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
      dataFactory.getOWLAnnotation(owlAnnotationProperty, dataFactory.getOWLLiteral(literal)))
    if manager.addAxiom(ontology, owlAnnotationAssertionAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not add annotation assertion for ${owlNamedObject.getIRI} with literal=$literal"
    owlAnnotationAssertionAxiom


  private def createOWLClass(umlClass: UMLClassIdentity, errorMessages: mutable.Seq[String]): OWLClass =
    val owlClass = dataFactory.getOWLClass(umlClass.getIRI)
    if manager.addAxiom(ontology, dataFactory.getOWLDeclarationAxiom(owlClass)) != SUCCESSFULLY then
      errorMessages :+ s"Could not declare class=${owlClass.getIRI}"
    owlClass

  private def createOWLDisjointClassesAxiom(disjointOWLClasses: mutable.Set[OWLClass],
                                            errorMessages: mutable.Seq[String]): Unit =
    if disjointOWLClasses.size < 2 then
      logger.warn(s"createOWLDisjointClassesAxiom: disjointOWLClasses.size=${disjointOWLClasses.size} < 2")
    else
      val disjointClassesAxiom = dataFactory.getOWLDisjointClassesAxiom(disjointOWLClasses.asJava)
      if manager.addAxiom(ontology, disjointClassesAxiom) != SUCCESSFULLY then
        errorMessages :+ s"Could not add disjointClasses axiom for ${disjointOWLClasses.map(_.getIRI).mkString(", ")}"

  private def createOWLEquivalentToUnionAxiom(owlClass: OWLClass, completeOverlappingOWLClasses: mutable.Set[OWLClass],
                                          errorMessages: mutable.Seq[String]): Unit =
    if completeOverlappingOWLClasses.size < 2 then
      logger.warn(s"createOWLEquivalentToUnionAxiom: completeOverlappingOWLClasses.size=${completeOverlappingOWLClasses.size} < 2")
    else
      val equivalentToUnionAxiom = dataFactory.getOWLEquivalentClassesAxiom(owlClass,
        dataFactory.getOWLObjectUnionOf(completeOverlappingOWLClasses.asJava))
      if manager.addAxiom(ontology, equivalentToUnionAxiom) != SUCCESSFULLY then
        errorMessages :+ s"Could not add overlappingUnion axiom for ${owlClass.getIRI}" +
          s" equivalent to union of ${completeOverlappingOWLClasses.map(_.getIRI).mkString(", ")}"

  private def createOWLDisjointUnionAxiom(owlClass: OWLClass, completeDisjointOWLClasses: mutable.Set[OWLClass],
                                       errorMessages: mutable.Seq[String]): OWLDisjointUnionAxiom =
    val disjointUnionAxiom = dataFactory.getOWLDisjointUnionAxiom(owlClass, completeDisjointOWLClasses.asJava)
    if manager.addAxiom(ontology, disjointUnionAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not add disjointUnion axiom for ${owlClass.getIRI}" +
        s" equivalent to disjoint union of ${completeDisjointOWLClasses.map(_.getIRI).mkString(", ")}"
    disjointUnionAxiom


  private def createOWLSubClassAxiom(childClass: OWLClass, superClass: OWLClass,
                                  errorMessages: mutable.Seq[String]): OWLSubClassOfAxiom =
    val subClassOfAxiom = dataFactory.getOWLSubClassOfAxiom(childClass, superClass)
    if manager.addAxiom(ontology, subClassOfAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not add subClassOf axiom for subclass=${childClass.getIRI}, superclass=${superClass.getIRI}"
    subClassOfAxiom


  private def processUMLClasses: mutable.Seq[String] =
    logger.info("processUMLClasses: Start")
    var errorMessages: mutable.Seq[String] = new ArrayBuffer[String]()
    classes.foreach(umlClass =>
        val owlClass = createAndAnnotateOWLClass(umlClass, errorMessages))
    createOWLDisjointClassesAxiom(disjointClassesNotPartOfGeneralizationSets, errorMessages)
    logger.info("processUMLClasses: Done")
    errorMessages
  end processUMLClasses

  private def processUMLAttributes: mutable.Seq[String] =
    logger.info("processUMLAttributes: Start")
    var errorMessages: mutable.Seq[String] = new ArrayBuffer[String]()
    attributes.foreach(umlAttribute =>
      val owlAttribute = createAndAnnotateOWLAttribute(umlAttribute, errorMessages))
    logger.info("processUMLAttributes: Done")
    errorMessages



  def generateOWL: Either[String, ListBuffer[String]] =
    logger.info("generateOWL: Start")
    var errorMessages = new ListBuffer[String]()
    errorMessages.appendAll(processUMLClasses)
    errorMessages.appendAll(processUMLAttributes)
    try
      manager.saveOntology(ontology, new RDFXMLDocumentFormat(), IRI.create(owlOntologyFile))
      logger.info("generateOWL: Done")
      Right(errorMessages)
    catch
      case e: OWLOntologyStorageException =>
        logger.info("generateOWL: Done")
        Left(e.getMessage)
  end generateOWL
  
end UML2OWLWriter
