package org.uml2semantics.writer

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model.parameters.ChangeApplied.UNSUCCESSFULLY
import org.uml2semantics.diagnostics.{Diagnostic, DiagnosticCode, Diagnostics}
import org.uml2semantics.inline.Code
import org.uml2semantics.model.*
import org.uml2semantics.model.SupportedDataType.getIRI

import java.io.File
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.{immutable, mutable}
import scala.jdk.CollectionConverters.*

class UML2OWLWriter(ontologyIRI: String,
                    owlOntologyFile: File,
                    classes: Set[UMLClass],
                    attributes: Set[UMLAttribute],
                    globalDisjointness: Boolean = true):

  private val logger = Logger("UML2OWLWriter")
  private val diagnostics = new Diagnostics
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
                                        errorMessages: mutable.Buffer[String]): OWLClass =
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
                                    errorMessages: mutable.Buffer[String]): mutable.Buffer[String] =

    logger.trace(s"createAndAnnotateOWLAttribute: umlAttribute=$umlAttribute, errorMessages=$errorMessages ${Code.source}")
    val owlClass = dataFactory.getOWLClass(umlAttribute.attributeIdentity.classIdentity.getIRI)
    val owlProperty = umlAttribute.typeOfAttribute.map { attributeType =>
      attributeType match
        case UMLXMLDataType(attributeType) =>
          createDataProperty(umlAttribute, owlClass, attributeType, errorMessages)
        case CurieBasedUMLClassAttributeType(attributeType) =>
          createObjectProperty(umlAttribute, owlClass, attributeType, errorMessages)
        case UMLClassType(attributeType) =>
          createObjectProperty(umlAttribute, owlClass, attributeType, errorMessages)
    }.getOrElse {
      createObjectProperty(umlAttribute, owlClass, errorMessages)
    }

    umlAttribute.definition.foreach(attributeDefinition =>
      createDefinitionAnnotation(owlProperty, attributeDefinition.definition, errorMessages))
    createLabelAnnotation(owlProperty, umlAttribute.getLabel, errorMessages)
    errorMessages


  private def createDataProperty(umlAttribute: UMLAttribute, domain: OWLClass, attributeType: SupportedDataType,
                                 errorMessages: mutable.Buffer[String]): OWLDataProperty =

    val owlDataProperty = dataFactory.getOWLDataProperty(umlAttribute.getIRI)
    val owlDataPropertyDomainAxiom = dataFactory.getOWLDataPropertyDomainAxiom(owlDataProperty, domain)
    if manager.addAxiom(ontology, owlDataPropertyDomainAxiom) == UNSUCCESSFULLY then
      errorMessages += s"Could not create domain axiom for $owlDataProperty with domain=$domain"
    val dataType: OWLDatatype = dataFactory.getOWLDatatype(IRI.create(getIRI(attributeType)))
    val owlDataPropertyRangeAxiom = dataFactory.getOWLDataPropertyRangeAxiom(owlDataProperty, dataType)
    if manager.addAxiom(ontology, owlDataPropertyRangeAxiom) == UNSUCCESSFULLY then
      errorMessages += s"Could not create range axiom for $owlDataProperty with range=$dataType"
    createDataExistentialRestrictions(umlAttribute, domain,  owlDataProperty, dataType, errorMessages)
    owlDataProperty

  private def createObjectProperty(umlAttribute: UMLAttribute, domain: OWLClass, attributeType: Curie,
                                   errorMessages: mutable.Buffer[String]): OWLObjectProperty =
    val owlObjectProperty = dataFactory.getOWLObjectProperty(umlAttribute.getIRI)
    val owlObjectPropertyDomainAxiom = dataFactory.getOWLObjectPropertyDomainAxiom(owlObjectProperty, domain)
    if manager.addAxiom(ontology, owlObjectPropertyDomainAxiom) == UNSUCCESSFULLY then
      errorMessages += s"Could not create domain axiom for $owlObjectProperty with domain=$domain"
    val owlRange: OWLClass = dataFactory.getOWLClass(attributeType.getIRI)
    val owlObjectPropertyRangeAxiom = dataFactory.getOWLObjectPropertyRangeAxiom(owlObjectProperty, owlRange)
    if manager.addAxiom(ontology, owlObjectPropertyRangeAxiom) == UNSUCCESSFULLY then
      errorMessages += s"Could not create range axiom for $owlObjectProperty with range=$owlRange"
    createObjectExistentialRestrictions(umlAttribute, domain, owlObjectProperty, errorMessages)
    owlObjectProperty

  private def createObjectProperty(umlAttribute: UMLAttribute, domain: OWLClass, attributeType: UMLClass,
                                   errorMessages: mutable.Buffer[String]): OWLObjectProperty =
    val owlObjectProperty = dataFactory.getOWLObjectProperty(umlAttribute.getIRI)
    val owlObjectPropertyDomainAxiom = dataFactory.getOWLObjectPropertyDomainAxiom(owlObjectProperty, domain)
    if manager.addAxiom(ontology, owlObjectPropertyDomainAxiom) == UNSUCCESSFULLY then
      errorMessages += s"Could not create domain axiom for $owlObjectProperty with domain=$domain"
    val owlRange: OWLClass = dataFactory.getOWLClass(attributeType.getIRI)
    val owlObjectPropertyRangeAxiom = dataFactory.getOWLObjectPropertyRangeAxiom(owlObjectProperty, owlRange)
    if manager.addAxiom(ontology, owlObjectPropertyRangeAxiom) == UNSUCCESSFULLY then
      errorMessages += s"Could not create range axiom for $owlObjectProperty with range=$owlRange"
    createObjectExistentialRestrictions(umlAttribute, domain, owlObjectProperty, errorMessages)
    owlObjectProperty

  private def createObjectProperty(umlAttribute: UMLAttribute, domain: OWLClass,
                                   errorMessages: mutable.Buffer[String]): OWLObjectProperty =
    val owlObjectProperty = dataFactory.getOWLObjectProperty(umlAttribute.getIRI)
    val owlObjectPropertyDomainAxiom = dataFactory.getOWLObjectPropertyDomainAxiom(owlObjectProperty, domain)
    if manager.addAxiom(ontology, owlObjectPropertyDomainAxiom) == UNSUCCESSFULLY then
      errorMessages += s"Could not create domain axiom for $owlObjectProperty with domain=$domain"
    createObjectExistentialRestrictions(umlAttribute, domain, owlObjectProperty, errorMessages)
    owlObjectProperty

  private def createObjectExistentialRestrictions(umlAttribute: UMLAttribute, domain: OWLClass,
                                                  owlObjectProperty: OWLObjectProperty,
                                                  errorMessages: mutable.Buffer[String]): mutable.Buffer[String] =

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
        if manager.addAxiom(ontology, axiom) == UNSUCCESSFULLY then
          errorMessages += s"Could not add subclass axiom representing cardinalities [$minCardinality, $maxCardinality] " +
            s"for ${umlAttribute.attributeIdentity.getLabel}"
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLInfiniteCardinality(_)) if minCardinality > 0 =>
        val axiom = dataFactory.getOWLSubClassOfAxiom(domain,
          dataFactory.getOWLObjectMinCardinality(minCardinality, owlObjectProperty,
            dataFactory.getOWLClass(dataFactory.getOWLThing)))
        if manager.addAxiom(ontology, axiom) == UNSUCCESSFULLY then
          errorMessages += s"Could not add subclass axiom representing cardinalities for [$minCardinality, *] " +
            s"${umlAttribute.attributeIdentity.getLabel}"
      // Unreachable: the model rejects an unbounded min and substitutes [0,*], and UMLMultiplicity's
      // constructor is private so nothing can build one behind its back. Kept because the compiler
      // cannot prove that, and reported rather than ignored: if this ever does arrive, the ontology
      // would otherwise be silently weaker than the model, with nothing to say why.
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLNonNegativeCardinality(maxCardinality)) =>
        diagnostics.error(DiagnosticCode.UnboundedMinMultiplicity,
          s"Reached the writer with multiplicity [*, $maxCardinality] on attribute " +
            s"${umlAttribute.attributeIdentity.getLabel}, which the model should have rejected. No " +
            s"cardinality restriction was asserted.")
      case UMLMultiplicity(UMLNonNegativeCardinality(_), UMLInfiniteCardinality(_)) =>
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) =>
    errorMessages


  private def createDataExistentialRestrictions(umlAttribute: UMLAttribute, domain: OWLClass,
                                                owlDataProperty: OWLDataProperty,
                                                dataType: OWLDatatype,
                                                errorMessages: mutable.Buffer[String]): mutable.Buffer[String] =

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
        if manager.addAxiom(ontology, axiom) == UNSUCCESSFULLY then
          errorMessages += s"Could not add subclass axiom representing cardinalities [$minCardinality, $maxCardinality] " +
            s"for ${umlAttribute.attributeIdentity.getLabel}"
      case UMLMultiplicity(UMLNonNegativeCardinality(minCardinality), UMLInfiniteCardinality(_)) if minCardinality > 0 =>
        val axiom = dataFactory.getOWLSubClassOfAxiom(domain,
          dataFactory.getOWLDataMinCardinality(minCardinality, owlDataProperty, dataType))
        if manager.addAxiom(ontology, axiom) == UNSUCCESSFULLY then
          errorMessages += s"Could not add subclass axiom representing cardinalities for [$minCardinality, *] " +
            s"${umlAttribute.attributeIdentity.getLabel}"
      // Unreachable: the model rejects an unbounded min and substitutes [0,*], and UMLMultiplicity's
      // constructor is private so nothing can build one behind its back. Kept because the compiler
      // cannot prove that, and reported rather than ignored: if this ever does arrive, the ontology
      // would otherwise be silently weaker than the model, with nothing to say why.
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLNonNegativeCardinality(maxCardinality)) =>
        diagnostics.error(DiagnosticCode.UnboundedMinMultiplicity,
          s"Reached the writer with multiplicity [*, $maxCardinality] on attribute " +
            s"${umlAttribute.attributeIdentity.getLabel}, which the model should have rejected. No " +
            s"cardinality restriction was asserted.")
      case UMLMultiplicity(UMLNonNegativeCardinality(_), UMLInfiniteCardinality(_)) =>
      case UMLMultiplicity(UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) =>

    errorMessages


  private def createCompleteDisjointChildren(owlClass: OWLClass, generalizationSet: UMLGeneralizationSet,
                                             errorMessages: mutable.Buffer[String]) =
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
                                                errorMessages: mutable.Buffer[String]) =
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
                                               errorMessages: mutable.Buffer[String]) =
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
                                                  errorMessages: mutable.Buffer[String]): Unit =
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
                                         errorMessages: mutable.Buffer[String]): Unit =
    logger.trace(s"createDefinitionAnnotation: owlNamedObject=$owlNamedObject, definition=$definition, " +
      s"errorMessages=$errorMessages ${Code.source}")
    if definition.nonEmpty then
      val owlClassDefinitionAnnotationAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
        dataFactory.getOWLAnnotation(dataFactory.getRDFSComment(), dataFactory.getOWLLiteral(definition)))
      if manager.addAxiom(ontology, owlClassDefinitionAnnotationAxiom) == UNSUCCESSFULLY then
        errorMessages += s"Could not add definition=$definition for ${owlNamedObject.getIRI}"

  /**
   * Creates an RDFS label annotation if the label is not empty, otherwise uses the IRI of the OWLNamedObject
   * @param owlNamedObject
   * @param label
   * @param errorMessages
   * @return
   */
  private def createLabelAnnotation(owlNamedObject: OWLNamedObject,
                                    label: String,
                                    errorMessages: mutable.Buffer[String]): OWLAnnotationAssertionAxiom =
    logger.trace(s"createLabelAnnotation: owlNamedObject=$owlNamedObject, label=$label, " +
      s"errorMessages=$errorMessages ${Code.source}")
    if label.nonEmpty then
      createOWLAnnotationAssertionAxiom(owlNamedObject, dataFactory.getRDFSLabel(), label, errorMessages)
    else
      createOWLAnnotationAssertionAxiom(owlNamedObject, dataFactory.getRDFSLabel(), owlNamedObject.getIRI.getIRIString, errorMessages)

  private def createOWLAnnotationAssertionAxiom(owlNamedObject: OWLNamedObject,
                                                owlAnnotationProperty: OWLAnnotationProperty,
                                                literal: String,
                                                errorMessages: mutable.Buffer[String]): OWLAnnotationAssertionAxiom =
    logger.trace(s"createOWLAnnotationAssertionAxiom: owlNamedObject=$owlNamedObject, " +
      s"owlAnnotationProperty=$owlAnnotationProperty, literal=$literal, errorMessages=$errorMessages ${Code.source}")

    if literal.isBlank then
      throw new IllegalArgumentException("Literal cannot be blank")
    val owlAnnotationAssertionAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
      dataFactory.getOWLAnnotation(owlAnnotationProperty, dataFactory.getOWLLiteral(literal)))
    if manager.addAxiom(ontology, owlAnnotationAssertionAxiom) == UNSUCCESSFULLY then
      errorMessages += s"Could not add annotation assertion for ${owlNamedObject.getIRI} with literal=$literal"
    owlAnnotationAssertionAxiom


  private def createOWLClass(umlClass: UMLClassIdentity, errorMessages: mutable.Buffer[String]): OWLClass =
    val owlClass = dataFactory.getOWLClass(umlClass.getIRI)
    if manager.addAxiom(ontology, dataFactory.getOWLDeclarationAxiom(owlClass)) == UNSUCCESSFULLY then
      errorMessages += s"Could not declare class=${owlClass.getIRI}"
    owlClass

  /**
   * Drops from the global disjointness axiom any class that shares a subclass with another member of
   * that axiom. Two classes declared disjoint cannot have a common subclass — the subclass would be
   * unsatisfiable — and excluding a generalization set's children only covers the direct parent-child
   * pair, where the shared subclass is the child itself. Two ancestors of the same class are the case
   * left over, and they are what this removes.
   *
   * Eligibility is read from a snapshot taken before anything is removed, so the result does not
   * depend on the order classes happen to be visited in. All of a class's eligible ancestors are
   * dropped rather than the one that would suffice, because which one to keep would be arbitrary.
   * Single inheritance is untouched however deep it runs: a chain leaves only its root eligible, and
   * one class cannot contradict itself.
   */
  private def relaxGlobalDisjointnessForSharedSubclasses(): Unit =
    val parentIRIsByChildIRI = mutable.Map[String, mutable.Set[String]]()
    val labelsByIRI = mutable.Map[String, String]()
    classes.foreach { umlClass =>
      val parentIRI = umlClass.classIdentity.getIRI
      labelsByIRI += (parentIRI -> umlClass.classIdentity.getLabel)
      umlClass.children.setOfGeneralizationSets.foreach(_.generalizationSet.foreach { child =>
        parentIRIsByChildIRI.getOrElseUpdate(child.getIRI, mutable.Set[String]()) += parentIRI
      })
    }

    def ancestorsOf(classIRI: String): Set[String] =
      val ancestors = mutable.Set[String]()
      val pending = mutable.Stack[String]()
      pending.pushAll(parentIRIsByChildIRI.getOrElse(classIRI, mutable.Set[String]()))
      while pending.nonEmpty do
        val ancestor = pending.pop()
        // A generalization cycle would otherwise loop forever; `add` reports whether this is new.
        if ancestor != classIRI && ancestors.add(ancestor) then
          pending.pushAll(parentIRIsByChildIRI.getOrElse(ancestor, mutable.Set[String]()))
      ancestors.toSet

    val eligibleIRIs = disjointClassesNotPartOfGeneralizationSets.map(_.getIRI.toString).toSet
    parentIRIsByChildIRI.keys.toSeq.sorted.foreach { childIRI =>
      val contradictoryAncestorIRIs = ancestorsOf(childIRI).intersect(eligibleIRIs)
      if contradictoryAncestorIRIs.size > 1 then
        contradictoryAncestorIRIs.foreach(ancestorIRI =>
          disjointClassesNotPartOfGeneralizationSets -= dataFactory.getOWLClass(ancestorIRI))
        val childLabel = labelsByIRI.getOrElse(childIRI, childIRI)
        val ancestorLabels = contradictoryAncestorIRIs.toSeq
          .map(ancestorIRI => labelsByIRI.getOrElse(ancestorIRI, ancestorIRI)).sorted
        diagnostics.warn(DiagnosticCode.GlobalDisjointnessRelaxed,
          s"$childLabel inherits from ${ancestorLabels.mkString(" and ")}, which the default disjointness " +
            s"assumption would have declared mutually disjoint, making $childLabel unsatisfiable. They were " +
            s"left out of the global disjointness axiom. Use --no-global-disjointness to switch the " +
            s"assumption off entirely.")
    }

  private def createOWLDisjointClassesAxiom(disjointOWLClasses: mutable.Set[OWLClass],
                                            errorMessages: mutable.Buffer[String]): Unit =
    if disjointOWLClasses.size < 2 then
      logger.warn(s"createOWLDisjointClassesAxiom: disjointOWLClasses.size=${disjointOWLClasses.size} < 2")
    else
      val disjointClassesAxiom = dataFactory.getOWLDisjointClassesAxiom(disjointOWLClasses.asJava)
      if manager.addAxiom(ontology, disjointClassesAxiom) == UNSUCCESSFULLY then
        errorMessages += s"Could not add disjointClasses axiom for ${disjointOWLClasses.map(_.getIRI).mkString(", ")}"

  private def createOWLEquivalentToUnionAxiom(owlClass: OWLClass, completeOverlappingOWLClasses: mutable.Set[OWLClass],
                                          errorMessages: mutable.Buffer[String]): Unit =
    if completeOverlappingOWLClasses.size < 2 then
      logger.warn(s"createOWLEquivalentToUnionAxiom: completeOverlappingOWLClasses.size=${completeOverlappingOWLClasses.size} < 2")
    else
      val equivalentToUnionAxiom = dataFactory.getOWLEquivalentClassesAxiom(owlClass,
        dataFactory.getOWLObjectUnionOf(completeOverlappingOWLClasses.asJava))
      if manager.addAxiom(ontology, equivalentToUnionAxiom) == UNSUCCESSFULLY then
        errorMessages += s"Could not add overlappingUnion axiom for ${owlClass.getIRI}" +
          s" equivalent to union of ${completeOverlappingOWLClasses.map(_.getIRI).mkString(", ")}"

  private def createOWLDisjointUnionAxiom(owlClass: OWLClass, completeDisjointOWLClasses: mutable.Set[OWLClass],
                                       errorMessages: mutable.Buffer[String]): OWLDisjointUnionAxiom =
    val disjointUnionAxiom = dataFactory.getOWLDisjointUnionAxiom(owlClass, completeDisjointOWLClasses.asJava)
    if manager.addAxiom(ontology, disjointUnionAxiom) == UNSUCCESSFULLY then
      errorMessages += s"Could not add disjointUnion axiom for ${owlClass.getIRI}" +
        s" equivalent to disjoint union of ${completeDisjointOWLClasses.map(_.getIRI).mkString(", ")}"
    disjointUnionAxiom


  private def createOWLSubClassAxiom(childClass: OWLClass, superClass: OWLClass,
                                  errorMessages: mutable.Buffer[String]): OWLSubClassOfAxiom =
    val subClassOfAxiom = dataFactory.getOWLSubClassOfAxiom(childClass, superClass)
    if manager.addAxiom(ontology, subClassOfAxiom) == UNSUCCESSFULLY then
      errorMessages += s"Could not add subClassOf axiom for subclass=${childClass.getIRI}, superclass=${superClass.getIRI}"
    subClassOfAxiom


  private def processUMLClasses: mutable.Buffer[String] =
    logger.info("processUMLClasses: Start")
    val errorMessages: mutable.Buffer[String] = new ArrayBuffer[String]()
    classes.foreach(umlClass =>
        val owlClass = createAndAnnotateOWLClass(umlClass, errorMessages))
    if globalDisjointness then
      relaxGlobalDisjointnessForSharedSubclasses()
      createOWLDisjointClassesAxiom(disjointClassesNotPartOfGeneralizationSets, errorMessages)
    logger.info("processUMLClasses: Done")
    errorMessages
  end processUMLClasses

  private def processUMLAttributes: mutable.Buffer[String] =
    logger.info("processUMLAttributes: Start")
    val errorMessages: mutable.Buffer[String] = new ArrayBuffer[String]()
    attributes.foreach(umlAttribute =>
      val owlAttribute = createAndAnnotateOWLAttribute(umlAttribute, errorMessages))
    logger.info("processUMLAttributes: Done")
    errorMessages



  /**
   * Builds the ontology and writes it to the output file, returning everything that went wrong.
   * A failed axiom does not abort the run: the axiom is dropped, a diagnostic is recorded, and
   * the rest of the ontology is still written.
   */
  def generateOWL: Seq[Diagnostic] =
    logger.info("generateOWL: Start")
    val errorMessages = new ListBuffer[String]()
    errorMessages.appendAll(processUMLClasses)
    errorMessages.appendAll(processUMLAttributes)
    errorMessages.foreach(diagnostics.error(DiagnosticCode.AxiomAdditionFailed, _))
    try
      manager.saveOntology(ontology, new RDFXMLDocumentFormat(), IRI.create(owlOntologyFile))
    catch
      case e: OWLOntologyStorageException =>
        diagnostics.error(DiagnosticCode.OntologySaveFailed,
          s"Could not save the ontology to $owlOntologyFile: ${e.getMessage}")
    logger.info("generateOWL: Done")
    diagnostics.toSeq
  end generateOWL
  
end UML2OWLWriter
