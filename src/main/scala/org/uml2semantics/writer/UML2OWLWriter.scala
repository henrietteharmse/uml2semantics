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
                    enumerations: Set[UMLEnumeration],
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
   * The classes that take part in the global disjointness axiom: those with no parents, less any that
   * share a descendant with another root. A class with a parent is left out because disjointness within
   * a hierarchy is the generalization set's business, and because a class is never disjoint from its own
   * superclass. A class having *children* does not exclude it — that says nothing about how it relates
   * to classes outside its hierarchy.
   *
   * The class of every enumeration takes part too, less any that share a value with another
   * enumeration. An enumeration has no parents, a value is never an instance of a UML class, and a
   * value belongs to one enumeration.
   */
  private val rootClassesForGlobalDisjointness: mutable.Set[OWLClass] = mutable.Set[OWLClass]()

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
      rootClassesForGlobalDisjointness += owlClass

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
      s"rootClassesForGlobalDisjointness=$rootClassesForGlobalDisjointness ${Code.source}")
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
        // An enumeration is translated to a class, so an attribute typed by one ranges over that class.
        case UMLEnumerationType(attributeType) =>
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

  /** An object property ranging over the class at the IRI of `attributeType`. */
  private def createObjectProperty(umlAttribute: UMLAttribute, domain: OWLClass, attributeType: LabeledIRI,
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
      rootClassesForGlobalDisjointness -= childClass
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
      rootClassesForGlobalDisjointness -= childClass
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
      rootClassesForGlobalDisjointness -= childClass
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
      rootClassesForGlobalDisjointness -= childClass
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
   * Enforces the second half of the global disjointness rule: root classes are mutually disjoint
   * *except where two roots share a descendant*. Two classes declared disjoint cannot have a common
   * subclass, or that subclass is unsatisfiable, and two roots can perfectly well share one. Those
   * roots are what this removes.
   *
   * Descendants, not children: a root can contradict another through a grandchild, so the test is
   * whether a class has more than one root among its *ancestors*. Where it does, all of them are
   * dropped rather than the one that would suffice, because which one to keep would be arbitrary.
   * Single inheritance is untouched however deep it runs — a chain has exactly one root, and one
   * class cannot contradict itself.
   *
   * Membership is read from a snapshot taken before anything is removed. Being a root is static, so
   * this changes no outcome; it only keeps the loop from reading a set it is modifying.
   */
  private def relaxGlobalDisjointnessForSharedDescendants(): Unit =
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

    val rootIRIs = rootClassesForGlobalDisjointness.map(_.getIRI.toString).toSet
    parentIRIsByChildIRI.keys.toSeq.sorted.foreach { descendantIRI =>
      val contradictoryRootIRIs = ancestorsOf(descendantIRI).intersect(rootIRIs)
      if contradictoryRootIRIs.size > 1 then
        contradictoryRootIRIs.foreach(rootIRI =>
          rootClassesForGlobalDisjointness -= dataFactory.getOWLClass(rootIRI))
        val childLabel = labelsByIRI.getOrElse(descendantIRI, descendantIRI)
        val ancestorLabels = contradictoryRootIRIs.toSeq
          .map(rootIRI => labelsByIRI.getOrElse(rootIRI, rootIRI)).sorted
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


  /**
   * Translates an enumeration to a class that is equivalent to the one-of of its values. Each value
   * is a named individual of that class, and the values are declared pairwise different, since OWL
   * makes no unique name assumption and two literals of an enumeration are never the same thing.
   *
   * An enumeration without values gets the class and nothing more. An empty one-of is not valid
   * OWL 2, and it would assert that the class is empty, which is not what an enumeration that is
   * still being drafted means. A single value has nothing to be different from, so that axiom needs
   * at least two.
   */
  private def createAndAnnotateOWLEnumeration(umlEnumeration: UMLEnumeration,
                                              errorMessages: mutable.Buffer[String]): OWLClass =
    logger.trace(s"createAndAnnotateOWLEnumeration: umlEnumeration=$umlEnumeration ${Code.source}")
    val owlClass = dataFactory.getOWLClass(umlEnumeration.getIRI)
    if manager.addAxiom(ontology, dataFactory.getOWLDeclarationAxiom(owlClass)) == UNSUCCESSFULLY then
      errorMessages += s"Could not declare class=${owlClass.getIRI}"
    umlEnumeration.definition.foreach(enumerationDefinition =>
      createDefinitionAnnotation(owlClass, enumerationDefinition.definition, errorMessages))
    createLabelAnnotation(owlClass, umlEnumeration.getLabel, errorMessages)
    rootClassesForGlobalDisjointness += owlClass

    val owlIndividuals = umlEnumeration.values.map { umlValue =>
      val owlIndividual = dataFactory.getOWLNamedIndividual(umlEnumeration.getValueIRI(umlValue))
      if manager.addAxiom(ontology, dataFactory.getOWLDeclarationAxiom(owlIndividual)) == UNSUCCESSFULLY then
        errorMessages += s"Could not declare individual=${owlIndividual.getIRI}"
      umlValue.definition.foreach(valueDefinition =>
        createDefinitionAnnotation(owlIndividual, valueDefinition.definition, errorMessages))
      createLabelAnnotation(owlIndividual, umlValue.getLabel, errorMessages)
      if manager.addAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(owlClass, owlIndividual)) == UNSUCCESSFULLY then
        errorMessages += s"Could not assert individual=${owlIndividual.getIRI} to be of class=${owlClass.getIRI}"
      owlIndividual
    }

    if owlIndividuals.nonEmpty then
      val equivalentToOneOfAxiom = dataFactory.getOWLEquivalentClassesAxiom(owlClass,
        dataFactory.getOWLObjectOneOf(owlIndividuals.asJava))
      if manager.addAxiom(ontology, equivalentToOneOfAxiom) == UNSUCCESSFULLY then
        errorMessages += s"Could not add equivalentClasses axiom for ${owlClass.getIRI} equivalent to one of " +
          owlIndividuals.map(_.getIRI).mkString(", ")
    if owlIndividuals.size > 1 then
      val differentIndividualsAxiom = dataFactory.getOWLDifferentIndividualsAxiom(owlIndividuals.asJava)
      if manager.addAxiom(ontology, differentIndividualsAxiom) == UNSUCCESSFULLY then
        errorMessages += s"Could not add differentIndividuals axiom for ${owlIndividuals.map(_.getIRI).mkString(", ")}"
    owlClass

  /**
   * Two enumerations declared disjoint cannot share a value, or the ontology is inconsistent: the
   * shared individual would belong to two classes that have nothing in common. Name-only values
   * never collide, because each is qualified by its enumeration, but two enumerations can be given
   * the same curie for a value. Those enumerations are what this removes, all of them rather than
   * all but one, because which to keep would be arbitrary.
   */
  private def relaxGlobalDisjointnessForSharedValues(): Unit =
    val enumerationsByValueIRI = mutable.Map[String, mutable.Set[UMLEnumeration]]()
    enumerations.foreach(umlEnumeration => umlEnumeration.values.foreach(umlValue =>
      enumerationsByValueIRI.getOrElseUpdate(umlEnumeration.getValueIRI(umlValue), mutable.Set()) += umlEnumeration))

    val sharedValueLabelsByEnumerations = mutable.Map[Set[UMLEnumeration], mutable.Set[String]]()
    enumerationsByValueIRI.foreach { (valueIRI, sharingEnumerations) =>
      if sharingEnumerations.size > 1 then
        val valueLabel = sharingEnumerations.head.values.find(sharingEnumerations.head.getValueIRI(_) == valueIRI)
          .map(_.getLabel).getOrElse(valueIRI)
        sharedValueLabelsByEnumerations.getOrElseUpdate(sharingEnumerations.toSet, mutable.Set()) += valueLabel
    }

    sharedValueLabelsByEnumerations.toSeq.sortBy((sharing, _) => sharing.toSeq.map(_.getLabel).sorted.mkString(" "))
      .foreach { (sharingEnumerations, valueLabels) =>
        sharingEnumerations.foreach(umlEnumeration =>
          rootClassesForGlobalDisjointness -= dataFactory.getOWLClass(umlEnumeration.getIRI))
        diagnostics.warn(DiagnosticCode.GlobalDisjointnessRelaxed,
          s"Enumerations ${sharingEnumerations.toSeq.map(_.getLabel).sorted.mkString(" and ")} share the " +
            s"value ${valueLabels.toSeq.sorted.mkString(", ")}, so the default disjointness assumption would " +
            s"have made the ontology inconsistent. They were left out of the global disjointness axiom. Use " +
            s"--no-global-disjointness to switch the assumption off entirely.")
      }

  private def processUMLClasses: mutable.Buffer[String] =
    logger.info("processUMLClasses: Start")
    val errorMessages: mutable.Buffer[String] = new ArrayBuffer[String]()
    classes.foreach(umlClass =>
        val owlClass = createAndAnnotateOWLClass(umlClass, errorMessages))
    logger.info("processUMLClasses: Done")
    errorMessages
  end processUMLClasses

  private def processUMLEnumerations: mutable.Buffer[String] =
    logger.info("processUMLEnumerations: Start")
    val errorMessages: mutable.Buffer[String] = new ArrayBuffer[String]()
    enumerations.foreach(umlEnumeration => createAndAnnotateOWLEnumeration(umlEnumeration, errorMessages))
    logger.info("processUMLEnumerations: Done")
    errorMessages

  /**
   * Declares the roots mutually disjoint. This runs once classes and enumerations have both been
   * translated, because the one axiom spans both.
   */
  private def processGlobalDisjointness: mutable.Buffer[String] =
    val errorMessages: mutable.Buffer[String] = new ArrayBuffer[String]()
    if globalDisjointness then
      relaxGlobalDisjointnessForSharedDescendants()
      relaxGlobalDisjointnessForSharedValues()
      createOWLDisjointClassesAxiom(rootClassesForGlobalDisjointness, errorMessages)
    errorMessages

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
    errorMessages.appendAll(processUMLEnumerations)
    errorMessages.appendAll(processGlobalDisjointness)
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
