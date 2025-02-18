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
import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Seq}
import scala.util.Try
import scala.jdk.CollectionConverters.*

class UML2OWLWriter(ontologyIRI: String,
                    owlOntologyFile: File,
                    classes: Set[UMLClass]):

  private val logger = Logger(this.getClass)
  private val manager = OWLManager.createOWLOntologyManager
  private val ontology = manager.createOntology(IRI.create(ontologyIRI))
  private val dataFactory = manager.getOWLDataFactory

  private val completeDisjointClasses: mutable.Map[IRI, mutable.Set[IRI]] = mutable.Map[IRI, mutable.Set[IRI]]()
  private val completeOverlappingClasses: mutable.Map[IRI, mutable.Set[IRI]] = mutable.Map[IRI, mutable.Set[IRI]]()
  private val incompleteDisjointClasses: mutable.Map[IRI, mutable.Set[IRI]] = mutable.Map[IRI, mutable.Set[IRI]]()
  private val incompleteOverlappingClasses: mutable.Map[IRI, mutable.Set[IRI]] = mutable.Map[IRI, mutable.Set[IRI]]()

  private val disjointClasses: mutable.Set[IRI] = mutable.Set[IRI]()

  /**
   * Creates an OWLClass for a UMLClass and annotates it with definition and label.
   * It also creates all children of this class.
   *
   * @param umlClass
   * @param errorMessages
   * @return
   */
  private def createAndAnnotateOWLClass(umlClass: UMLClass,
                                        errorMessages: mutable.Seq[String]): OWLClass =
    logger.debug(s"createAndAnnotateOWLClass: umlClass=$umlClass, errorMessages=$errorMessages ${Code.source}")
    val owlClass = createOWLClass(umlClass.classIdentity, errorMessages)
    disjointClasses += owlClass.getIRI
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

    umlClass.classDefinition.definitionOption.foreach(definition =>
      createDefinitionAnnotation(owlClass, definition, errorMessages))
    createLabelAnnotation(owlClass, umlClass.classIdentity.getLabel, errorMessages)
    owlClass


  private def createCompleteDisjointChildren(owlClass: OWLClass, generalizationSet: UMLGeneralizationSet,
                                             errorMessages: Seq[String]) =
    val completeDisjointClassesAsIRIs = mutable.Set[IRI]()
    val completeDisjointOWLClasses = mutable.Set[OWLClass]()
    generalizationSet.generalizationSet.foreach { child =>
      val childClass = createOWLClass(child, errorMessages)
      completeDisjointOWLClasses += childClass
      completeDisjointClassesAsIRIs += childClass.getIRI
      createOWLSubClassAxiom(childClass, owlClass, errorMessages)
    }
    createOWLDisjointUnionAxiom(owlClass, completeDisjointOWLClasses, errorMessages)
    completeDisjointClasses += (owlClass.getIRI -> completeDisjointClassesAsIRIs)


  private def createCompleteOverlappingChildren(owlClass: OWLClass, generalizationSet: UMLGeneralizationSet,
                                                errorMessages: Seq[String]) =
    val completeOverlappingClassesAsIRIs = mutable.Set[IRI]()
    val completeOverlappingOWLClasses = mutable.Set[OWLClass]()
    generalizationSet.generalizationSet.foreach { child =>
      val childClass: OWLClass = createOWLClass(child, errorMessages)
      completeOverlappingOWLClasses += childClass
      completeOverlappingClassesAsIRIs += childClass.getIRI
      createOWLSubClassAxiom(childClass, owlClass, errorMessages)
    }
    createOWLEquivalentToUnionAxiom(owlClass, completeOverlappingOWLClasses, errorMessages)
    completeOverlappingClasses += (owlClass.getIRI -> completeOverlappingClassesAsIRIs)

  private def createIncompleteDisjointChildren(owlClass: OWLClass, generalizationSet: UMLGeneralizationSet,
                                               errorMessages: Seq[String]) =
    val incompleteDisjointClassesAsIRIs = mutable.Set[IRI]()
    val incompleteDisjointOWLClasses = mutable.Set[OWLClass]()
    generalizationSet.generalizationSet.foreach { child =>
      val childClass = createOWLClass(child, errorMessages)
      incompleteDisjointOWLClasses += childClass
      incompleteDisjointClassesAsIRIs += childClass.getIRI
      createOWLSubClassAxiom(childClass, owlClass, errorMessages)
    }
    createOWLDisjointUnionAxiom(owlClass, incompleteDisjointOWLClasses, errorMessages)
    incompleteDisjointClasses += (owlClass.getIRI -> incompleteDisjointClassesAsIRIs)


  private def createIncompleteOverlappingChildren(owlClass: OWLClass, generalizationSet: UMLGeneralizationSet,
                                                  errorMessages: Seq[String]): Unit =
    val incompleteOverlappingClassesAsIRIs = mutable.Set[IRI]()
    generalizationSet.generalizationSet.foreach { child =>
      val childClass = createOWLClass(child, errorMessages)
      incompleteOverlappingClassesAsIRIs += childClass.getIRI
      createOWLSubClassAxiom(childClass, owlClass, errorMessages)
    }
    incompleteOverlappingClasses += (owlClass.getIRI -> incompleteOverlappingClassesAsIRIs)



  private def createDefinitionAnnotation(owlNamedObject: OWLNamedObject,
                                         definition: String,
                                         errorMessages: mutable.Seq[String]): Unit =
    logger.debug(s"createDefinitionAnnotation: owlNamedObject=$owlNamedObject, definition=$definition, " +
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
    logger.debug(s"createLabelAnnotation: owlNamedObject=$owlNamedObject, label=$label, " +
      s"errorMessages=$errorMessages ${Code.source}")
    if label.nonEmpty then
      createOWLAnnotationAssertionAxiom(owlNamedObject, dataFactory.getRDFSLabel(), label, errorMessages)
    else
      createOWLAnnotationAssertionAxiom(owlNamedObject, dataFactory.getRDFSLabel(), owlNamedObject.getIRI.getIRIString, errorMessages)

  private def createOWLAnnotationAssertionAxiom(owlNamedObject: OWLNamedObject,
                                                owlAnnotationProperty: OWLAnnotationProperty, literal: String,
                                                errorMessages: mutable.Seq[String]): OWLAnnotationAssertionAxiom =
    logger.debug(s"createOWLAnnotationAssertionAxiom: owlAnnotationProperty=$owlAnnotationProperty, literal=$literal, " +
      s"errorMessages=$errorMessages ${Code.source}")
    if literal.isBlank then
      throw new IllegalArgumentException("Literal cannot be blank")
    val owlAnnotationAssertionAxiom = dataFactory.getOWLAnnotationAssertionAxiom(owlNamedObject.getIRI,
      dataFactory.getOWLAnnotation(owlAnnotationProperty, dataFactory.getOWLLiteral(literal)))
    if manager.addAxiom(ontology, owlAnnotationAssertionAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not add annotation assertion for ${owlNamedObject.getIRI} with literal=$literal"
    owlAnnotationAssertionAxiom


  private def createOWLClass(child: UMLClassIdentity, errorMessages: mutable.Seq[String]): OWLClass =
    val owlClass = dataFactory.getOWLClass(child.getIRI)
    if manager.addAxiom(ontology, dataFactory.getOWLDeclarationAxiom(owlClass)) != SUCCESSFULLY then
      errorMessages :+ s"Could not declare class=${owlClass.getIRI}"
    owlClass

  private def createOWLDisjointClassesAxiom(disjointOWLClasses: mutable.Set[OWLClass],
                                            errorMessages: mutable.Seq[String]): OWLDisjointClassesAxiom =
    val disjointClassesAxiom = dataFactory.getOWLDisjointClassesAxiom(disjointOWLClasses.asJava)
    if manager.addAxiom(ontology, disjointClassesAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not add disjointClasses axiom for ${disjointOWLClasses.map(_.getIRI).mkString(", ")}"
    disjointClassesAxiom

  private def createOWLEquivalentToUnionAxiom(owlClass: OWLClass, completeOverlappingOWLClasses: mutable.Set[OWLClass],
                                          errorMessages: mutable.Seq[String]): OWLEquivalentClassesAxiom =
    val equivalentToUnionAxiom = dataFactory.getOWLEquivalentClassesAxiom(owlClass,
      dataFactory.getOWLObjectUnionOf(completeOverlappingOWLClasses.asJava))
    if manager.addAxiom(ontology, equivalentToUnionAxiom) != SUCCESSFULLY then
      errorMessages :+ s"Could not add overlappingUnion axiom for ${owlClass.getIRI}" +
        s" equivalent to union of ${completeOverlappingOWLClasses.map(_.getIRI).mkString(", ")}"
    equivalentToUnionAxiom

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
    logger.info("processUMLClasses: Done")
    errorMessages
  end processUMLClasses

  def generateOWL: Either[String, ListBuffer[String]] =
    logger.info("generateOWL: Start")
    var errorMessages = new ListBuffer[String]()
    errorMessages.appendAll(processUMLClasses)
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
