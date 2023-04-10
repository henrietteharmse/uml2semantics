package org.uml2owl

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddAxiom, IRI, OWLSubClassOfAxiom}
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat

import java.io.File
import scala.collection.mutable

def generateOWL(umlClassDiagram: UmlClassDiagram): Unit =
  val logger = Logger("generateOWL")
  val manager = OWLManager.createOWLOntologyManager
  logger.trace(s"umlClassDiagram.owlOntologyFile=${umlClassDiagram.owlOntologyFile}")
//  val file = new File(umlClassDiagram.ontologyIRI)
//  val iri = IRI.create(file)
  val ontology = manager.createOntology(IRI.create(umlClassDiagram.ontologyIRI))
  val dataFactory = manager.getOWLDataFactory
  umlClassDiagram.umlClasses.keySet.foreach(id => {
    val umlClass = umlClassDiagram.umlClasses.get(id)
    val owlClass = dataFactory.getOWLClass(umlClass.get.id.asIRI(umlClassDiagram.ontologyIRI))
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

//  val manager = OWLManager.createOWLOntologyManager
//ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)
//dataFactory = manager.getOWLDataFactory
//  0