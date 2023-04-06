package org.uml2owl

import org.semanticweb.owlapi.apibinding.OWLManager

def generateOWL(umlClassDiagram: UmlClassDiagram): Unit =
  val manager = OWLManager.createOWLOntologyManager
  val  dataFactory = manager.getOWLDataFactory
//  umlClassDiagram.umlClasses.foreach(umlClass => dataFactory.)

//  val manager = OWLManager.createOWLOntologyManager
//ontology = manager.loadOntologyFromOntologyDocument(ontologyFile)
//dataFactory = manager.getOWLDataFactory
//  0