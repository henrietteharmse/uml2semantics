package org.uml2semantics.model

trait UMLNamedElement:
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def getName: String
