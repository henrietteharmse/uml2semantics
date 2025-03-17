package org.uml2semantics.model

sealed trait UMLAttributeIdentifier(classIdentifier: UMLClassIdentifier) extends UMLNamedElement

case class UMLAttributeName(classIdentifier: UMLClassIdentifier, name: String) extends UMLAttributeIdentifier(classIdentifier):
  def isEmpty: Boolean = name.isEmpty
  def nonEmpty: Boolean = name.nonEmpty
  def getName: String = classIdentifier.getName + FRAGMENT_SEPARATOR + name

case class UMLAttributeCurie(classIdentifier: UMLClassIdentifier, curie: Curie) extends UMLAttributeIdentifier(classIdentifier):
  def isEmpty: Boolean = curie.isEmpty
  def nonEmpty: Boolean = curie.nonEmpty
  def getName: String = classIdentifier.getName + FRAGMENT_SEPARATOR + curie


case class UMLAttributeIdentity(classIdentity: UMLClassIdentity,
                                nameOption: Option[UMLAttributeName] = None,
                                curieOption: Option[UMLAttributeCurie] = None) extends UMLIdentity:
  def getIRI: String =
    (curieOption, nameOption) match
      case (Some(curie), _) => curie.curie.toIRI
      case (_, Some(name)) => val iri = classIdentity.ontologyPrefix.prefixIRI.iri
        iri + (if iri.matches(".*[#]$") then name.getName
          else if iri.matches(".*[/]$") then classIdentity.getLabel + FRAGMENT_SEPARATOR + name.getName
          else  PATH_SEPARATOR + classIdentity.getLabel + FRAGMENT_SEPARATOR + name.getName)
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")

  def getLabel: String =
    (curieOption, nameOption) match
      case (_, Some(name)) => name.getName
      case (Some(curie), _) => curie.getName
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")

