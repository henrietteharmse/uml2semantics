package org.uml2semantics.model

import org.uml2semantics.model.cache.AttributeIdentityBuilderCache

sealed trait UMLAttributeIdentifier(classIdentifier: UMLClassIdentifier) extends UMLNamedElement

case class UMLAttributeName(classIdentifier: UMLClassIdentifier, name: String) extends UMLAttributeIdentifier(classIdentifier):
  def isEmpty: Boolean = name.isEmpty
  def nonEmpty: Boolean = name.nonEmpty
  def getName: String = classIdentifier.getName + FRAGMENT_SEPARATOR + name

object UMLAttributeName:
  def apply(classIdentifier: UMLClassIdentifier, name: String): Option[UMLAttributeName] =
    Option.when(classIdentifier.nonEmpty && !name.isBlank && !name.contains(':'))
      (new UMLAttributeName(classIdentifier, name))

case class UMLAttributeCurie(classIdentifier: UMLClassIdentifier, curie: Curie) extends UMLAttributeIdentifier(classIdentifier):
  def isEmpty: Boolean = curie.isEmpty
  def nonEmpty: Boolean = curie.nonEmpty
  def getName: String = classIdentifier.getName + FRAGMENT_SEPARATOR + curie

object UMLAttributeCurie:
  def apply(classIdentifier: UMLClassIdentifier, curie: String): Option[UMLAttributeCurie] =
    if classIdentifier.nonEmpty then
      Curie.fromString(curie).map(new UMLAttributeCurie(classIdentifier: UMLClassIdentifier, _))
    else None


case class UMLAttributeIdentity(classIdentity: UMLClassIdentity,
                                nameOption: Option[UMLAttributeName] = None,
                                curieOption: Option[UMLAttributeCurie] = None) extends UMLIdentity:
  def getIRI: String =
    (curieOption, nameOption) match
      case (Some(curie), _) => curie.curie.toIRI
      case (_, Some(name)) => val iri = classIdentity.ontologyPrefix.prefixIRI.iri
        iri + (if iri.matches(".*#$") then name.getName
          else if iri.matches(".*/$") then classIdentity.getLabel + FRAGMENT_SEPARATOR + name.getName
          else  PATH_SEPARATOR + classIdentity.getLabel + FRAGMENT_SEPARATOR + name.getName)
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")

  def getLabel: String =
    (curieOption, nameOption) match
      case (_, Some(name)) => name.getName
      case (Some(curie), _) => curie.getName
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")

object UMLAttributeIdentity:
  def apply(classIdentity: UMLClassIdentity, attributeIdentifier: String): UMLAttributeIdentity =
    (UMLAttributeCurie(classIdentity.getClassIdentifier, attributeIdentifier),
      UMLAttributeName(classIdentity.getClassIdentifier, attributeIdentifier)) match
      case (Some(curie), _) => UMLAttributeIdentity(classIdentity, None, Some(curie))
      case (_, Some(name)) => UMLAttributeIdentity(classIdentity, Some(name), None)
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")
      
//  def apply(classIdentifier: UMLClassIdentifier, attributeIdentifier: UMLAttributeIdentifier):  UMLAttributeIdentity =
//    val classIdentity = UMLClassIdentity(classIdentifier)
//    attributeIdentifier match
//      case name: UMLAttributeName => UMLAttributeIdentity(classIdentifier, Some(name), None)
//      case curie: UMLAttributeCurie => UMLAttributeIdentity(classIdentifier, None, Some(curie))


  class AttributeIdentityBuilder(var prefixNamespace: PrefixNamespace):
    protected var classIdentityBuilder = UMLClassIdentity.builder(prefixNamespace)
    protected var nameOption: Option[String] = None
    protected var curieOption: Option[String] = None

    def withClassName(name: String): AttributeIdentityBuilder =
      this.classIdentityBuilder = classIdentityBuilder.withName(name)
      this

    def withClassCurie(curie: String): AttributeIdentityBuilder =
      this.classIdentityBuilder = classIdentityBuilder.withCurie(curie)
      this

    def withName(name: String): AttributeIdentityBuilder =
      this.nameOption = Some(name)
      this

    def withCurie(curieAsString: String): AttributeIdentityBuilder =
      this.curieOption = Some(curieAsString)
      this

    def withNameAndCurie(name: String, curie: String): AttributeIdentityBuilder =
      this.nameOption = Some(name)
      this.curieOption = Some(curie)
      this

    def withNameOrCurie(nameOrCurie: String): AttributeIdentityBuilder =
      if nameOrCurie.contains(':') then
        this.curieOption = Some(nameOrCurie)
      else
        this.nameOption = Some(nameOrCurie)
      this

    def build: UMLAttributeIdentity =
      val classIdentity = classIdentityBuilder.build

      if nameOption.isEmpty && curieOption.isEmpty then
        throw new IllegalArgumentException("Name and curie must not be empty.")

      val attributeNameOption: Option[UMLAttributeName] = nameOption.flatMap(UMLAttributeName(
        classIdentity.getClassIdentifier, _))
      val attributeCurieOption: Option[UMLAttributeCurie] = curieOption.flatMap(UMLAttributeCurie(
        classIdentity.getClassIdentifier, _))
      val attributeIdentity: UMLAttributeIdentity = UMLAttributeIdentity(classIdentity, attributeNameOption, attributeCurieOption)
      if attributeNameOption.isDefined && attributeCurieOption.isDefined then
        AttributeIdentityBuilderCache.cacheUMLAttributeIdentity(attributeIdentity,  this)
        return attributeIdentity

      val attributeIdentityBuilderOption =
        AttributeIdentityBuilderCache.getUMLAttributeIdentityBuilder(classIdentity, attributeIdentity)
      if attributeIdentityBuilderOption.isDefined then
        attributeIdentityBuilderOption.get.build
      else
        attributeIdentity





