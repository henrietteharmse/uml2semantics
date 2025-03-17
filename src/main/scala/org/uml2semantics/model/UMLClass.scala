package org.uml2semantics.model

import org.uml2semantics.model.cache.{ClassBuilderCache, ClassIdentityBuilderCache}

import scala.collection.mutable

sealed trait UMLClassIdentifier extends UMLNamedElement

case class UMLClassName(name: String) extends UMLClassIdentifier:
  override def isEmpty: Boolean = name.isEmpty
  override def nonEmpty: Boolean = name.nonEmpty
  override def getName: String = name

object UMLClassName:
  def apply (name: String): Option[UMLClassName] =
    Option.when(!name.isBlank && !name.contains(':'))(new UMLClassName(name))

case class UMLClassCurie(curie: Curie) extends UMLClassIdentifier:
  override def isEmpty: Boolean = curie.isEmpty
  override def nonEmpty: Boolean = curie.nonEmpty
  override def getName: String = curie.curie

object UMLClassCurie:
  def apply(curie: String): Option[UMLClassCurie] =
    Curie.fromString(curie).map(new UMLClassCurie(_))

case class UMLClassIdentity(nameOption: Option[UMLClassName] = None,
                            curieOption: Option[UMLClassCurie] = None,
                            ontologyPrefix: PrefixNamespace) extends UMLIdentity:

  def getIRI: String =
    (curieOption, nameOption) match
      case (Some(curie), _) => curie.curie.toIRI
      case (_, Some(name)) =>   val iri = ontologyPrefix.prefixIRI.iri
        iri + (if iri.matches(".*[/#]$") then name.getName else "/" + name.getName)
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")

  def getLabel: String =
    (curieOption, nameOption) match
      case (_, Some(name)) => name.getName
      case (Some(curie), _) => curie.getName
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")

object UMLClassIdentity:

  def apply(identifier: String, prefixNamespace: PrefixNamespace): UMLClassIdentity =
    (UMLClassCurie(identifier), UMLClassName(identifier)) match
      case (Some(curie), _) => UMLClassIdentity(None, Some(curie), prefixNamespace)
      case (_, Some(name)) => UMLClassIdentity(Some(name), None, prefixNamespace)
      case _ => throw new IllegalArgumentException("Name and curie must not be empty.")

  def apply(identifier: UMLClassIdentifier, prefixNamespace: PrefixNamespace): UMLClassIdentity =
    identifier match
      case name: UMLClassName => UMLClassIdentity(Some(name), None, prefixNamespace)
      case curie: UMLClassCurie => UMLClassIdentity(None, Some(curie), prefixNamespace)


  def builder(prefixNamespace: PrefixNamespace): ClassIdentityBuilder =
    ClassIdentityBuilder(prefixNamespace)

  class ClassIdentityBuilder(var prefixNamespace: PrefixNamespace):
    protected var name: Option[UMLClassName] = None
    protected var curie: Option[UMLClassCurie] = None

    def withName(name: String): ClassIdentityBuilder =
      this.name = UMLClassName(name)
      this

    def withCurie(curieAsString: String): ClassIdentityBuilder =
      this.curie = UMLClassCurie(curieAsString)
      this

    def withNameAndCurie(name: String, curie: String): ClassIdentityBuilder =
      this.name = UMLClassName(name)
      this.curie = UMLClassCurie(curie)
      this


    def withNameOrCurie(nameOrCurie: String): ClassIdentityBuilder =
      if nameOrCurie.contains(':') then
        this.curie = UMLClassCurie(nameOrCurie)
      else
        this.name = UMLClassName(nameOrCurie)
      this

    def build: UMLClassIdentity =
      if name.isEmpty && curie.isEmpty then
        throw new IllegalArgumentException("Name and curie must not be empty.")

      val classIdentity = UMLClassIdentity(name, curie, prefixNamespace)
      val classIdentityByNameOption = name.flatMap(ClassIdentityBuilderCache.getUMLClassIdentity)
      val classIdentityByCurieOption = curie.flatMap(ClassIdentityBuilderCache.getUMLClassIdentity)

      val classIdentityToReturn = (classIdentity.nameOption, classIdentity.curieOption) match
        case (Some(_), Some(_)) => classIdentity
        case (None, Some(_)) if classIdentityByNameOption.isDefined =>
          UMLClassIdentity(classIdentityByNameOption.get.nameOption, curie, prefixNamespace)
        case (Some(_), None) if classIdentityByCurieOption.isDefined =>
          UMLClassIdentity(name, classIdentityByCurieOption.get.curieOption, prefixNamespace)
        case _ => classIdentity

      ClassIdentityBuilderCache.cacheUMLClassIdentity(classIdentityToReturn, this)
      classIdentityToReturn


case class UMLClassDefinition(definition: String)


enum CoveringConstraint:
  case Complete
  case Incomplete

enum DisjointConstraint:
  case Disjoint
  case Overlapping


/**
 * @Todo: Add support for generalizationSet name.
 */
case class UMLGeneralizationSet(generalizationSet: Set[UMLClassIdentity],
                                coveringConstraint: CoveringConstraint = CoveringConstraint.Incomplete,
                                disjointConstraint: DisjointConstraint = DisjointConstraint.Disjoint)

object UMLGeneralizationSet:
  def builder(prefixNamespace: PrefixNamespace): GeneralizationSetBuilder =
    GeneralizationSetBuilder(prefixNamespace)

  /**
   *  According to the UML specification, the default values covering and disjoint are
   *  incomplete and overlapping respectively.
   *
   * Note: We keep track of the related parent. For now it will most likely be used for debugging purposes.
   * @param prefixNamespace
   */
  class GeneralizationSetBuilder(var prefixNamespace: PrefixNamespace):
    private var parent: Option[UMLClassIdentity] = None
    private var children: scala.collection.mutable.Set[UMLClassIdentity] = scala.collection.mutable.HashSet()
    private var coveringConstraint: CoveringConstraint = CoveringConstraint.Incomplete
    private var disjointConstraint: DisjointConstraint = DisjointConstraint.Overlapping

    def withParentAndChildren(parent:String, children: Set[String]): GeneralizationSetBuilder =
      this.parent = Some(ClassIdentityBuilderCache.getUMLClassIdentity(parent).getOrElse(
        UMLClassIdentity.builder(prefixNamespace).withNameOrCurie(parent).build))
      children.map(child =>
        var childIdentity = ClassIdentityBuilderCache.getUMLClassIdentity(child).getOrElse(
          UMLClassIdentity.builder(prefixNamespace).withNameOrCurie(child).build)
        var childBuilder = ClassIdentityBuilderCache.getClassIdentityBuilder(childIdentity)
        this.children += childBuilder.withNameOrCurie(child).build)
      this

    def withParentAndChildren(parent: UMLClassIdentity, children: Set[UMLClassIdentity]): GeneralizationSetBuilder =
      this.parent = Some(parent)
      this.children = children.to(mutable.HashSet)
      this

    def withCoveringConstraint(coveringConstraint: CoveringConstraint): GeneralizationSetBuilder =
      this.coveringConstraint = coveringConstraint
      this

    def withDisjointConstraint(disjointConstraint: DisjointConstraint): GeneralizationSetBuilder =
      this.disjointConstraint = disjointConstraint
      this

    def build: UMLGeneralizationSet =
      UMLGeneralizationSet(children.toSet, coveringConstraint, disjointConstraint)

case class UMLClassChildren(setOfGeneralizationSets: Set[UMLGeneralizationSet])

case class UMLClass(classIdentity: UMLClassIdentity,
                    classDefinitionOption: Option[UMLClassDefinition] = None,
                    children: UMLClassChildren = UMLClassChildren(Set())) extends UMLIdentity:
  def getIRI: String = classIdentity.getIRI
  def getLabel: String = classIdentity.getLabel

object UMLClass:

  def builder(prefixNamespace: PrefixNamespace): ClassBuilder =
    ClassBuilder(prefixNamespace)

  /**
   * According to the UML specification, a class can belong to multiple generalization sets. Here we make the simplifying
   * assumption that a class belongs to a single generalization set.
   *
   * @Todo: Handle the case where a single class can belong to multiple generalization sets, if we find this is indeed a
   *
   * @param prefixNamespaceOption
   */
  class ClassBuilder(prefixNamespaceOption: PrefixNamespace):
    private var classIdentityBuilder = UMLClassIdentity.builder(prefixNamespaceOption)
    private var definition: Option[String] = None
    private var children: scala.collection.mutable.Set[UMLGeneralizationSet.GeneralizationSetBuilder] =
      scala.collection.mutable.HashSet()

    def withName(name: String): ClassBuilder =
      this.classIdentityBuilder = classIdentityBuilder.withName(name)
      this

    def withCurie(curie: String): ClassBuilder =
      this.classIdentityBuilder = classIdentityBuilder.withCurie(curie)
      this

    def withNameOrCurie(nameOrCurie: String): ClassBuilder =
      this.classIdentityBuilder = classIdentityBuilder.withNameOrCurie(nameOrCurie)
      this

    def withNameAndCurie(name: String, curie: String): ClassBuilder =
      if name.isEmpty || curie.isEmpty then
        throw new IllegalArgumentException("Name or curie must not be empty.")
      this.classIdentityBuilder = classIdentityBuilder.withNameAndCurie(name, curie)
      this

    def withDefinition(definition: String): ClassBuilder =
      this.definition = Some(definition)
      this

    def withChildren(parent: String, children: Set[String]): ClassBuilder =
      this.children += UMLGeneralizationSet.builder(prefixNamespaceOption)
        .withParentAndChildren(parent, children)
      this

    def withParentAndChildren(parent: String, children: Set[String],
                              covering: CoveringConstraint, disjoint: DisjointConstraint): ClassBuilder =
      this.children += UMLGeneralizationSet.builder(prefixNamespaceOption)
        .withParentAndChildren(parent, children)
        .withCoveringConstraint(covering)
        .withDisjointConstraint(disjoint)
      this

    def withCompleteDisjoint(complete: Boolean, disjoint: Boolean): ClassBuilder =
      val covering = if complete then CoveringConstraint.Complete else CoveringConstraint.Incomplete
      val disjointConstraint = if disjoint then DisjointConstraint.Disjoint else DisjointConstraint.Overlapping
      if children.isEmpty then
        throw new IllegalArgumentException("Children must not be empty. The assumption is that the children for this class" +
          "has already been populated.")
      children.head.withCoveringConstraint(covering)
        .withDisjointConstraint(disjointConstraint)
      this
    
    def build: UMLClass =
      val umlClass = UMLClass(
        classIdentityBuilder.build,
        definition.map(UMLClassDefinition.apply(_)),
        UMLClassChildren(children.map(_.build).toSet)
      )
      ClassBuilderCache.cacheUMLClass(umlClass, this)
      umlClass

