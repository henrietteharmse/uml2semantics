package org.uml2semantics.model
import java.io.File
import com.typesafe.scalalogging.Logger

import scala.annotation.targetName
import scala.collection.mutable

sealed trait ClassId
case class ClassName(name: String = "") extends ClassId:
  def nonEmpty: Boolean = name.nonEmpty

case class ClassCurie(curie: String = "") extends ClassId:
  def nonEmpty: Boolean = curie.nonEmpty

case class UncertainClassId(uncertainId: String) extends ClassId

case class ClassParentIds(setOfParentIds: Set[UncertainClassId])
object ClassParentIds:
  val logger = Logger[ClassParentIds]
  @targetName("fromSetOfStrings")
  def apply(setOfParentIds: Set[String]): ClassParentIds =
    logger.trace(s"setOfParentIds=$setOfParentIds")
    logger.trace(s"setOfParentIds.isEmpty=${setOfParentIds.isEmpty}")
    val setOfParentUncertainClassIds = setOfParentIds
      .filterNot(s => s.isEmpty)
      .map(m => UncertainClassId(m))
    ClassParentIds(setOfParentUncertainClassIds)


case class ClassIRI(ontologyPrefix: OntologyPrefix, classId: ClassId):
  val iri = ontologyPrefix.ontologyPrefix + classId
//  override def toString: String = ontologyPrefix.ontologyPrefix + classId

case class OntologyIRI(ontologyIRI: String)
case class OntologyPrefix(ontologyPrefix: String):
  def +(classId: ClassId): ClassIRI =
    ClassIRI(OntologyPrefix(ontologyPrefix), classId)

case class ClassDefinition(definition: String = "")

sealed trait Cardinality
object Cardinality:
  extension (c1: Cardinality)
    def >(c2: Cardinality): Boolean = (c1, c2) match
      case (t1, t2): (InfiniteCardinality, InfiniteCardinality) => false
      case (t1, t2): (InfiniteCardinality, NonNegativeCardinality) => true
      case (t1, t2): (NonNegativeCardinality, InfiniteCardinality) => false
      case (t1, t2): (NonNegativeCardinality, NonNegativeCardinality) => t1 > t2
      case (t1, t2): (_, _) =>
        println(s"Unexpected case t1=$t1 and t2=$t2")
        false


opaque type Infinite <: Char = '*'

opaque type NonNegativeInteger <: Int = Int

object NonNegativeInteger:
  inline
  def apply(inline n: Int): NonNegativeInteger =
    inline if n < 0
    then compiletime.error("Cannot compile NonNegativeInteger(n). n must be >= 0.")
    else n: NonNegativeInteger

case class InfiniteCardinality(infinite: Infinite) extends Cardinality
case class NonNegativeCardinality(nonNegativeInteger: NonNegativeInteger) extends Cardinality

case class Multiplicity (min: Cardinality,
                         max: Cardinality)

object Multiplicity:
  def apply (min: Cardinality, max: Cardinality): Multiplicity =
    require(max > min, "max cardinality must be greater than min cardinality")
    Multiplicity(min, max)

case class ClassIdentity(classCurie: ClassCurie,
                         className: ClassName,
                         ontologyPrefix: OntologyPrefix):
  var tmpClassId: ClassId = _
  if classCurie.nonEmpty then
    tmpClassId = classCurie
  else if className.nonEmpty then
    tmpClassId = className
  val classIRI = ClassIRI(ontologyPrefix, tmpClassId)



sealed trait UmlClassDiagramElement
case class UmlClass(classIdentity: ClassIdentity,
                    classDefinition: ClassDefinition,
                    classParentIds: ClassParentIds)
  extends UmlClassDiagramElement
type Primitive = String
object Primitive:
  def apply(string: String): Primitive = string
given CanEqual[Primitive, Primitive] = CanEqual.derived

case class UmlClassAttribute(classId: ClassIdentity, attributeId: ClassIdentity,
                             typeOfAttribute: Primitive|ClassIdentity,
                             multiplicity: Multiplicity, definition: Option[String])
  extends UmlClassDiagramElement

case class UmlClassDiagram(owlOntologyFile: File,
                           ontologyIRI: OntologyIRI,
                           ontologyPrefix: OntologyPrefix,
                           umlClasses: Map[ClassId, UmlClass])
object UmlClassDiagram:
  def apply(owlOntologyFile: File, ontologyIRI: OntologyIRI, ontologyPrefix: OntologyPrefix) =
    new UmlClassDiagram(owlOntologyFile,ontologyIRI, ontologyPrefix, Map())
