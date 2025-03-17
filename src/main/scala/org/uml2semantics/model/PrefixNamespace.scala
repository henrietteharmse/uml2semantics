package org.uml2semantics.model

import com.typesafe.scalalogging.Logger
import org.uml2semantics.inline.Code
import org.uml2semantics.model.PrefixNamespace.prefixMap

import scala.collection.mutable.Map
import scala.collection.{immutable, mutable}

private val FRAGMENT_SEPARATOR: Char = '#'
private val PATH_SEPARATOR: Char = '/'


case class PrefixName(name: String):
  def nonEmpty: Boolean = name.nonEmpty
  def isEmpty: Boolean = name.isEmpty

case class PrefixIRI(iri: String):
  def nonEmpty: Boolean = iri.nonEmpty
  def isEmpty: Boolean = iri.isEmpty

case class PrefixNamespace(prefixName: PrefixName, prefixIRI: PrefixIRI):
  private def toSimpleString: String = prefixName.name + ":" + prefixIRI.iri
  def nonEmpty: Boolean = prefixName.nonEmpty && prefixIRI.iri.nonEmpty
  def isEmpty: Boolean = prefixName.isEmpty && prefixIRI.isEmpty

object PrefixNamespace:
  private val logger = Logger[this.type]

  private val prefixMap: mutable.Map[PrefixName, PrefixNamespace] = mutable.HashMap[PrefixName, PrefixNamespace]()

  private val predefinedPrefixNamespaces: immutable.Map[PrefixName, PrefixNamespace] = immutable.HashMap(
    {
      val prefixName = PrefixName("xsd")
      prefixName -> PrefixNamespace(prefixName, PrefixIRI("http://www.w3.org/2001/XMLSchema#"))
    },
    {
      val prefixName = PrefixName("rdf")
      prefixName -> PrefixNamespace(prefixName, PrefixIRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
    },
    {
      val prefixName = PrefixName("rdfs")
      prefixName -> PrefixNamespace(prefixName, PrefixIRI("http://www.w3.org/2000/01/rdf-schema#"))
    },
    {
      val prefixName = PrefixName("owl")
      prefixName -> PrefixNamespace(prefixName, PrefixIRI("http://www.w3.org/2002/07/owl#"))
    }
  )

  def apply(prefixName: PrefixName, prefixIRI: PrefixIRI): PrefixNamespace =
    logger.debug(s"prefixName=$prefixName, prefixIRI=$prefixIRI, ${Code.source}")
    val prefixNamespace: PrefixNamespace = new PrefixNamespace(prefixName, prefixIRI)
    prefixMap += (prefixName -> prefixNamespace)
    prefixNamespace

  def apply(s: String): PrefixNamespace =
    logger.debug(s"s=$s ${Code.source}")
    require(s.contains(":"), s"String = '$s' does not contain a ':'.")
    val args: Array[String] = s.split(":", 2)
    val prefixName: PrefixName = PrefixName(args(0))
    val prefixIRI: PrefixIRI = PrefixIRI(args(1))
    new PrefixNamespace(prefixName, prefixIRI)

  def predefinedPrefixNamespacesAsStrings(): Seq[String] =
    predefinedPrefixNamespaces.values.map(a => a.toSimpleString).toSet.toSeq

  def cachePrefixes(prefixes: Seq[String]): Unit =
    logger.debug(s"prefixes=$prefixes ${Code.source}")
    prefixes.foreach(prefix => {
      val prefixNamespace: PrefixNamespace = PrefixNamespace(prefix)
      logger.debug(s"prefixNamespace.prefixName = $prefixNamespace ${Code.source}")
      prefixMap += (prefixNamespace.prefixName -> prefixNamespace)
    })

  def cachePrefix(prefix: String): Unit =
    logger.debug(s"prefix=$prefix ${Code.source}")
    val prefixNamespace: PrefixNamespace = PrefixNamespace(prefix)
    prefixMap += (prefixNamespace.prefixName -> prefixNamespace)


  def getPrefixNamespace(prefixName: PrefixName): Option[PrefixNamespace] =
    logger.debug(s"prefixName=$prefixName ${Code.source}")
    prefixMap.get(prefixName) match
      case Some(prefixNamespace) => Some(prefixNamespace)
      case None => predefinedPrefixNamespaces.get(prefixName)


case class PrefixReference(reference: String)

private val SEPARATOR: String = ":"

case class Curie(curie: String):
  private val logger = Logger[this.type]
  require(Curie.isCurieBasedOnConfiguredPrefix(curie), s"Curie=$curie is not using a known prefix.")
  private val args: Array[String] = curie.split(SEPARATOR)
  val prefixName: PrefixName = PrefixName(args(0))
  logger.debug(s"prefixName=$prefixName ${Code.source}")
  val prefixReference: PrefixReference = PrefixReference(args(1))
  logger.debug(s"prefixReference=$prefixReference ${Code.source}")


  def toIRI: String =
    require(PrefixNamespace.getPrefixNamespace(prefixName).nonEmpty, s"Prefix name = '$prefixName' is undefined. Please define it.")
    PrefixNamespace.getPrefixNamespace(prefixName).get.prefixIRI.iri + prefixReference.reference

  def nonEmpty: Boolean = prefixName.nonEmpty && curie.nonEmpty
  def isEmpty: Boolean = prefixName.isEmpty && curie.isEmpty
object Curie:
  private val logger = Logger[this.type]

  /**
   * See https://www.w3.org/TR/curie for specification
   *
   * @param prefixName
   * @param prefixReference
   */
  def apply(prefixName: PrefixName, prefixReference: PrefixReference): Curie =
    logger.debug(s"prefixName=$prefixName, prefixReference=$prefixReference ${Code.source}")
    new Curie(prefixName.name + SEPARATOR + prefixReference.reference)
      
  def fromString(s: String): Option[Curie] =
    logger.debug(s"s=$s ${Code.source}")
    if s.contains(SEPARATOR) && isCurieBasedOnConfiguredPrefix(s) then
      Some(new Curie(s))
    else
      None

  def unapply(curie: Curie): Option[(PrefixName, PrefixReference)] =
    logger.debug(s"curie=$curie ${Code.source}")
    Some((curie.prefixName, curie.prefixReference))

  def isCurieBasedOnConfiguredPrefix(s: String): Boolean =
    logger.debug(s"s=$s ${Code.source}")
    if s.contains(SEPARATOR) then
      val stringArray: Array[String] = s.split(SEPARATOR)
      val possiblePrefixName: String = stringArray(0)
      val optionPrefixNamespace: Option[PrefixNamespace] = PrefixNamespace.getPrefixNamespace(PrefixName(possiblePrefixName))
      optionPrefixNamespace match
        case Some(_) => true
        case None => false
    else
      false

