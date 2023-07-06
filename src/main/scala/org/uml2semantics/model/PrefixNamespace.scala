package org.uml2semantics.model

import scala.annotation.targetName


case class PrefixName(name: String):
  @targetName("appendIRI")
  def +(prefixIRI: PrefixIRI): String = name +  ":" + prefixIRI.iri

case class PrefixIRI(iri: String)

case class PrefixNamespace(prefixName: PrefixName, prefixIRI: PrefixIRI):
  def toSimpleString: String = prefixName + prefixIRI

object PrefixNamespace:
  val predefinedPrefixNamespaces : Seq[PrefixNamespace] = Seq(
    PrefixNamespace(PrefixName("xsd"), PrefixIRI("http://www.w3.org/2001/XMLSchema#")),
    PrefixNamespace(PrefixName("rdf"), PrefixIRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#")),
    PrefixNamespace(PrefixName("rdfs"), PrefixIRI("http://www.w3.org/2000/01/rdf-schema#")),
    PrefixNamespace(PrefixName("owl"), PrefixIRI("http://www.w3.org/2002/07/owl#")),
  )

  def predefinedPrefixNamespacesAsStrings(): Seq[String] =
    val seqOfString: Seq[String] = for prefixNamespace <- predefinedPrefixNamespaces yield
      prefixNamespace.toSimpleString
    seqOfString