package org.uml2semantics.model

import com.typesafe.scalalogging.Logger

enum XMLDataType:
  case
    `xsd:anyURI`,
    `xsd:base64Binary`,
    `xsd:boolean`,
    `xsd:byte`,
    `xsd:dateTime`,
    `xsd:dateTimeStamp`,
    `xsd:decimal`,
    `xsd:double`,
    `xsd:float`,
    `xsd:hexBinary`,
    `xsd:int`,
    `xsd:integer`,
    `xsd:language`,
    `xsd:long`,
    `xsd:Name`,
    `xsd:NCName`,
    `xsd:negativeInteger`,
    `xsd:NMTOKEN`,
    `xsd:nonNegativeInteger`,
    `xsd:nonPositiveInteger`,
    `xsd:normalizedString`,
    `xsd:positiveInteger`,
    `xsd:short`,
    `xsd:string`,
    `xsd:token`,
    `xsd:unsignedByte`,
    `xsd:unsignedInt`,
    `xsd:unsignedLong`,
    `xsd:unsignedShort`

object XMLDataType:
  private val prefixName: PrefixName = PrefixName("xsd")
  def unapply(s: String): Option[XMLDataType] =
    try
      val dataType: XMLDataType = XMLDataType.valueOf(s)
      Some(dataType)
    catch
      case _ => None

  def getIRI(dataType: XMLDataType): String =
    PrefixNamespace.getPrefixNamespace(prefixName).get.prefixIRI.iri + dataType
