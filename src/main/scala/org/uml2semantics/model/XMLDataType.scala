package org.uml2semantics.model

import com.typesafe.scalalogging.Logger
import org.uml2semantics.inline.Code

enum XMLDataType(prefixReference: String):
  val _prefixReference: String = prefixReference
  case `xsd:anyURI` extends XMLDataType("anyURI")
  case `xsd:base64Binary` extends XMLDataType("base64Binary")
  case `xsd:boolean` extends XMLDataType("boolean")
  case `xsd:byte` extends XMLDataType("byte")
  case `xsd:dateTime` extends XMLDataType("dateTime")
  case `xsd:dateTimeStamp` extends XMLDataType("dateTimeStamp")
  case `xsd:decimal` extends XMLDataType("decimal")
  case `xsd:double` extends XMLDataType("double")
  case `xsd:float` extends XMLDataType("float")
  case `xsd:hexBinary` extends XMLDataType("hexBinary")
  case `xsd:int` extends XMLDataType("int")
  case `xsd:integer` extends XMLDataType("integer")
  case `xsd:language` extends XMLDataType("language")
  case `xsd:long` extends XMLDataType("long")
  case `xsd:Name` extends XMLDataType("Name")
  case `xsd:NCName` extends XMLDataType("NCName")
  case `xsd:negativeInteger` extends XMLDataType("negativeInteger")
  case `xsd:NMTOKEN` extends XMLDataType("NMTOKEN")
  case `xsd:nonNegativeInteger` extends XMLDataType("nonNegativeInteger")
  case `xsd:nonPositiveInteger` extends XMLDataType("nonPositiveInteger")
  case `xsd:normalizedString` extends XMLDataType("normalizedString")
  case `xsd:positiveInteger` extends XMLDataType("positiveInteger")
  case `xsd:short` extends XMLDataType("short")
  case `xsd:string` extends XMLDataType("string")
  case `xsd:token` extends XMLDataType("token")
  case `xsd:unsignedByte` extends XMLDataType("unsignedByte")
  case `xsd:unsignedInt` extends XMLDataType("unsignedInt")
  case `xsd:unsignedLong` extends XMLDataType("unsignedLong")
  case `xsd:unsignedShort` extends XMLDataType("unsignedShort")

object XMLDataType:
  private val logger = Logger[this.type]
  private val prefixName: PrefixName = PrefixName("xsd")

  def unapply(s: String): Option[XMLDataType] =
    logger.debug(s"s=$s ${Code.source}")
    try
      val dataType: XMLDataType = XMLDataType.valueOf(s)
      Some(dataType)
    catch
      case _ => None

  def getIRI(dataType: XMLDataType): String =
    logger.debug(s"dataType=$dataType ${Code.source}")
    PrefixNamespace.getPrefixNamespace(prefixName).get.prefixIRI.iri + dataType._prefixReference
