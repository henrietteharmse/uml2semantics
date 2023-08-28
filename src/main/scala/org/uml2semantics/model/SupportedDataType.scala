package org.uml2semantics.model

import com.typesafe.scalalogging.Logger
import org.uml2semantics.inline.Code


enum SupportedDataType(curie: Curie):
  val _curie: Curie = curie
  case `xsd:anyURI` extends SupportedDataType(Curie("xsd:anyURI"))
  case `xsd:base64Binary` extends SupportedDataType(Curie("xsd:base64Binary"))
  case `xsd:boolean` extends SupportedDataType(Curie("xsd:boolean"))
  case `xsd:byte` extends SupportedDataType(Curie("xsd:byte"))
  case `xsd:dateTime` extends SupportedDataType(Curie("xsd:dateTime"))
  case `xsd:dateTimeStamp` extends SupportedDataType(Curie("xsd:dateTimeStamp"))
  case `xsd:decimal` extends SupportedDataType(Curie("xsd:decimal"))
  case `xsd:double` extends SupportedDataType(Curie("xsd:double"))
  case `xsd:float` extends SupportedDataType(Curie("xsd:float"))
  case `xsd:hexBinary` extends SupportedDataType(Curie("xsd:hexBinary"))
  case `xsd:int` extends SupportedDataType(Curie("xsd:int"))
  case `xsd:integer` extends SupportedDataType(Curie("xsd:integer"))
  case `xsd:language` extends SupportedDataType(Curie("xsd:language"))
  case `xsd:long` extends SupportedDataType(Curie("xsd:long"))
  case `xsd:Name` extends SupportedDataType(Curie("xsd:Name"))
  case `xsd:NCName` extends SupportedDataType(Curie("xsd:NCName"))
  case `xsd:negativeInteger` extends SupportedDataType(Curie("xsd:negativeInteger"))
  case `xsd:NMTOKEN` extends SupportedDataType(Curie("xsd:NMTOKEN"))
  case `xsd:nonNegativeInteger` extends SupportedDataType(Curie("xsd:nonNegativeInteger"))
  case `xsd:nonPositiveInteger` extends SupportedDataType(Curie("xsd:nonPositiveInteger"))
  case `xsd:normalizedString` extends SupportedDataType(Curie("xsd:normalizedString"))
  case `xsd:positiveInteger` extends SupportedDataType(Curie("xsd:positiveInteger"))
  case `xsd:short` extends SupportedDataType(Curie("xsd:short"))
  case `xsd:string` extends SupportedDataType(Curie("xsd:string"))
  case `xsd:token` extends SupportedDataType(Curie("xsd:token"))
  case `xsd:unsignedByte` extends SupportedDataType(Curie("xsd:unsignedByte"))
  case `xsd:unsignedInt` extends SupportedDataType(Curie("xsd:unsignedInt"))
  case `xsd:unsignedLong` extends SupportedDataType(Curie("xsd:unsignedLong"))
  case `xsd:unsignedShort` extends SupportedDataType(Curie("xsd:unsignedShort"))
  case `rdfs:Literal` extends SupportedDataType(Curie("rdfs:Literal"))
  case `rdfs:DataType` extends SupportedDataType(Curie("rdfs:DataType"))
  case `rdf:langString` extends SupportedDataType(Curie("rdf:langString"))
  case `rdf:HTML` extends SupportedDataType(Curie("rdf:HTML"))
  case `rdf:XMLLiteral` extends SupportedDataType(Curie("rdf:XMLLiteral"))
  case `rdf:JSON` extends SupportedDataType(Curie("rdf:JSON"))

object SupportedDataType:
  private val logger = Logger[this.type]

  def unapply(s: String): Option[SupportedDataType] =
    logger.debug(s"s=$s ${Code.source}")
    try
      val dataType: SupportedDataType = SupportedDataType.valueOf(s)
      Some(dataType)
    catch
      case _ => None


  def getIRI(dataType: SupportedDataType): String =
    logger.debug(s"dataType=$dataType ${Code.source}")
    dataType._curie.toIRI

