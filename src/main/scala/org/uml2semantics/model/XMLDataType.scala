package org.uml2semantics.model

enum XMLPrimitiveDataType:
  case `xsd:boolean` extends XMLPrimitiveDataType
  case `xsd:float`  extends XMLPrimitiveDataType
  case `xsd:string` extends XMLPrimitiveDataType

object XMLPrimitiveDataType:
  def unapply(s: String): XMLPrimitiveDataType =
    XMLPrimitiveDataType.valueOf(s)

