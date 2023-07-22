package org.uml2semantics.inline

object Code:
  // Used for debug logging
  inline def source: String = s"at line ${summon [sourcecode.Line].value} of ${summon [sourcecode.FileName].value}"

  // Used for errors
  inline def sourceDetail: String = s"at line ${summon [sourcecode.Line].value} of ${summon [sourcecode.Name].value} in ${summon [sourcecode.File].value}"

  // @Todo com.lihaoyi.sourcecode library not retrieving objects. Should use this once fixed.
  inline def arguments: String = s"${summon [sourcecode.Enclosing].value} with args: ${summon [sourcecode.Args].value
    .map(_.map(a => {
      s"${a.source} = ${a.value}"
    }).mkString("(", ", ", ")"))}"


  // args.value.map(_.map(a => a.source + "=" + a.value).mkString("(", ", ", ")"))