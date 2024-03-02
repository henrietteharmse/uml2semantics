package org.uml2semantics.reader

import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.model.{PrefixNamespace, UMLClass, UMLClassDiagram, UMLClasses}
import org.uml2semantics.inline.Code
import org.w3c.dom.{Document, Node, NodeList}

import scala.jdk.CollectionConverters.*
import java.io.File
import scala.collection.mutable
import sourcecode.Text.generate

import java.util.Collections
import java.util.stream.{Collectors, IntStream}
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import javax.xml.xpath.{XPath, XPathConstants, XPathExpression, XPathFactory}

def parseClassesFromXMI(maybeXMIFile: Option[File], ontologyPrefix: PrefixNamespace): /*UMLClasses*/ Unit =
  val logger = Logger("parseClasses")
  logger.info("Start")
//  val reader = XML.loadFile(maybeXMIFile.get)

//  var umlClasses = mutable.Set[UMLClass]()




def parseUMLClassDiagramFromXMI(input: InputParameters): Option[UMLClassDiagram] =
  val logger = Logger("parseUMLClassDiagramFromXMI")
  logger.debug(s"start xmi parsing ${Code.source}")
//  val xmi = XML.loadFile(input.xmiFile.get)

  // Scala xml solution
//  val classes = (xmi \\ "packagedElement").filter(_.attribute("http://schema.omg.org/spec/XMI/2.1", "type").exists(_.text == "uml:Class"))

  // Java xml
  val builderFactory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance
  val builder: DocumentBuilder = builderFactory.newDocumentBuilder
  val xmlDocument: Document = builder.parse(input.xmiFile.get)
  val xPath: XPath = XPathFactory.newInstance.newXPath
  val expression: String = "//packagedElement[@type='uml:Class']"
  val classNodeList: NodeList = xPath.compile(expression).evaluate(xmlDocument, XPathConstants.NODESET).asInstanceOf[NodeList]
  logger.debug(s"classNodes = ${classNodeList.getLength}")

  val classNodes: Seq[Node] = IntStream.range(0, classNodeList.getLength())
    .mapToObj(classNodeList.item)
    .collect(Collectors.toList()).asScala.toSeq

  for node <- classNodes do logger.debug(s"class = ${node.getAttributes.getNamedItem("name")}")
  None

