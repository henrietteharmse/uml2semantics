package org.uml2semantics.reader

import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.inline.Code
import org.uml2semantics.model.*
import org.uml2semantics.reader.ReaderHelper.populateParentsWithTheirChildren
import org.w3c.dom.{Document, Node, NodeList}

import java.util
import java.util.stream.{Collectors, IntStream}
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.*
import scala.collection.{immutable, mutable}
import scala.jdk.CollectionConverters.*

object XMIReader extends UMLClassDiagramReader :
  val logger = Logger("XMIReader")

  override def parseUMLClassDiagram(input: InputParameters): Unit =
    logger.debug(s"start xmi parsing ${Code.source}")
    val ontologyPrefix = PrefixNamespace(input.ontologyPrefix)
    val xmiDocument = DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(input.xmiFile.get)
    parseClasses(ontologyPrefix, XPathFactory.newInstance.newXPath, xmiDocument)

  private def parseClasses(ontologyPrefix: PrefixNamespace, xPath: XPath, xmiDocument: Document): Unit =
    val classNodes = executeXPathQuery(xPath, xmiDocument, "//packagedElement[@type='uml:Class']")
    val parentToChildrenMap = mutable.Map[String, mutable.Set[String]]()

    for classNode <- classNodes do
      val classIdentifier = classNode.getAttributes.getNamedItem("name").getNodeValue
      val parentSet = extractParents(xPath, xmiDocument, classNode)
      if parentSet.nonEmpty then
        parentSet.foreach(parent =>
          parentToChildrenMap.getOrElseUpdate(parent, mutable.HashSet()) += classIdentifier)
      val definition= extractDefintion(xPath, xmiDocument, classNode, classIdentifier)
      var classBuilder = UMLClass.builder(ontologyPrefix)
        .withNameOrCurie(classIdentifier)
      if definition.nonEmpty then
        classBuilder = classBuilder.withDefinition(definition)
      classBuilder.build

    populateParentsWithTheirChildren(parentToChildrenMap, ontologyPrefix)
    parseGeneralizationSets(ontologyPrefix, xPath, xmiDocument)


  private def parseGeneralizationSets(ontologyPrefix: PrefixNamespace, xPath: XPath, xmiDocument: Document): Unit =
    val generalizationSetNodes = executeXPathQuery(xPath, xmiDocument, "//packagedElement[@type='uml:GeneralizationSet']")
    for generalizationSetNode <- generalizationSetNodes do
      val generalizationSetName = generalizationSetNode.getAttributes.getNamedItem("name").getNodeValue
      val isCovering = generalizationSetNode.getAttributes.getNamedItem("isCovering").getNodeValue.toBoolean
      val isDisjoint = generalizationSetNode.getAttributes.getNamedItem("isDisjoint").getNodeValue.toBoolean
      val optionParent = findParentOfGeneralizationSet(xPath, xmiDocument, generalizationSetName)

      val k = 1


  /**
   * We make the simplifying assumption that a class belongs to a single generalization set.
   *
   * @Todo: Handle the case where a single class can belong to multiple generalization sets, if we find this is indeed a
   * case that is used in practice.
   *
   * @param xPath
   * @param xmiDocument
   * @param generalizationSetName
   * @return
   */
  private def findParentOfGeneralizationSet(xPath: XPath, xmiDocument: Document, generalizationSetName: String): Option[String] =
    executeXPathQuery(xPath, xmiDocument,
      s"//packagedElement[@type='uml:GeneralizationSet' and @name='$generalizationSetName']/generalization/@idref")
      .headOption
      .flatMap { idref =>
        executeXPathQuery(xPath, xmiDocument,
          s"//packagedElement/generalization[@type='uml:Generalization' and @id='${idref.getNodeValue}']/@general")
          .headOption
          .flatMap { general =>
            executeXPathQuery(xPath, xmiDocument,
              s"//packagedElement[@type='uml:Class' and @id='${general.getNodeValue}']/@name")
              .headOption
              .map(_.getNodeValue)
          }
      }

  private def extractDefintion(xPath: XPath, xmiDocument: Document,
                                   classNode: Node, classIdentifier: String): String =
    val classId = classNode.getAttributes.getNamedItem("xmi:id").getNodeValue
    val classProperties = executeXPathQuery(xPath, xmiDocument,
      s"//element[@idref='$classId' and @type='uml:Class' and @name='$classIdentifier']/properties")
    val definition = StringBuilder()
    classProperties.foreach { propertiesNode =>
      val definitionNode = propertiesNode.getAttributes.getNamedItem("documentation")
      if definitionNode != null then
        definition.append(definitionNode.getNodeValue)
    }
    definition.toString()


  def extractParents(xPath: XPath, document: Document, classNode: Node): mutable.Set[String] =
    val generalizationNodes = executeXPathQueryAsNodeList(xPath, classNode, "generalization")
    val parentIds = (0 until generalizationNodes.getLength).map(i =>
      generalizationNodes.item(i).getAttributes.getNamedItem("general").getNodeValue)
      .to(mutable.Set)
    parentIds.flatMap { parentId =>

      val parentNodeList = executeXPathQueryAsNodeList(xPath, document,
        s"//packagedElement[@id='$parentId' and @type='uml:Class']")

      if parentNodeList.getLength > 0 then
        Option(parentNodeList.item(0).getAttributes.getNamedItem("name")).map(_.getNodeValue)
      else None
    }

  private def executeXPathQueryAsNodeList(xPath: XPath, node: Node, query: String): NodeList =
    xPath
      .compile(query)
      .evaluate(node, XPathConstants.NODESET)
      .asInstanceOf[NodeList]

  private def executeXPathQuery(xPath: XPath, document: Document, query: String): Seq[Node] =
    nodeListToSeq(xPath
      .compile(query)
      .evaluate(document, XPathConstants.NODESET)
      .asInstanceOf[NodeList])

  private def nodeListToSeq(nodeList: NodeList): Seq[Node] =
    IntStream
      .range(0, nodeList.getLength)
      .mapToObj(nodeList.item)
      .collect(Collectors.toList)
      .asScala
      .toSeq