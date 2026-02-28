package org.uml2semantics.reader

import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.inline.Code
import org.uml2semantics.model.*
import org.uml2semantics.model.UMLClass.ClassBuilder
import org.uml2semantics.model.cache.{ClassBuilderCache, ClassIdentityBuilderCache}
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
    val ontologyPrefix: PrefixNamespace = PrefixNamespace(input.ontologyPrefix)
    val xmiDocument: Document = DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(input.xmiFile.get)
    parseClasses(ontologyPrefix, XPathFactory.newInstance.newXPath, xmiDocument)

  private def parseClasses(ontologyPrefix: PrefixNamespace, xPath: XPath, xmiDocument: Document): Unit =
    val classNodes: immutable.Seq[Node] = doXPathQueryGetAsSeqOfNodes(xPath, xmiDocument, "//packagedElement[@type='uml:Class']")
    val parentToGeneralizationSetMap = mutable.Map[String, XMIGeneralizationSet]()

    for classNode <- classNodes do
      val classIdentifier: String = classNode.getAttributes.getNamedItem("name").getNodeValue
      parseClassParents(xPath, xmiDocument, classNode, parentToGeneralizationSetMap)

      val definition= parseClassDefinition(xPath, xmiDocument, classNode, classIdentifier)
      var classBuilder = UMLClass.builder(ontologyPrefix)
        .withNameOrCurie(classIdentifier)
      if definition.nonEmpty then
        classBuilder = classBuilder.withDefinition(definition)
      val umlClass = classBuilder.build
      parseClassAttributes(xPath, xmiDocument, classNode, umlClass.classIdentity)


    populateParentsWithTheirChildren(parentToGeneralizationSetMap, ontologyPrefix)



  private def parseClassDefinition(xPath: XPath, xmiDocument: Document,
                                   classNode: Node, classIdentifier: String): String =
    val classId = classNode.getAttributes.getNamedItem("xmi:id").getNodeValue
    doXPathQueryGetAsString(xPath, xmiDocument,
      s"//element[@idref='$classId' and @type='uml:Class' and @name='$classIdentifier']/properties/@documentation")



  private def parseClassParents(xPath: XPath, document: Document, childNode: Node,
                                parentToGeneralizationSetMap: mutable.Map[String, XMIGeneralizationSet]):
                                immutable.Seq[XMIGeneralizationSet] =

    val generalizationNodes = doXPathQueryGetAsSeqOfNodes(xPath, childNode, "generalization")
    val childName = childNode.getAttributes.getNamedItem("name").getNodeValue

    try {
      generalizationNodes.map { generalizationNode =>
        val parentId = generalizationNode.getAttributes.getNamedItem("general").getNodeValue
        val parentName = doXPathQueryGetAsString(xPath, document,
          s"//packagedElement[@id='$parentId' and @type='uml:Class']/@name")
        val memberNodes = doXPathQueryGetAsSeqOfNodes(xPath, document,
          s"//packagedElement/generalization[@idref='${generalizationNode.getAttributes.getNamedItem("xmi:id").getNodeValue}']")
        val existingGeneralizationSet = parentToGeneralizationSetMap.get(parentName)

        val generalizationSet = if memberNodes.nonEmpty then
          val generalizationSetNode = memberNodes.head.getParentNode
          val isCovering = Option(generalizationSetNode.getAttributes.getNamedItem("isCovering"))
            .map(_.getNodeValue.toBoolean)
            .getOrElse(false)
          val isDisjoint = Option(generalizationSetNode.getAttributes.getNamedItem("isDisjoint"))
            .map(_.getNodeValue.toBoolean)
            .getOrElse(false)
          XMIGeneralizationSet(parentName, mutable.Set(childName), isCovering, isDisjoint)
        else
          XMIGeneralizationSet(parentName, mutable.Set(childName))

        parentToGeneralizationSetMap.put(parentName,
          existingGeneralizationSet.map(generalizationSet.merge).getOrElse(generalizationSet))
        generalizationSet
      }
    } catch {
      case t: Throwable =>
        logger.error(s"Error while extracting parents of class $childName: ${t.getMessage}", t)
        Seq.empty
    }

  private def populateParentsWithTheirChildren(parentToChildrenMap: mutable.Map[String, XMIGeneralizationSet],
                                       ontologyPrefix: PrefixNamespace): Unit =

    var classesToRebuild = scala.collection.mutable.Set[ClassBuilder]()

    parentToChildrenMap.foreach { (parent, generalizationSet) =>
      val parentClassIdentity = ClassIdentityBuilderCache.getUMLClassIdentity(parent).getOrElse(
        UMLClassIdentity.builder(ontologyPrefix).withNameOrCurie(parent).build)
      val classBuilder = ClassBuilderCache.getUMLClassBuilder(parentClassIdentity).getOrElse(
          UMLClass.builder(ontologyPrefix).withNameOrCurie(parent))
        .withChildren(parent, generalizationSet.getChildren)
        .withCompleteDisjoint(generalizationSet.isComplete, generalizationSet.isDisjoint)
      classesToRebuild += classBuilder
    }

    classesToRebuild.foreach(classBuilder => classBuilder.build)

  private def parseClassAttributes(xPath: XPath, xmiDocument: Document, classNode: Node,
                                   umlClassIdentity: UMLClassIdentity): Unit =

    val classId = classNode.getAttributes.getNamedItem("xmi:id").getNodeValue
    val classAttributeNodes = doXPathQueryGetAsSeqOfNodes(xPath, xmiDocument,
      s"//element[@idref='$classId' and @type='uml:Class']//attribute")
    val attributes = classAttributeNodes.map { attributeNode =>
      val attributeName = attributeNode.getAttributes.getNamedItem("name").getNodeValue
      val attributeId = attributeNode.getAttributes.getNamedItem("xmi:idref").getNodeValue
      val attributeDefinition = doXPathQueryGetAsString(xPath, xmiDocument,
        s"//attribute[@idref='$attributeId']/documentation/@value")
      val attributeType = doXPathQueryGetAsString(xPath, xmiDocument,
        s"//attribute[@idref='$attributeId']/properties/@type")
      val attributeBounds = AttributeBounds(
        doXPathQueryGetAsString(xPath, xmiDocument, s"//attribute[@idref='$attributeId']/bounds/@lower"),
        doXPathQueryGetAsString(xPath, xmiDocument, s"//attribute[@idref='$attributeId']/bounds/@upper")
      )
      XMIAttribute(attributeName, attributeType, attributeBounds, attributeDefinition)
    }

    attributes.foreach { xmiAttribute =>
      val attributeBuilder = UMLAttribute.builder(umlClassIdentity.ontologyPrefix)
        .withClassIdentity(umlClassIdentity)
        .withNameOrCurie(xmiAttribute.name)
        .withType(xmiAttribute.typeOfAttribute)
        .withMultiplicity(xmiAttribute.bounds.lowerBound, xmiAttribute.bounds.upperBound)
        .withDefinition(xmiAttribute.definition)
        .build
    }

  private def doXPathQueryGetAsSeqOfNodes(xPath: XPath, node: Node, query: String): immutable.Seq[Node] =
    try {
      nodeListToSeq(xPath
        .compile(query)
        .evaluate(node, XPathConstants.NODESET)
        .asInstanceOf[NodeList])
    }
    catch {
      case e: XPathExpressionException =>
        logger.error(s"Error while executing XPath query $query on node $node: ${e.getMessage}", e)
        Seq.empty
    }

  private def doXPathQueryGetAsString(xPath: XPath, node: Node, query: String): String =
    xPath
      .compile(query)
      .evaluate(node, XPathConstants.STRING)
      .asInstanceOf[String]

  private def nodeListToSeq(nodeList: NodeList): Seq[Node] =
    IntStream
      .range(0, nodeList.getLength)
      .mapToObj(nodeList.item)
      .collect(Collectors.toList)
      .asScala
      .toSeq

  private class XMIGeneralizationSet(parent: String, children: mutable.Set[String],
                                     complete: Boolean=false, disjoint: Boolean=true):
    def isComplete: Boolean = complete
    def isDisjoint: Boolean = disjoint
    private def getParent: String = parent
    def getChildren: immutable.Set[String] = children.toSet

    def merge(other: XMIGeneralizationSet): XMIGeneralizationSet =
      if (this == other) then this
      else if this.parent == other.getParent &&
        this.isComplete == other.isComplete && this.isDisjoint == other.isDisjoint then
        XMIGeneralizationSet(this.parent, children ++ other.getChildren, this.isComplete, this.isDisjoint)
      else
        throw new IllegalArgumentException("Generalization sets must have the same complete and disjoint attributes.")

  private case class AttributeBounds(lowerBound: String, upperBound: String)
  private case class XMIAttribute(name: String, typeOfAttribute: String, bounds: AttributeBounds, definition:String)
