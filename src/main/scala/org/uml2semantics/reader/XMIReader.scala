package org.uml2semantics.reader

import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.diagnostics.{Diagnostic, DiagnosticCode, Diagnostics, SourceLocation, UMLModelException}
import org.uml2semantics.inline.Code
import org.uml2semantics.model.*
import org.uml2semantics.model.UMLClass.ClassBuilder
import org.uml2semantics.model.cache.{AttributeCache, ClassBuilderCache, ClassIdentityBuilderCache, EnumerationCache}
import org.w3c.dom.{Document, Node}

import javax.xml.parsers.DocumentBuilderFactory
import scala.collection.{immutable, mutable}
import scala.util.control.NonFatal

/**
 * Reads an Enterprise Architect XMI export. What is read, and from where in the file, is specified
 * in `docs/specs/xmi-enumerations.md` and `docs/specs/xmi-associations.md`.
 */
object XMIReader extends UMLClassDiagramReader :
  val logger = Logger("XMIReader")

  private val ClassType = "uml:Class"
  private val EnumerationType = "uml:Enumeration"

  override def parseUMLClassDiagram(input: InputParameters): Seq[Diagnostic] =
    logger.debug(s"start xmi parsing ${Code.source}")
    val reading = Reading(
      XMIFile(DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(input.xmiFile.get)),
      PrefixNamespace(input.ontologyPrefix),
      new Diagnostics,
      SourceLocation(input.xmiFile.get.getName))

    // Classes, then enumerations, then attributes, then associations, whatever the order of the
    // document. An attribute type or an association end naming an enumeration then finds it
    // instead of building a placeholder class, an unnamed association can be checked against the
    // attributes that may support it, and a named association has the last word over an attribute
    // of the same name.
    val classes = parseClasses(reading)
    parseEnumerations(reading)
    classes.foreach((classNode, classIdentity) => parseClassAttributes(reading, classNode, classIdentity))
    parseAssociations(reading)
    reading.diagnostics.toSeq

  /** What every step of reading one file needs. */
  private case class Reading(xmi: XMIFile, ontologyPrefix: PrefixNamespace, diagnostics: Diagnostics,
                             location: SourceLocation):
    def error(code: DiagnosticCode, message: String): Unit =
      logger.error(message)
      diagnostics.error(code, message, Some(location))

  // --- Classes and generalization ------------------------------------------------------------

  /** Reads every class with its definition and its generalizations, and returns the ones it kept. */
  private def parseClasses(reading: Reading): Seq[(Node, UMLClassIdentity)] =
    val parentToGeneralizationSetMap = mutable.Map[String, XMIGeneralizationSet]()

    val classes = reading.xmi.packagedElements(ClassType).flatMap { classNode =>
      val classIdentifier = attribute(classNode, "name").getOrElse("")
      try
        val definition = parseDefinition(reading.xmi, classNode, ClassType, Some(classIdentifier))
        var classBuilder = UMLClass.builder(reading.ontologyPrefix)
          .withNameOrCurie(classIdentifier)
        if definition.nonEmpty then
          classBuilder = classBuilder.withDefinition(definition)
        val umlClass = classBuilder.build
        parseClassParents(reading, classNode, parentToGeneralizationSetMap)
        Some(classNode -> umlClass.classIdentity)
      catch
        case e: UMLModelException =>
          reading.error(e.code, s"Class '$classIdentifier' was not read: ${e.getMessage}")
          None
    }

    populateParentsWithTheirChildren(reading, parentToGeneralizationSetMap)
    classes

  /** The definition Enterprise Architect keeps for a class or an enumeration in its extension part. */
  private def parseDefinition(xmi: XMIFile, classifierNode: Node, classifierType: String,
                              name: Option[String] = None): String =
    xmiValue(classifierNode, "id").toSeq
      .flatMap(xmi.extensionElements(_, classifierType))
      .filter(element => name.forall(attribute(element, "name").contains))
      .flatMap(children(_, "properties"))
      .flatMap(attribute(_, "documentation"))
      .headOption.getOrElse("")

  private def parseClassParents(reading: Reading, childNode: Node,
                                parentToGeneralizationSetMap: mutable.Map[String, XMIGeneralizationSet]): Unit =
    val childName = attribute(childNode, "name").getOrElse("")

    try {
      children(childNode, "generalization").foreach { generalizationNode =>
        val parentId = generalizationNode.getAttributes.getNamedItem("general").getNodeValue
        val parentNodes = reading.xmi.packagedElementsById(parentId)
        parentNodes.find(isOfType(ClassType)).flatMap(attribute(_, "name")) match
          case Some(parentName) =>
            val generalizationId = generalizationNode.getAttributes.getNamedItem("xmi:id").getNodeValue
            val generalizationSet = reading.xmi.generalizationSetOf(generalizationId) match
              case Some(generalizationSetNode) =>
                val isCovering = attribute(generalizationSetNode, "isCovering").exists(_.toBoolean)
                val isDisjoint = attribute(generalizationSetNode, "isDisjoint").exists(_.toBoolean)
                XMIGeneralizationSet(parentName, mutable.Set(childName), isCovering, isDisjoint)
              case None =>
                XMIGeneralizationSet(parentName, mutable.Set(childName))

            parentToGeneralizationSetMap.put(parentName,
              parentToGeneralizationSetMap.get(parentName).map(generalizationSet.merge).getOrElse(generalizationSet))
          case None if parentNodes.exists(isOfType(EnumerationType)) =>
            val enumerationName = parentNodes.find(isOfType(EnumerationType)).flatMap(attribute(_, "name")).getOrElse("")
            reading.error(DiagnosticCode.ClassifierKindConflict,
              s"The generalization of class '$childName' was not read: its parent '$enumerationName' is an " +
                s"enumeration, and an enumeration takes no part in generalization.")
          case None =>
            reading.error(DiagnosticCode.RowRejected,
              s"The generalization of class '$childName' was not read: its parent is not a class in this file.")
      }
    } catch {
      case t: Throwable =>
        logger.error(s"Error while extracting parents of class $childName: ${t.getMessage}", t)
    }

  private def populateParentsWithTheirChildren(reading: Reading,
                                               parentToChildrenMap: mutable.Map[String, XMIGeneralizationSet]): Unit =

    var classesToRebuild = scala.collection.mutable.Set[ClassBuilder]()

    parentToChildrenMap.foreach { (parent, generalizationSet) =>
      try
        val parentClassIdentity = ClassIdentityBuilderCache.getUMLClassIdentity(parent).getOrElse(
          UMLClassIdentity.builder(reading.ontologyPrefix).withNameOrCurie(parent).build)
        val classBuilder = ClassBuilderCache.getUMLClassBuilder(parentClassIdentity).getOrElse(
            UMLClass.builder(reading.ontologyPrefix).withNameOrCurie(parent))
          .withChildren(parent, generalizationSet.getChildren)
          .withCompleteDisjoint(generalizationSet.isComplete, generalizationSet.isDisjoint)
        classesToRebuild += classBuilder
      catch
        case e: UMLModelException =>
          reading.error(e.code, s"Parent '$parent' of " +
            s"${generalizationSet.getChildren.toSeq.sorted.mkString("'", "', '", "'")} was not read: ${e.getMessage}")
    }

    classesToRebuild.foreach(classBuilder => classBuilder.build)

  // --- Enumerations --------------------------------------------------------------------------

  /**
   * Reads every enumeration with its literals. An enumeration that cannot be honoured is reported
   * and takes its literals with it. A literal that cannot be honoured leaves its enumeration and
   * its other literals in place.
   */
  private def parseEnumerations(reading: Reading): Unit =
    reading.xmi.packagedElements(EnumerationType).foreach { enumerationNode =>
      val enumerationIdentifier = attribute(enumerationNode, "name").getOrElse("")
      try
        val umlEnumeration = UMLEnumeration.builder(reading.ontologyPrefix)
          .withNameOrCurie(enumerationIdentifier)
          .withDefinition(parseDefinition(reading.xmi, enumerationNode, EnumerationType))
          .build
        EnumerationCache.recordDeclaration(umlEnumeration.identity, reading.location)

        parseEnumerationValues(reading, enumerationNode, enumerationIdentifier)

        // An association end that Enterprise Architect exports as an owned attribute is left to
        // the association it belongs to, which reports it there.
        children(enumerationNode, "ownedAttribute").filter(attribute(_, "association").isEmpty)
          .foreach(attributeNode =>
            reading.error(DiagnosticCode.ClassifierKindConflict,
              s"Attribute '${attribute(attributeNode, "name").getOrElse("")}' of enumeration " +
                s"'$enumerationIdentifier' was not read: an enumeration owns no attributes."))
        children(enumerationNode, "generalization").foreach(_ =>
          reading.error(DiagnosticCode.ClassifierKindConflict,
            s"A generalization of enumeration '$enumerationIdentifier' was not read: an enumeration takes no " +
              s"part in generalization."))
      catch
        case e: UMLModelException =>
          reading.error(e.code, s"Enumeration '$enumerationIdentifier' was not read: ${e.getMessage}")
    }

  private def parseEnumerationValues(reading: Reading, enumerationNode: Node, enumerationIdentifier: String): Unit =
    children(enumerationNode, "ownedLiteral").filter(isOfType("uml:EnumerationLiteral")).foreach { literalNode =>
      val valueIdentifier = attribute(literalNode, "name").getOrElse("")
      try
        UMLEnumerationValue.builder(reading.ontologyPrefix)
          .withEnumeration(enumerationIdentifier)
          .withNameOrCurie(valueIdentifier)
          .withDefinition(xmiValue(literalNode, "id")
            .map(reading.xmi.extensionAttributeValue(_, "documentation", "value")).getOrElse(""))
          .build
      catch
        case e: UMLModelException =>
          reading.error(e.code,
            s"Value '$valueIdentifier' of enumeration '$enumerationIdentifier' was not read: ${e.getMessage}")
    }

  // --- Attributes ----------------------------------------------------------------------------

  private def parseClassAttributes(reading: Reading, classNode: Node, umlClassIdentity: UMLClassIdentity): Unit =
    val diagnostics = reading.diagnostics
    val location = reading.location

    val classAttributeNodes = xmiValue(classNode, "id").toSeq
      .flatMap(reading.xmi.extensionElements(_, ClassType))
      .flatMap(descendants(_, "attribute"))
    val attributes = classAttributeNodes.flatMap { attributeNode =>
      try
        val attributeName = attributeNode.getAttributes.getNamedItem("name").getNodeValue
        val attributeId = attributeNode.getAttributes.getNamedItem("xmi:idref").getNodeValue
        val attributeDefinition = reading.xmi.extensionAttributeValue(attributeId, "documentation", "value")
        val attributeType = reading.xmi.extensionAttributeValue(attributeId, "properties", "type")
        val attributeBounds = AttributeBounds(
          reading.xmi.extensionAttributeValue(attributeId, "bounds", "lower"),
          reading.xmi.extensionAttributeValue(attributeId, "bounds", "upper")
        )
        Some(XMIAttribute(attributeName, attributeType, attributeBounds, attributeDefinition))
      catch
        case NonFatal(t) =>
          logger.error(s"Error while extracting an attribute of class ${umlClassIdentity.getLabel}: ${t.getMessage}", t)
          diagnostics.error(DiagnosticCode.RowRejected,
            s"Could not extract an attribute of class ${umlClassIdentity.getLabel}: ${t.getMessage}", Some(location))
          None
    }

    attributes.foreach { xmiAttribute =>
      try
        UMLAttribute.builder(umlClassIdentity.ontologyPrefix)
          .withClassIdentity(umlClassIdentity)
          .withNameOrCurie(xmiAttribute.name)
          .withType(xmiAttribute.typeOfAttribute, diagnostics, Some(location))
          .withMultiplicity(xmiAttribute.bounds.lowerBound, xmiAttribute.bounds.upperBound,
            diagnostics, Some(location))
          .withDefinition(xmiAttribute.definition)
          .build
      catch
        case e: UMLModelException =>
          logger.error(s"Skipped attribute ${xmiAttribute.name} of class ${umlClassIdentity.getLabel}: ${e.getMessage}")
          diagnostics.error(e.code, e.getMessage, Some(location))
        case NonFatal(t) =>
          logger.error(s"Error while processing attribute ${xmiAttribute.name} of class " +
            s"${umlClassIdentity.getLabel}: ${t.getMessage}", t)
          diagnostics.error(DiagnosticCode.RowRejected, t.getMessage, Some(location))
    }

  // --- Associations --------------------------------------------------------------------------

  /** One end of an association, as Enterprise Architect records it in its extension part. */
  private case class AssociationEnd(classifier: String, kind: String, roleName: Option[String],
                                    multiplicity: String, definition: String):
    def isEnumeration: Boolean = kind == "Enumeration"
    def isClassifier: Boolean = kind == "Class" || isEnumeration

  private def parseAssociationEnd(connectorNode: Node, end: String): AssociationEnd =
    def valueOf(child: String, attributeName: String): Option[String] =
      children(connectorNode, end).flatMap(children(_, child)).flatMap(attribute(_, attributeName)).headOption

    AssociationEnd(
      valueOf("model", "name").getOrElse(""),
      valueOf("model", "type").getOrElse(""),
      valueOf("role", "name").filterNot(_.isBlank),
      valueOf("type", "multiplicity").getOrElse(""),
      valueOf("documentation", "value").getOrElse(""))

  /**
   * Reads every association. An association belongs to its source: its name, or the role name on
   * its target end, becomes an attribute of the source class typed by the target classifier. A
   * role name on the source end gives a second attribute in the opposite direction. An
   * association becomes attributes and nothing else, so everything from here on, the translation
   * included, is what it is for an attribute.
   */
  private def parseAssociations(reading: Reading): Unit =
    reading.xmi.connectors("Association").foreach { connectorNode =>
      val name = attribute(connectorNode, "name").filterNot(_.isBlank)
      val source = parseAssociationEnd(connectorNode, "source")
      val target = parseAssociationEnd(connectorNode, "target")
      val association = name.map(name => s"Association '$name'").getOrElse("The unnamed association") +
        s" from '${source.classifier}' to '${target.classifier}'"

      val forwardName = target.roleName.orElse(name)
      val backwardName = source.roleName

      Seq(source, target).find(!_.isClassifier) match
        case Some(end) =>
          reading.error(DiagnosticCode.RowRejected,
            s"$association was not read: '${end.classifier}' is neither a class nor an enumeration.")
        case None if forwardName.isEmpty && backwardName.isEmpty =>
          if !hasSupportingAttribute(source, target) then
            reading.error(DiagnosticCode.UnnamedAssociation,
              s"$association was not read: it has no name and no role name to name an attribute with, and no " +
                s"attribute of either class states it.")
        case None =>
          forwardName.foreach(attributeName =>
            val definition = if target.roleName.isDefined then target.definition
              else children(connectorNode, "documentation").flatMap(attribute(_, "value")).headOption.getOrElse("")
            buildAssociationAttribute(reading, association, source, attributeName, target, definition))
          backwardName.foreach(attributeName =>
            buildAssociationAttribute(reading, association, target, attributeName, source, source.definition))
    }

  /** Builds the attribute `attributeName` of the class at `owner`, typed by the classifier at `typedBy`. */
  private def buildAssociationAttribute(reading: Reading, association: String, owner: AssociationEnd,
                                        attributeName: String, typedBy: AssociationEnd, definition: String): Unit =
    // Collected apart, so that each finding can name the association it is about, and so that a
    // multiplicity such as 'many', which is both the minimum and the maximum, is reported once.
    val attributeDiagnostics = new Diagnostics
    try
      if owner.isEnumeration || EnumerationCache.getUMLEnumeration(owner.classifier).isDefined then
        throw new UMLModelException(DiagnosticCode.ClassifierKindConflict,
          s"'${owner.classifier}' is an enumeration, and an enumeration owns no attributes.")
      val classIdentity = UMLClassIdentity.builder(reading.ontologyPrefix)
        .withNameOrCurie(owner.classifier)
        .build
      val (minMultiplicity, maxMultiplicity) = multiplicityBounds(typedBy.multiplicity)

      UMLAttribute.builder(reading.ontologyPrefix)
        .withClassIdentity(classIdentity)
        .withNameOrCurie(attributeName)
        .withType(typedBy.classifier, attributeDiagnostics, Some(reading.location))
        .withMultiplicity(minMultiplicity, maxMultiplicity, attributeDiagnostics, Some(reading.location))
        .withDefinition(definition)
        .build

      // As for a row of the TSV attributes file: only an attribute that was accepted is evidence
      // that its owner exists.
      if ClassBuilderCache.getUMLClass(classIdentity).isEmpty then
        UMLClass.builder(reading.ontologyPrefix).withNameOrCurie(owner.classifier).build
    catch
      case e: UMLModelException =>
        reading.error(e.code,
          s"$association: attribute '$attributeName' of '${owner.classifier}' was not read. ${e.getMessage}")
      case NonFatal(t) =>
        reading.error(DiagnosticCode.RowRejected,
          s"$association: attribute '$attributeName' of '${owner.classifier}' was not read. ${t.getMessage}")
    finally
      attributeDiagnostics.toSeq.distinct.foreach(diagnostic =>
        reading.diagnostics.add(diagnostic.copy(message = s"$association: ${diagnostic.message}")))

  /**
   * The minimum and maximum of a multiplicity as it is written on an association end: `n` is
   * [n,n], `m..n` is [m,n] and `*` is [0,*]. An end without a multiplicity gives two blanks, which
   * the model reads as the UML default of 1.
   */
  private def multiplicityBounds(multiplicity: String): (String, String) =
    multiplicity.trim match
      case "*" => ("0", "*")
      case range if range.contains("..") =>
        val bounds = range.split("\\.\\.", 2)
        (bounds(0).trim, bounds(1).trim)
      case single => (single, single)

  /**
   * Whether an attribute already states the association between two ends: an attribute of the
   * class at either end, typed by the classifier at the other end, whatever its name and
   * multiplicity.
   */
  private def hasSupportingAttribute(source: AssociationEnd, target: AssociationEnd): Boolean =
    def identifies(classIdentity: UMLClassIdentity, end: AssociationEnd): Boolean =
      classIdentity.nameOption.exists(_.getName == end.classifier) ||
        classIdentity.curieOption.exists(_.getName == end.classifier)

    def isTypedBy(umlAttribute: UMLAttribute, end: AssociationEnd): Boolean =
      umlAttribute.typeOfAttribute.exists {
        case UMLClassType(umlClass) => identifies(umlClass.classIdentity, end)
        case UMLEnumerationType(enumerationIdentity) => enumerationIdentity.isIdentifiedBy(end.classifier)
        case CurieBasedUMLClassAttributeType(curie) => curie.curie == end.classifier
        case UMLXMLDataType(_) => false
      }

    AttributeCache.getAttributes.exists(umlAttribute =>
      val owner = umlAttribute.attributeIdentity.classIdentity
      (identifies(owner, source) && isTypedBy(umlAttribute, target)) ||
        (identifies(owner, target) && isTypedBy(umlAttribute, source)))

  // --- The document --------------------------------------------------------------------------
  //
  // The document is walked once and what the reader looks things up by is indexed, instead of
  // searching the whole document for every class, generalization, attribute and literal. A model of
  // a few thousand classes cannot be read otherwise.
  //
  // The DocumentBuilderFactory is deliberately left namespace-UNaware, so an attribute keeps the
  // name it has in the file: "xmi:type", "xmi:id", "xmi:idref". `xmiValue` reads such an
  // attribute by its local name, with or without the prefix.

  private class XMIFile(document: Document):
    private val elementsByTag: Map[String, Seq[Node]] =
      val found = mutable.Map[String, mutable.ArrayBuffer[Node]]()
      val wanted = Set("packagedElement", "element", "attribute", "generalization", "connector")

      def walk(node: Node): Unit =
        var child = node.getFirstChild
        while child != null do
          if child.getNodeType == Node.ELEMENT_NODE then
            if wanted.contains(child.getNodeName) then
              found.getOrElseUpdate(child.getNodeName, mutable.ArrayBuffer()) += child
            walk(child)
          child = child.getNextSibling

      walk(document)
      found.view.mapValues(_.toSeq).toMap

    private def elements(tag: String): Seq[Node] = elementsByTag.getOrElse(tag, Seq.empty)

    private def indexed(nodes: Seq[Node], key: String): Map[String, Seq[Node]] =
      nodes.flatMap(node => xmiValue(node, key).map(_ -> node)).groupMap(_._1)(_._2)

    private val packagedElementsByIdIndex = indexed(elements("packagedElement"), "id")
    private val extensionElementsByIdref = indexed(elements("element"), "idref")
    private val extensionAttributesByIdref = indexed(elements("attribute"), "idref")
    // A generalization set lists its members as references to the generalizations of the classes.
    private val generalizationSetMembersByIdref =
      indexed(elements("generalization").filter(_.getParentNode.getNodeName == "packagedElement"), "idref")

    /** Every packaged element of a type, at any package depth, in document order. */
    def packagedElements(xmiType: String): Seq[Node] = elements("packagedElement").filter(isOfType(xmiType))

    def packagedElementsById(id: String): Seq[Node] = packagedElementsByIdIndex.getOrElse(id, Seq.empty)

    /** What Enterprise Architect's extension part records for a classifier of the standard part. */
    def extensionElements(idref: String, xmiType: String): Seq[Node] =
      extensionElementsByIdref.getOrElse(idref, Seq.empty).filter(isOfType(xmiType))

    /**
     * A value Enterprise Architect's extension part records for an attribute or an enumeration
     * literal, which it also lists as an attribute: blank when it records none.
     */
    def extensionAttributeValue(idref: String, child: String, attributeName: String): String =
      extensionAttributesByIdref.getOrElse(idref, Seq.empty)
        .flatMap(children(_, child))
        .flatMap(attribute(_, attributeName))
        .headOption.getOrElse("")

    def generalizationSetOf(generalizationId: String): Option[Node] =
      generalizationSetMembersByIdref.getOrElse(generalizationId, Seq.empty).headOption.map(_.getParentNode)

    /** The connectors of the extension part that are of the given Enterprise Architect type. */
    def connectors(eaType: String): Seq[Node] =
      elements("connector").filter(_.getParentNode.getNodeName == "connectors")
        .filter(children(_, "properties").flatMap(attribute(_, "ea_type")).headOption.contains(eaType))

  private def attribute(node: Node, name: String): Option[String] =
    Option(node.getAttributes).flatMap(attributes => Option(attributes.getNamedItem(name))).map(_.getNodeValue)

  private def xmiValue(node: Node, localName: String): Option[String] =
    attribute(node, s"xmi:$localName").orElse(attribute(node, localName))

  private def isOfType(xmiType: String)(node: Node): Boolean = xmiValue(node, "type").contains(xmiType)

  private def children(node: Node, tag: String): Seq[Node] =
    val found = mutable.ArrayBuffer[Node]()
    var child = node.getFirstChild
    while child != null do
      if child.getNodeType == Node.ELEMENT_NODE && child.getNodeName == tag then found += child
      child = child.getNextSibling
    found.toSeq

  private def descendants(node: Node, tag: String): Seq[Node] =
    val found = mutable.ArrayBuffer[Node]()
    def walk(parent: Node): Unit =
      var child = parent.getFirstChild
      while child != null do
        if child.getNodeType == Node.ELEMENT_NODE then
          if child.getNodeName == tag then found += child
          walk(child)
        child = child.getNextSibling
    walk(node)
    found.toSeq

  private class XMIGeneralizationSet(parent: String, children: mutable.Set[String],
                                     complete: Boolean=false, disjoint: Boolean=false):
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
