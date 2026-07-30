package org.uml2semantics.reader

import com.github.tototoshi.csv.*
import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.diagnostics.{Diagnostic, DiagnosticCode, Diagnostics, SourceLocation, UMLModelException}
import org.uml2semantics.model.*
import org.uml2semantics.model.cache.ClassBuilderCache
import org.uml2semantics.reader.ReaderHelper.populateParentsWithTheirChildren

import java.io.File
import scala.collection.mutable

object TSVReader extends UMLClassDiagramReader:
  val logger = Logger("TSVReader")

  /** The header occupies line 1, so the first data row is line 2. */
  private val FirstDataRowLineNumber = 2

  enum ClassesHeader:
    case Name, Curie, Definition, Parents

  enum ClassAttributesHeader:
    case Class, Curie, Name, ClassEnumOrPrimitiveType, MinMultiplicity, MaxMultiplicity, Definition

  enum EnumerationsHeader:
    case Name, Curie, Definition

  enum EnumerationValuesHeader:
    case Enumeration, Name, Curie, Definition

  override def parseUMLClassDiagram(input: InputParameters): Seq[Diagnostic] =
    val ontologyPrefix = PrefixNamespace(input.ontologyPrefix)
    val diagnostics = new Diagnostics
    if (input.classesTsv.isDefined)
      parseClasses(input.classesTsv, ontologyPrefix, diagnostics)
    if (input.attributesTsv.isDefined)
      parseClassAttributes(input.attributesTsv, ontologyPrefix, diagnostics)
    diagnostics.toSeq

  /**
   * Reads the rows of a TSV file, handing each one to `parseRow` along with its location.
   * A row that throws is recorded and skipped: a `UMLModelException` carries the code that
   * says precisely what was wrong, anything else degrades to a rejected row.
   */
  private def forEachRow(tsvFile: File, diagnostics: Diagnostics)
                        (parseRow: (Map[String, String], SourceLocation) => Unit): Unit =
    implicit object TsvFormat extends TSVFormat {}

    val reader = CSVReader.open(tsvFile)
    val rows = try reader.allWithHeaders() finally reader.close()

    rows.zipWithIndex.foreach { (row, index) =>
      val location = SourceLocation(tsvFile.getName, Some(index + FirstDataRowLineNumber))
      logger.trace(s"row = $row")
      try parseRow(row, location)
      catch
        case e: UMLModelException =>
          logger.error(s"Skipped $location: ${e.getMessage}")
          diagnostics.error(e.code, e.getMessage, Some(location))
        case t: Throwable =>
          logger.error(s"Skipped $location: ${t.getMessage}", t)
          diagnostics.error(DiagnosticCode.RowRejected, t.getMessage, Some(location))
    }

  private def parseClasses(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace,
                           diagnostics: Diagnostics): Unit =
    import ClassesHeader.*
    logger.info("Start")

    val parentToChildrenMap = mutable.Map[String, mutable.Set[String]]()

    forEachRow(maybeTsvFile.get, diagnostics) { (row, _) =>
      extractParents(parentToChildrenMap, Name, Curie, Parents, row)

      UMLClass.builder(ontologyPrefix)
        .withNameAndCurie(row(Name.toString), row(Curie.toString))
        .withDefinition(row(Definition.toString))
        .build
    }

    populateParentsWithTheirChildren(parentToChildrenMap, ontologyPrefix)


  private def extractParents(parentToChildrenMap: mutable.Map[String, mutable.Set[String]],
                             Name: ClassesHeader.Name.type, Curie: ClassesHeader.Curie.type,
                             Parents: ClassesHeader.Parents.type, header: Map[String, String]): Unit =
    val parentSet = {
      val maybeParent = header.get(Parents.toString)
      logger.trace(s"maybeParent=$maybeParent")
      maybeParent.getOrElse("")
    }.split('|')
      .map(_.trim)
      .filter(_.nonEmpty)
      .toSet

    if parentSet.nonEmpty then
      val child = if header(Curie.toString).nonEmpty then header(Curie.toString) else header(Name.toString)
      parentSet.foreach(parent =>
        parentToChildrenMap.getOrElseUpdate(parent, mutable.HashSet()) += child
      )


  private def parseClassAttributes(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace,
                                   diagnostics: Diagnostics): Unit =
    import ClassAttributesHeader.*
    logger.info("Start")

    forEachRow(maybeTsvFile.get, diagnostics) { (row, location) =>
      val classIdentifier = row(Class.toString)
      val classIdentity = UMLClassIdentity.builder(ontologyPrefix)
        .withNameOrCurie(classIdentifier)
        .build

      UMLAttribute.builder(ontologyPrefix)
        .withClassIdentity(classIdentity)
        .withNameAndCurie(row(Name.toString), row(Curie.toString))
        .withDefinition(row(Definition.toString))
        .withType(row(ClassEnumOrPrimitiveType.toString), diagnostics, Some(location))
        .withMultiplicity(row(MinMultiplicity.toString), row(MaxMultiplicity.toString),
          diagnostics, Some(location))
        .build

      // A class named only as an attribute's owner still becomes a class, mirroring how a parent
      // named only in a Parents column is built on the fly. Without this it would reach the output
      // as a bare declaration — unlabelled, undefined, and exempt from global disjointness.
      //
      // Deliberately after the attribute is built, not before: a row that was rejected should not
      // leave a class behind, so only a valid attribute is evidence that its owner exists.
      if ClassBuilderCache.getUMLClass(classIdentity).isEmpty then
        UMLClass.builder(ontologyPrefix).withNameOrCurie(classIdentifier).build
    }
