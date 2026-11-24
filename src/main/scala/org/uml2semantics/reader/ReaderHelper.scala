package org.uml2semantics.reader

import com.typesafe.scalalogging.Logger
import org.uml2semantics.diagnostics.{DiagnosticCode, Diagnostics, SourceLocation, UMLModelException}
import org.uml2semantics.model.UMLClass.ClassBuilder
import org.uml2semantics.model.{PrefixNamespace, UMLClass, UMLClassIdentity}
import org.uml2semantics.model.cache.{ClassBuilderCache, ClassIdentityBuilderCache, EnumerationCache}

import scala.collection.mutable

object ReaderHelper:
  private val logger = Logger("ReaderHelper")

  /**
   * Attaches to each parent the classes that named it. A parent that cannot be a class is reported
   * and skipped, leaving its children as they are: the one way that happens is a parent that names
   * an enumeration, which a source read earlier may have supplied.
   */
  def populateParentsWithTheirChildren(parentToChildrenMap: mutable.Map[String, mutable.Set[String]],
                                       ontologyPrefix: PrefixNamespace,
                                       diagnostics: Diagnostics,
                                       location: SourceLocation): Unit =

    var classesToRebuild = scala.collection.mutable.Set[ClassBuilder]()

    parentToChildrenMap.foreach { (parent, children) =>
      try
        val parentClassIdentity = ClassIdentityBuilderCache.getUMLClassIdentity(parent).getOrElse(
          UMLClassIdentity.builder(ontologyPrefix).withNameOrCurie(parent).build)
        val classBuilder = ClassBuilderCache.getUMLClassBuilder(parentClassIdentity).getOrElse(
            UMLClass.builder(ontologyPrefix).withNameOrCurie(parent))
          .withChildren(parent, children.toSet)
        classesToRebuild += classBuilder
      catch
        case e: UMLModelException =>
          val message = s"Parent '$parent' of ${children.toSeq.sorted.mkString("'", "', '", "'")} was not read: " +
            e.getMessage
          logger.error(message)
          diagnostics.error(e.code, message, Some(location))
    }

    classesToRebuild.foreach(classBuilder =>
      val umlClass = classBuilder.build
      ClassBuilderCache.cacheUMLClass(umlClass, classBuilder)
    )

  /**
   * An enumeration without values is accepted, for the same reason a blank attribute type is: the
   * tool cannot tell an omission from a model that is still being drafted, so it keeps the
   * enumeration and says something. This runs once every input has been read, because only then is
   * it known which enumerations ended up with no values: one source may declare an enumeration
   * and another give it its values.
   */
  def reportEnumerationsWithoutValues(diagnostics: Diagnostics): Unit =
    EnumerationCache.getEnumerations.filter(_.values.isEmpty).toSeq.sortBy(_.getLabel).foreach { umlEnumeration =>
      val message = s"Enumeration '${umlEnumeration.getLabel}' has no values."
      logger.warn(message)
      diagnostics.warn(DiagnosticCode.EnumerationWithoutValues, message, EnumerationCache.whereDeclared(umlEnumeration))
    }
