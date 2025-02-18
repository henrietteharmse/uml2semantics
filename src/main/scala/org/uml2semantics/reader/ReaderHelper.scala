package org.uml2semantics.reader

import org.uml2semantics.model.UMLClass.ClassBuilder
import org.uml2semantics.model.{PrefixNamespace, UMLClass, UMLClassIdentity}
import org.uml2semantics.model.cache.{ClassBuilderCache, ClassIdentityBuilderCache}

import scala.collection.mutable

object ReaderHelper:
  def populateParentsWithTheirChildren(parentToChildrenMap: mutable.Map[String, mutable.Set[String]],
                                       ontologyPrefix: PrefixNamespace): Unit =

    var classesToRebuild = scala.collection.mutable.Set[ClassBuilder]()

    parentToChildrenMap.foreach { (parent, children) =>
      val parentClassIdentity = ClassIdentityBuilderCache.getUMLClassIdentity(parent).getOrElse(
        UMLClassIdentity.builder(ontologyPrefix).withNameOrCurie(parent).build)
      val classBuilder = ClassBuilderCache.getUMLClassBuilder(parentClassIdentity).getOrElse(
          UMLClass.builder(ontologyPrefix).withNameOrCurie(parent))
        .withChildren(parent, children.toSet)
      classesToRebuild += classBuilder
    }

    classesToRebuild.foreach(classBuilder =>
      val umlClass = classBuilder.build
      ClassBuilderCache.cacheUMLClass(umlClass, classBuilder)
    )

