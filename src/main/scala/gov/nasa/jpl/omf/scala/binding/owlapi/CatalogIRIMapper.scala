/*
 * Copyright 2015 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package gov.nasa.jpl.omf.scala.binding.owlapi

import java.io.{File, IOException}
import java.net.{MalformedURLException, URI, URL}
import java.lang.Throwable

import org.apache.xml.resolver.{Catalog, CatalogManager}
import org.apache.xml.resolver.tools.CatalogResolver
import org.semanticweb.owlapi.annotations.HasPriority
import org.semanticweb.owlapi.model.{IRI, OWLOntologyIRIMapper}

import gov.nasa.jpl.omf.scala.core.OMFError

import scala.collection.immutable.Set
import scala.{Option,None,Some,StringContext,Unit}
import scala.Predef.{Set=>_,Map=>_,_}
import scala.util.control.Exception._
import scalaz._, Scalaz._

class CatalogURIMapperException
(override val message: String,
 override val cause: OMFError.Throwables = OMFError.emptyThrowables)
  extends OMFError.OMFException(message, cause)

@HasPriority(0)
case class CatalogIRIMapper
(catalogManager: CatalogManager,
 catalogResolver: CatalogResolver,
 catalog: Catalog) extends OWLOntologyIRIMapper {

  require(null != catalogManager)
  require(null != catalogResolver)
  require(null != catalog)

  def this
  (catalogManager: CatalogManager,
   catalogResolver: CatalogResolver) =
    this(catalogManager, catalogResolver, catalogResolver.getCatalog)

  def this
  (catalogManager: CatalogManager) =
    this(catalogManager, new CatalogResolver(catalogManager))

  def parseCatalog(catalogURI: URI)
  : Set[java.lang.Throwable] \/ Unit =

    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            catalogURIMapperException(
              s"parseCatalog: catalogURI:$catalogURI failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        catalog.parseCatalog(catalogURI.toURL).right
      })

  def getDocumentIRI(ontologyIRI: IRI): IRI =
    resolveIRI(ontologyIRI, loadResolutionStrategy) match {
      case null => ontologyIRI
      case resolvedIRI => resolvedIRI
    }

  def loadResolutionStrategy(resolved: String): Option[IRI] = {

    def ignore(e: Throwable) = {}

    val normalized = new URI(resolved)
    val normalizedPath = normalized.toString

    val f1 = new URL(normalizedPath)
    val f2 =
      if (normalizedPath.endsWith(".owl")) f1
      else new URL(normalizedPath + ".owl")
    try {
      for {
        is <- Option.apply(f2.openStream)
        if is.available() > 0
      } {
        is.close()
        return Some(IRI.create(f2.toString))
      }
    }
    catch {
      case e: IOException => ignore(e)
      // try another variant.
    }
    try {
      for {
        is <- Option.apply(f1.openStream())
        if is.available() > 0
      } {
        is.close()
        return Some(IRI.create(f1.toString))
      }
    }
    catch {
      case e: IOException => ignore(e)
      // try another variant.
    }
    None
  }

  def saveResolutionStrategy(resolved: String): Option[IRI] = {
    val normalized = new URI(resolved)
    val normalizedPath = normalized.toString
    val normalizedOwlPath =
      if (normalizedPath.endsWith(".owl")) normalizedPath
      else normalizedPath + ".owl"
    val f1 = new URL(normalizedOwlPath)
    val outputFile =
      if (resolved.startsWith("file:")) new File(resolved.substring(5))
      else new File(resolved)

    outputFile.getParentFile match {
      case null =>
        None

      case outputDir =>
        if (!outputDir.exists)
          outputDir.mkdirs

        if (outputDir.exists && outputDir.isDirectory && outputDir.canWrite)
          Some(IRI.create(f1.toString))
        else
          None
    }
  }

  def resolveIRI(iri: IRI, resolutionStrategy: (String) => Option[IRI]): IRI = {

    def ignore(e: Throwable) = {}

    val rawPath = iri.toURI.toString
    val iriPath =
      if (rawPath.endsWith("#")) rawPath.substring(0, rawPath.length() - 1)
      else rawPath

    try {
      catalog.resolveURI(iriPath) match {
        case null =>
          iri
        case resolved =>
          resolutionStrategy(resolved) match {
            case None =>
              iri
            case Some(resolvedIRI) =>
              resolvedIRI
          }
      }
    }
    catch {
      case e: MalformedURLException =>
        ignore(e)
        iri
      case e: IOException =>
        ignore(e)
        iri
    }
  }
}