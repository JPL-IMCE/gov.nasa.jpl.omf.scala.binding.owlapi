/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2015, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.binding.owlapi

import java.io.{File, IOException}
import java.net.{MalformedURLException, URI, URL}
import java.lang.Throwable

import org.apache.xml.resolver.{Catalog, CatalogManager}
import org.apache.xml.resolver.tools.CatalogResolver
import org.semanticweb.owlapi.annotations.HasPriority
import org.semanticweb.owlapi.model.{IRI, OWLOntologyIRIMapper}

import scala.{Option,None,Some,StringContext,Unit}
import scala.Predef.{Set=>_,Map=>_,_}
import scala.util.{Failure, Success, Try}

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

  def parseCatalog(catalogURI: URI): Try[Unit] =
    Try(catalog.parseCatalog(catalogURI.toURL))

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