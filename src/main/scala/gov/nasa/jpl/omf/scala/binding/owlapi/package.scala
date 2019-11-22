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

package gov.nasa.jpl.omf.scala.binding

import java.nio.file.Path

import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{ImmutableModule, MutableModule}
import gov.nasa.jpl.omf.scala.core.generateUUIDFromString
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.OMFError
import org.apache.xml.resolver.{Catalog, CatalogManager}
import org.apache.xml.resolver.tools.CatalogResolver
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model._

import scala.Ordering
import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.{Boolean, Int, None, Option, StringContext, Unit}
import scala.Predef.{augmentString,require,ArrowAssoc,String}
import scalaz._
import Scalaz._

package object owlapi {

  implicit val iriOrdering: Ordering[IRI] = new Ordering[IRI] {
    override def compare(x: IRI, y: IRI): Int
    = x.toString.compareTo(y.toString)
  }
  implicit val owlObjectProperty: Ordering[OWLObjectProperty] = new Ordering[OWLObjectProperty] {
    override def compare(x: OWLObjectProperty, y: OWLObjectProperty): Int
    = x.getIRI.toString.compareTo(y.getIRI.toString)
  }
  implicit val owlClassOrdering: Ordering[OWLClass] = new Ordering[OWLClass] {
    override def compare(x: OWLClass, y: OWLClass): Int
    = x.getIRI.toString.compareTo(y.getIRI.toString)
  }

  type MutableModulesNES
  = Throwables \/ Set[MutableModule]

  val emptyMutableModulesNES
  : MutableModulesNES
  = \/-(Set())

  type ImmutableModulesNES
  = Throwables \/ Set[ImmutableModule]

  val emptyImmutableModulesNES
  : ImmutableModulesNES
  = \/-(Set())

  def catalogURIMapperException
  (message: String,
   cause: OMFError.Throwables = OMFError.emptyThrowables)
  : java.lang.Throwable
  = new CatalogURIMapperException(message, cause)

  def catalogURIMapperException
  (message: String,
   cause: java.lang.Throwable)
  : java.lang.Throwable
  = new CatalogURIMapperException(message, Set[java.lang.Throwable](cause))

  def applyOntologyChange
  (ontManager: OWLOntologyManager,
   ontChange: OWLOntologyChange,
   ifError: String,
   ifSuccess: Option[() => Unit] = None)
  : Throwables \/ Unit
  = ontManager.applyChange(ontChange) match {
    case ChangeApplied.SUCCESSFULLY =>
      ifSuccess
      .fold[Throwables \/ Unit](
        \/-(())
      ){ callback =>
        \/.fromTryCatchNonFatal[Unit](callback())
        .fold[Throwables \/ Unit](
        l = (t: java.lang.Throwable) =>
          -\/(
            Set(OMFError.omfBindingException(ifError, t))
          ),
        r = (_: Unit) =>
          \/-(())
        )
      }
    case ChangeApplied.NO_OPERATION =>
      Set(
        OMFError.omfBindingError(s"$ifError (no-operation change)")
      ).left
    case ChangeApplied.UNSUCCESSFULLY =>
      Set(
        OMFError.omfBindingError(s"$ifError (unsuccessful change)")
      ).left
  }

  def applyOntologyChanges
  (ontManager: OWLOntologyManager,
   ontChanges: Seq[OWLOntologyChange],
   ifError: => String,
   ifSuccess: => Option[() => Unit] = None)
  : Throwables \/ Unit
  = ontChanges.foldLeft[Throwables \/ Unit](\/-(())) { case (acc, ontChange) =>
    acc.flatMap { _ =>
      applyOntologyChange(ontManager, ontChange, ifError, ifSuccess)
    }
  }

  def applyOntologyChangeOrNoOp
  (ontManager: OWLOntologyManager,
   ontChange: OWLOntologyChange,
   ifError: => String,
   ifSuccess: => Option[() => Unit] = None)
  : Throwables \/ Unit
  = ontManager.applyChange(ontChange) match {
      case ChangeApplied.SUCCESSFULLY | ChangeApplied.NO_OPERATION =>
        ifSuccess
          .fold[Throwables \/ Unit](
          \/-(())
        ){ callback =>
          \/.fromTryCatchNonFatal[Unit](callback())
            .fold[Throwables \/ Unit](
            l = (t: java.lang.Throwable) =>
              -\/(
                Set(OMFError.omfBindingException(ifError, t))
              ),
            r = (_: Unit) =>
              \/-(())
          )
        }
      case ChangeApplied.UNSUCCESSFULLY =>
        Set(
          OMFError.omfBindingError(s"$ifError (unsuccessful change)")
        ).left
    }

  def applyOntologyChangesOrNoOp
  (ontManager: OWLOntologyManager,
   ontChanges: Seq[OWLOntologyChange],
   ifError: => String,
   ifSuccess: => Option[() => Unit] = None)
  : Throwables \/ Unit
  = ontChanges.foldLeft[Throwables \/ Unit](\/-(())) { case (acc, ontChange) =>
      acc.flatMap { _ =>
        applyOntologyChangeOrNoOp(ontManager, ontChange, ifError, ifSuccess)
      }
  }

  def findAnnotationAssertionAxiom
  ( ont: OWLOntology,
    resourceIRI: IRI,
    annotationProperty: IRI)
  : Option[OWLAnnotationAssertionAxiom]
  = ont
    .annotationAssertionAxioms( resourceIRI )
    .toScala[Set]
    .find( _.getProperty.getIRI == annotationProperty )

  def createOMFGraphStore
  (catalogManager: CatalogManager,
   catalogResolver: CatalogResolver,
   catalog: Catalog,
   ontManager: OWLOntologyManager = OWLManager.createOWLOntologyManager(),
   excludeOMLImports: Boolean,
   excludeOMLContent: Boolean,
   excludePurlImports: Boolean)
  : Throwables \/ OWLAPIOMFGraphStore
  = for {
    module <- OWLAPIOMFModule.owlAPIOMFModule(catalogManager)
    store = OWLAPIOMFGraphStore.initGraphStore(module, ontManager, catalogResolver, catalog, excludeOMLImports = excludeOMLImports, excludeOMLContent = excludeOMLContent, excludePurlImports = excludePurlImports)
  } yield store

  def loadCatalog(s: OWLAPIOMFGraphStore, cls: java.lang.Class[_], catalogPath: String)
  : Throwables \/ Unit
  = Option.apply(cls.getResource(catalogPath))
    .fold[Throwables \/ Unit] {
    Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
      s"Catalog '$catalogPath' not found on the classpath of ${cls.getName}")).left
  } { url =>
    s.catalogIRIMapper.parseCatalog(url.toURI)
  }

  def loadCatalog(s: OWLAPIOMFGraphStore, file: Path)
  : Throwables \/ Unit
  = s.catalogIRIMapper.parseCatalog(file.toUri)

  def getAnnotationPropertyUUIDfromOWLAnnotationProperty
  (ap: OWLAnnotationProperty)
  : resolver.api.taggedTypes.AnnotationPropertyUUID
  = resolver.api.taggedTypes.annotationPropertyUUID(generateUUIDFromString(ap.getIRI.getIRIString))

  val annotationNSPrefixes: Map[String, String] = Map(
    "http://purl.org/dc/terms/" -> "terms:",
    "http://purl.org/dc/elements/1.1/" -> "dc:",
    "http://www.w3.org/2000/01/rdf-schema#" -> "rdfs:",
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" -> "rdf:",
    "http://imce.jpl.nasa.gov/oml/oml#" -> "oml:"
  )

  def getDefaultNSPrefix(aIRI: String)
  : String
  = {
    val hash = aIRI.lastIndexOf('#')
    if (hash > 0) {
      val slash = aIRI.lastIndexOf('/')
      if (slash > 0) {
        val ns = aIRI.substring(slash + 1, hash)
        ns + ":"
      } else
        ""
    } else
      ""
  }

  def getAnnotationPropertyFromOWLAnnotationProperty
  (ap: OWLAnnotationProperty)
  : Throwables \/ tables.AnnotationProperty
  = {
    import gov.nasa.jpl.imce.oml.resolver.Extent2Tables.toUUIDString

    val aIRI = ap.getIRI.getIRIString
    val shortIRI = ap.getIRI.getShortForm
    val abIRI = if (shortIRI.contains(":"))
      shortIRI
    else
      annotationNSPrefixes
        .find { case (ns,_) => aIRI.startsWith(ns) }
        .map { case (_,prefix) => prefix }
        .getOrElse { getDefaultNSPrefix(aIRI) } + shortIRI

    val abModuleIRI =
      annotationNSPrefixes
        .find { case (ns,_) => aIRI.startsWith(ns) }
        .map { case (ns,_) =>
          tables.taggedTypes.iri(ns.stripSuffix("#"))
        }
        .getOrElse {
          require(aIRI.contains("#"))
          val ontIRI = tables.taggedTypes.iri(aIRI.substring(0, aIRI.indexOf('#')))
          ontIRI
        }

    val abModuleUUID =
      tables.taggedTypes.moduleUUID(generateUUIDFromString(abModuleIRI).toString)

    if (abIRI.contains(":"))
      tables.AnnotationProperty(
        getAnnotationPropertyUUIDfromOWLAnnotationProperty(ap),
        abModuleUUID,
        tables.taggedTypes.iri(aIRI),
        tables.taggedTypes.abbrevIRI(abIRI)).right
    else
      Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
        s"Unknown abbreviated IRI for $aIRI (short form=$abIRI)")
      ).left
  }

  def getAnnotationPropertyUUIDfromOWLAnnotation
  (a: OWLAnnotation)
  : resolver.api.taggedTypes.AnnotationPropertyUUID
  = resolver.api.taggedTypes.annotationPropertyUUID(generateUUIDFromString(a.getProperty.getIRI.getIRIString))

  def getAnnotationPropertyFromOWLAnnotation
  (a: OWLAnnotation)
  : Throwables \/ tables.AnnotationProperty
  = getAnnotationPropertyFromOWLAnnotationProperty(a.getProperty)

  def getAnnotationValueFromOWLAnnotation
  (av: OWLAnnotationValue)
  : Throwables \/ tables.taggedTypes.StringDataType
  = av match {
    case i: OWLAnonymousIndividual =>
      Set[java.lang.Throwable](OMFError.omfError(
        s"getAnnotationValueFromOWLAnnotation: an anonymous individual cannot be the value of an annotation in OML"
      )).left
    case i: IRI =>
      tables.taggedTypes.stringDataType(i.getIRIString).right
    case l: OWLLiteral =>
      tables.taggedTypes.stringDataType(l.getLiteral).right
  }

  def getRelevantOntologyAnnotations
  (ont: OWLOntology)
  (implicit store: OWLAPIOMFGraphStore)
  : Vector[OWLAnnotation]
  = if (store.excludeOMLContent)
    ont
    .annotations()
    .toScala[Vector]
    .filterNot { a =>
      val apIRI = a.getProperty.getIRI.getIRIString
      apIRI.startsWith("http://imce.jpl.nasa.gov/foundation")
    }
  else
    ont
      .annotations()
      .toScala[Vector]
      .filterNot { a =>
        val apIRI = a.getProperty.getIRI.getIRIString
        apIRI.startsWith("http://imce.jpl.nasa.gov/foundation") ||
          apIRI.startsWith("http://imce.jpl.nasa.gov/oml")
      }

  def getRelevantSubjectAnnotationAssertions
  (ont: OWLOntology, iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Vector[OWLAnnotationAssertionAxiom]
  = if (store.excludeOMLContent)
    ont
    .annotationAssertionAxioms(iri)
    .toScala[Vector]
    .filterNot { a =>
      val apIRI = a.getProperty.getIRI.getIRIString
      apIRI.startsWith("http://imce.jpl.nasa.gov/foundation")
    }
  else
    ont
      .annotationAssertionAxioms(iri)
      .toScala[Vector]
      .filterNot { a =>
        val apIRI = a.getProperty.getIRI.getIRIString
        apIRI.startsWith("http://imce.jpl.nasa.gov/foundation") ||
          apIRI.startsWith("http://imce.jpl.nasa.gov/oml")
      }

}