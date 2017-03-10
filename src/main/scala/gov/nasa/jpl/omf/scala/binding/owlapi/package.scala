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

import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty,UUID}
import gov.nasa.jpl.omf.scala.core.{generateUUID,OMFError}
import org.apache.xml.resolver.CatalogManager
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model._

import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.{None, Option, StringContext, Unit}
import scala.Predef.String
import scalaz._
import Scalaz._

package object owlapi {


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
  : Set[java.lang.Throwable] \/ Unit
  = ontManager.applyChange(ontChange) match {
    case ChangeApplied.SUCCESSFULLY =>
      ifSuccess
      .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ){ callback =>
        \/.fromTryCatchNonFatal[Unit](callback())
        .fold[Set[java.lang.Throwable] \/ Unit](
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
  : Set[java.lang.Throwable] \/ Unit
  = ontChanges.foldLeft[Set[java.lang.Throwable] \/ Unit](\/-(())) { case (acc, ontChange) =>
    acc.flatMap { _ =>
      applyOntologyChange(ontManager, ontChange, ifError, ifSuccess)
    }
  }

  def applyOntologyChangeOrNoOp
  (ontManager: OWLOntologyManager,
   ontChange: OWLOntologyChange,
   ifError: => String,
   ifSuccess: => Option[() => Unit] = None)
  : Set[java.lang.Throwable] \/ Unit
  = ontManager.applyChange(ontChange) match {
      case ChangeApplied.SUCCESSFULLY | ChangeApplied.NO_OPERATION =>
        ifSuccess
          .fold[Set[java.lang.Throwable] \/ Unit](
          \/-(())
        ){ callback =>
          \/.fromTryCatchNonFatal[Unit](callback())
            .fold[Set[java.lang.Throwable] \/ Unit](
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
  : Set[java.lang.Throwable] \/ Unit
  = ontChanges.foldLeft[Set[java.lang.Throwable] \/ Unit](\/-(())) { case (acc, ontChange) =>
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

  def createOMFGraphStore()
  : Set[java.lang.Throwable] \/ OWLAPIOMFGraphStore
  = for {
    module <- OWLAPIOMFModule.owlAPIOMFModule(new CatalogManager())
    store = OWLAPIOMFGraphStore(module, OWLManager.createOWLOntologyManager())
  } yield store

  def loadCatalog(s: OWLAPIOMFGraphStore, cls: java.lang.Class[_], catalogPath: String)
  : Set[java.lang.Throwable] \/ Unit
  = Option.apply(cls.getResource(catalogPath))
    .fold[Set[java.lang.Throwable] \/ Unit] {
    Set[java.lang.Throwable](new java.lang.IllegalArgumentException(
      s"Catalog '$catalogPath' not found on the classpath of ${cls.getName}")).left
  } { url =>
    s.catalogIRIMapper.parseCatalog(url.toURI)
  }

  def loadCatalog(s: OWLAPIOMFGraphStore, file: Path)
  : Set[java.lang.Throwable] \/ Unit
  = s.catalogIRIMapper.parseCatalog(file.toUri)

  def getAnnotationPropertyUUIDfromOWLAnnotation
  (a: OWLAnnotation)
  : UUID
  = generateUUID(a.getProperty.getIRI.getIRIString).toString

  def getAnnotationPropertyFromOWLAnnotation
  (a: OWLAnnotation)
  : AnnotationProperty
  = AnnotationProperty(
    getAnnotationPropertyUUIDfromOWLAnnotation(a),
    a.getProperty.getIRI.getIRIString,
    a.getProperty.getIRI.getShortForm)

  def getRelevantOntologyAnnotations
  (ont: OWLOntology)
  : Vector[OWLAnnotation]
  = ont
    .annotations()
    .toScala[Vector]
    .filterNot(_.getProperty.getIRI.getIRIString.startsWith("http://imce.jpl.nasa.gov/foundation"))

  def getRelevantSubjectAnnotationAssertions
  (ont: OWLOntology, iri: IRI)
  : Vector[OWLAnnotationAssertionAxiom]
  = ont
    .annotationAssertionAxioms(iri)
    .toScala[Vector]
    .filterNot(_.getProperty.getIRI.getIRIString.startsWith("http://imce.jpl.nasa.gov/foundation"))

}