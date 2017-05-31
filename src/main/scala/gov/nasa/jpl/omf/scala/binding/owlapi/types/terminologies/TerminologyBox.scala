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

package gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies

import java.io.OutputStream
import java.util.UUID

import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.common.Module
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{Axiom, Term}
import gov.nasa.jpl.omf.scala.core.{OMFError, TerminologyBoxSignature, terminologyBoxImportClosure}
import gov.nasa.jpl.omf.scala.core.OMLString.LocalName
import org.semanticweb.owlapi.model._

import scala.collection.immutable.{Iterable, Map, Seq, Set}
import scala.compat.java8.StreamConverters._
import scala.util.control.Exception._
import scala.{Any, Boolean, Option, StringContext, Unit}
import scala.Predef.String
import scalaz.Scalaz._
import scalaz._

trait TerminologyBox extends Module {

  override type MS <: TerminologyBoxSignature[OWLAPIOMF, scala.collection.Iterable]

  override val sig: MS

  val extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance]

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: TerminologyBox => true
    case _ => false
  }

  implicit val ops: OWLAPIOMFOps
  import ops._

  val mutabilityKind: String
  val isMutable: Boolean
  val isImmutable: Boolean
  val isGraph: Boolean
  val isBundle: Boolean

  val rdfs_labelAP = owlDataFactory.getOWLAnnotationProperty(rdfs_label)
  val isAbstractAP = owlDataFactory.getOWLAnnotationProperty(AnnotationIsAbstract)
  val isDerivedAP = owlDataFactory.getOWLAnnotationProperty(AnnotationIsDerived)

  protected val iri2typeTerm: scala.collection.Map[IRI, Term]

  def createOMFProvenanceAnnotation
  (uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OWLAnnotation
  = owlDataFactory.getOWLAnnotation(
    store.ANNOTATION_HAS_UUID,
    owlDataFactory.getOWLLiteral(uuid.toString)
  )

  def isTypeTermDefined
  (t: Term)
  : Boolean
  = iri2typeTerm.values.exists(_ == t)

  def isTypeTermDefinedRecursively
  (t: Term)
  (implicit store: OWLAPIOMFGraphStore)
  : Boolean
  = {
    val closure = terminologyBoxImportClosure[OWLAPIOMF](this)
    val found = closure.exists(_.isTypeTermDefined(t))
    found
  }

  def lookupTerm
  (iri: IRI, recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[Term]
  = if (recursively)
    lookupTypeTermRecursively(iri)
  else
    iri2typeTerm.get(iri)

  def lookupTerm
  (iri: Option[IRI], recursively: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[Term]
  = for {
    _iri <- iri
    _t <- lookupTerm(_iri, recursively)
  } yield _t

  def lookupTypeTermRecursively
  (iri: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Option[Term]
  = terminologyBoxImportClosure[OWLAPIOMF](this)
    .flatMap(_.lookupTerm(iri, recursively = false)).headOption

  def lookupTypeTermRecursively
  (iri: Option[IRI])
  (implicit store: OWLAPIOMFGraphStore)
  : Option[Term]
  = for {
    _iri <- iri
    _t <- lookupTypeTermRecursively(_iri)
  } yield _t

  val kindIRI: IRI

  protected def makeKindIRI(kind: String)
  : IRI
  = iri.resolve(iri.getRemainder.orElse("") + "?kind=" + kind)

  def getEntityDefinitionMap: Map[OWLClass, Entity]

  def getDataRelationshipsFromEntityToScalar
  : Seq[EntityScalarDataProperty]
  = sig.entityScalarDataProperties.to[Seq]

  def getScalarDatatypeDefinitionMap: Map[OWLDatatype, DataRange]

  def getNestingAxiomIfAny
  : Option[TerminologyNestingAxiom]
  = sig.nesting.headOption

  def getTermAxioms: (IRI, Iterable[Axiom]) = (iri, sig.axioms.to[Iterable])

  def getTypeTerms: (IRI, Iterable[Term]) = (iri, iri2typeTerm.values.to[Iterable])

  def getTerminologyGraphShortNameAnnotation
  : Option[OWLAnnotation]
  = ont.annotations.toScala[Set].find(_.getProperty.getIRI == ops.rdfs_label)

  def getTerminologyGraphLocalName
  : LocalName
  = name

  def getTerminologyGraphUUIDAnnotation
  : Option[OWLAnnotation]
  = ont.annotations.toScala[Set].find(_.getProperty.getIRI == ops.AnnotationHasUUID)


  def getTermLocalNameAnnotationAssertionAxiom
  (term: types.Term)
  : Option[OWLAnnotationAssertionAxiom]
  = findAnnotationAssertionAxiom(ont, term.iri, ops.rdfs_label)

  def getTermUUIDAnnotationAssertionAxiom
  (term: types.Term)
  : Option[OWLAnnotationAssertionAxiom]
  = findAnnotationAssertionAxiom(ont, term.iri, ops.AnnotationHasUUID)

  def getTermIDAnnotationAssertionAxiom
  (term: types.Term)
  : Option[OWLAnnotationAssertionAxiom]
  = ont.annotationAssertionAxioms(term.iri)
    .toScala[Set]
    .find(_.getProperty.getIRI == ops.AnnotationHasID)

  def getTermURLAnnotationAssertionAxiom
  (term: types.Term)
  : Option[OWLAnnotationAssertionAxiom]
  = ont.annotationAssertionAxioms(term.iri)
    .toScala[Set]
    .find(_.getProperty.getIRI == ops.AnnotationHasURL)

  def save(saveIRI: IRI): Set[java.lang.Throwable] \/ Unit
  = nonFatalCatch[Unit]
    .withApply {
      (cause: java.lang.Throwable) =>
        Set(
          OMFError.omfException(
            s"saving ModelTerminologyGraph failed: ${cause.getMessage}",
            cause)
        ).left
    }
    .apply({
      ontManager.saveOntology(ont, saveIRI).right
    })

  def save(os: OutputStream): Set[java.lang.Throwable] \/ Unit
  = nonFatalCatch[Unit]
    .withApply {
      (cause: java.lang.Throwable) =>
        Set(
          OMFError.omfException(
            s"saving ModelTerminologyGraph failed: ${cause.getMessage}",
            cause)
        ).left
    }
    .apply {
      ontManager.saveOntology(ont, os).right
    }
}
