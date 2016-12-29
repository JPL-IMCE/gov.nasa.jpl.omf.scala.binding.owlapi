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

import gov.nasa.jpl.imce.omf.schema.tables.{Annotation, AnnotationProperty, LocalName}
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{Axiom, Resource, Term, TerminologyThing}
import gov.nasa.jpl.omf.scala.core.TerminologyKind
import gov.nasa.jpl.omf.scala.core.{OMFError, terminologyImportClosure}
import org.semanticweb.owlapi.model._

import scala.collection.immutable.{Iterable, Map, Seq, Set}
import scala.compat.java8.StreamConverters._
import scala.util.control.Exception._
import scala.{Any, Boolean, None, Option, Some, StringContext, Unit}
import scala.Predef.String
import scalaz.Scalaz._
import scalaz._

trait TerminologyBox extends TerminologyThing with Resource {

  val kind: TerminologyKind
  val ont: OWLOntology
  val backbone: OMFBackbone
  val extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance]

  def fromTerminology
  : OWLAPITerminologySignature
  = OWLAPITerminologySignature(
    isBundle,
    uuid, name, iri, kind,
    imports(),
    nAxiom.map(_.nestingContext),
    aspects.to[Iterable],
    concepts.to[Iterable],
    reifiedRelationships.to[Iterable],
    unreifiedRelationships.to[Iterable],
    sc.to[Iterable],
    st.to[Iterable],
    scalarOneOfRestrictions.to[Iterable],
    binaryScalarRestrictions.to[Iterable],
    iriScalarRestrictions.to[Iterable],
    numericScalarRestrictions.to[Iterable],
    plainLiteralScalarRestrictions.to[Iterable],
    stringScalarRestrictions.to[Iterable],
    synonymScalarRestrictions.to[Iterable],
    timeScalarRestrictions.to[Iterable],
    e2sc.to[Iterable],
    e2st.to[Iterable],
    s2sc.to[Iterable],
    s2st.to[Iterable],
    ax.to[Iterable],
    gx.to[Iterable],
    bAxioms.to[Iterable],
    rTAxioms.to[Iterable],
    aTAxioms.to[Iterable],
    sTAxioms.to[Iterable],
    annotations.toMap)

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

  val ontManager = ont.getOWLOntologyManager
  val owlDataFactory = ontManager.getOWLDataFactory

  val rdfs_labelAP = owlDataFactory.getOWLAnnotationProperty(rdfs_label)
  val isAbstractAP = owlDataFactory.getOWLAnnotationProperty(AnnotationIsAbstract)
  val isDerivedAP = owlDataFactory.getOWLAnnotationProperty(AnnotationIsDerived)

  protected val aspects: scala.collection.Seq[OWLAPIOMF#Aspect]
  protected val concepts: scala.collection.Seq[OWLAPIOMF#Concept]
  protected val reifiedRelationships: scala.collection.Seq[OWLAPIOMF#ReifiedRelationship]
  protected val unreifiedRelationships: scala.collection.Seq[OWLAPIOMF#UnreifiedRelationship]
  protected val sc: scala.collection.Seq[OWLAPIOMF#Scalar]
  protected val st: scala.collection.Seq[OWLAPIOMF#Structure]

  protected val scalarOneOfRestrictions: scala.collection.Seq[OWLAPIOMF#ScalarOneOfRestriction]
  protected val binaryScalarRestrictions: scala.collection.Seq[OWLAPIOMF#BinaryScalarRestriction]
  protected val iriScalarRestrictions: scala.collection.Seq[OWLAPIOMF#IRIScalarRestriction]
  protected val numericScalarRestrictions: scala.collection.Seq[OWLAPIOMF#NumericScalarRestriction]
  protected val plainLiteralScalarRestrictions: scala.collection.Seq[OWLAPIOMF#PlainLiteralScalarRestriction]
  protected val stringScalarRestrictions: scala.collection.Seq[OWLAPIOMF#StringScalarRestriction]
  protected val synonymScalarRestrictions: scala.collection.Seq[OWLAPIOMF#SynonymScalarRestriction]
  protected val timeScalarRestrictions: scala.collection.Seq[OWLAPIOMF#TimeScalarRestriction]

  protected val e2sc: scala.collection.Seq[OWLAPIOMF#EntityScalarDataProperty]
  protected val e2st: scala.collection.Seq[OWLAPIOMF#EntityStructuredDataProperty]
  protected val s2sc: scala.collection.Seq[OWLAPIOMF#ScalarDataProperty]
  protected val s2st: scala.collection.Seq[OWLAPIOMF#StructuredDataProperty]
  protected val ax: scala.collection.Seq[OWLAPIOMF#Axiom]
  protected val gx: scala.collection.Seq[OWLAPIOMF#TerminologyBoxAxiom]

  protected var cAxiom: Option[OWLAPIOMF#ConceptDesignationTerminologyAxiom]
  protected val eAxioms: scala.collection.Set[OWLAPIOMF#TerminologyExtensionAxiom]
  protected var nAxiom: Option[OWLAPIOMF#TerminologyNestingAxiom]

  protected val bAxioms: scala.collection.Set[OWLAPIOMF#BundledTerminologyAxiom]
  protected val rTAxioms: scala.collection.Set[OWLAPIOMF#RootConceptTaxonomyAxiom]
  protected val aTAxioms: scala.collection.Set[OWLAPIOMF#AnonymousConceptTaxonomyAxiom]
  protected val sTAxioms: scala.collection.Set[OWLAPIOMF#SpecificDisjointConceptAxiom]

  protected val annotations: scala.collection.Map[AnnotationProperty, scala.collection.immutable.Seq[Annotation]]

  def getAnnotations
  ()
  : Map[AnnotationProperty, Seq[Annotation]]
  = annotations.toMap

  final def imports()
  : scala.collection.immutable.Iterable[TerminologyBox]
  = bAxioms.map(_.bundledTerminology).to[Set] ++
    cAxiom.map(_.designatedTerminology).to[Set] ++
    eAxioms.map(_.extendedTerminology).to[Set] ++
    nAxiom.map(_.nestingTerminology).to[Set]

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
    val closure = terminologyImportClosure[OWLAPIOMF, TerminologyBox](this, onlyCompatibleKind = true)
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
  = terminologyImportClosure[OWLAPIOMF, TerminologyBox](this, onlyCompatibleKind = true)
    .flatMap(_.lookupTerm(iri, recursively = false)).headOption

  def lookupTypeTermRecursively
  (iri: Option[IRI])
  (implicit store: OWLAPIOMFGraphStore)
  : Option[Term]
  = for {
    _iri <- iri
    _t <- lookupTypeTermRecursively(_iri)
  } yield _t

  val iri: IRI = ont.getOntologyID.getOntologyIRI.get

  val kindIRI: IRI

  protected def makeKindIRI(kind: String)
  : IRI
  = iri.resolve(iri.getRemainder.orElse("") + "?kind=" + kind)

  def getEntityDefinitionMap: Map[OWLClass, Entity]

  def getDataRelationshipsFromEntityToScalar
  : Seq[EntityScalarDataProperty]
  = e2sc.to[Seq]

  def getScalarDatatypeDefinitionMap: Map[OWLDatatype, DataRange]

  def getNestingAxiomIfAny
  : Option[TerminologyNestingAxiom]
  = gx
    .flatMap {
      case n: TerminologyNestingAxiom =>
        Some(n)
      case _ =>
        None
    }
    .headOption

  def getTermAxioms: (IRI, Iterable[Axiom]) = (iri, ax.to[Iterable])

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
