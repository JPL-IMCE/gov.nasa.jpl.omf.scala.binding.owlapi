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

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.{AnnotationEntry, AnnotationProperty, LocalName}
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.core.TerminologyKind
import org.semanticweb.owlapi.model._

import scala.collection.immutable._
import scala.language.postfixOps
import scala.{Any, Boolean, Int, Option}
import scala.Predef.{Map => _, Set => _, _}
import scalaz.\/

case class ImmutableBundle private
(override val uuid: UUID,
 override val name: LocalName,
 override val kind: TerminologyKind,
 override val ont: OWLOntology,
 override val extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance],
 override val backbone: OMFBackbone,
 override protected val aspects: Vector[OWLAPIOMF#Aspect],
 override protected val concepts: Vector[OWLAPIOMF#Concept],
 override protected val reifiedRelationships: Vector[OWLAPIOMF#ReifiedRelationship],
 override protected val unreifiedRelationships: Vector[OWLAPIOMF#UnreifiedRelationship],
 override protected val sc: Vector[OWLAPIOMF#Scalar],
 override protected val st: Vector[OWLAPIOMF#Structure],

 override protected val scalarOneOfRestrictions: Vector[OWLAPIOMF#ScalarOneOfRestriction],
 override protected val binaryScalarRestrictions: Vector[OWLAPIOMF#BinaryScalarRestriction],
 override protected val iriScalarRestrictions: Vector[OWLAPIOMF#IRIScalarRestriction],
 override protected val numericScalarRestrictions: Vector[OWLAPIOMF#NumericScalarRestriction],
 override protected val plainLiteralScalarRestrictions: Vector[OWLAPIOMF#PlainLiteralScalarRestriction],
 override protected val stringScalarRestrictions: Vector[OWLAPIOMF#StringScalarRestriction],
 override protected val synonymScalarRestrictions: Vector[OWLAPIOMF#SynonymScalarRestriction],
 override protected val timeScalarRestrictions: Vector[OWLAPIOMF#TimeScalarRestriction],

 override protected val e2sc: Vector[OWLAPIOMF#EntityScalarDataProperty],
 override protected val e2st: Vector[OWLAPIOMF#EntityStructuredDataProperty],
 override protected val s2sc: Vector[OWLAPIOMF#ScalarDataProperty],
 override protected val s2st: Vector[OWLAPIOMF#StructuredDataProperty],
 override protected val ax: Vector[OWLAPIOMF#Axiom],
 override protected val gx: Vector[OWLAPIOMF#TerminologyBoxAxiom],

 override protected var cAxiom: Option[OWLAPIOMF#ConceptDesignationTerminologyAxiom],
 override protected val eAxioms: Set[OWLAPIOMF#TerminologyExtensionAxiom],
 override protected var nAxiom: Option[OWLAPIOMF#TerminologyNestingAxiom],
 override protected val bAxioms: Set[OWLAPIOMF#BundledTerminologyAxiom],
 override protected val rTAxioms: Set[OWLAPIOMF#RootConceptTaxonomyAxiom],
 override protected val aTAxioms: Set[OWLAPIOMF#AnonymousConceptTaxonomyAxiom],
 override protected val sTAxioms: Set[OWLAPIOMF#SpecificDisjointConceptAxiom],
 override protected val annotations: Map[AnnotationProperty, Seq[AnnotationEntry]])
(override implicit val ops: OWLAPIOMFOps)
extends Bundle with ImmutableTerminologyBox {

  require(null != kind)
  require(null != ont)
  require(null != aspects)
  require(null != concepts)
  require(null != reifiedRelationships)
  require(null != unreifiedRelationships)
  require(null != sc)
  require(null != st)
  require(null != scalarOneOfRestrictions)
  require(null != binaryScalarRestrictions)
  require(null != iriScalarRestrictions)
  require(null != numericScalarRestrictions)
  require(null != plainLiteralScalarRestrictions)
  require(null != stringScalarRestrictions)
  require(null != synonymScalarRestrictions)
  require(null != timeScalarRestrictions)
  require(null != e2sc)
  require(null != e2st)
  require(null != s2sc)
  require(null != s2st)
  require(null != ax)
  require(null != gx)
  require(null != cAxiom)
  require(null != eAxioms)
  require(null != nAxiom)
  require(null != bAxioms)
  require(null != rTAxioms)
  require(null != aTAxioms)
  require(null != sTAxioms)

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ImmutableBundle => true
    case _ => false
  }

  override val hashCode: Int = (uuid, name, extraProvenanceMetadata, ont).##

  override def equals(other: Any): Boolean = other match {
    case that: ImmutableBundle =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.kind == that.kind) &&
        (this.extraProvenanceMetadata == that.extraProvenanceMetadata) &&
        (this.ont == that.ont) &&
        (this.aspects == that.aspects) &&
        (this.concepts == that.concepts) &&
        (this.reifiedRelationships == that.reifiedRelationships) &&
        (this.unreifiedRelationships == that.unreifiedRelationships) &&
        (this.sc == that.sc) &&
        (this.st == that.st) &&
        (this.scalarOneOfRestrictions == that.scalarOneOfRestrictions) &&
        (this.binaryScalarRestrictions == that.binaryScalarRestrictions) &&
        (this.iriScalarRestrictions == that.iriScalarRestrictions) &&
        (this.numericScalarRestrictions == that.numericScalarRestrictions) &&
        (this.plainLiteralScalarRestrictions == that.plainLiteralScalarRestrictions) &&
        (this.stringScalarRestrictions == that.stringScalarRestrictions) &&
        (this.synonymScalarRestrictions == that.synonymScalarRestrictions) &&
        (this.timeScalarRestrictions == that.timeScalarRestrictions) &&
        (this.e2sc == that.e2sc) &&
        (this.e2st == that.e2st) &&
        (this.s2sc == that.s2sc) &&
        (this.s2st == that.s2st) &&
        (this.ax == that.ax) &&
        (this.gx == that.gx) &&
        (this.cAxiom == that.cAxiom) &&
        (this.eAxioms == that.eAxioms) &&
        (this.nAxiom == that.nAxiom) &&
        (this.bAxioms == that.bAxioms) &&
        (this.rTAxioms == that.rTAxioms) &&
        (this.aTAxioms == that.aTAxioms) &&
        (this.sTAxioms == that.sTAxioms)
    case _ =>
      false
  }

  override val mutabilityKind: String = "immutableBundle"
  override val isImmutable = true
  override val isMutable = false

  override val kindIRI: IRI = makeKindIRI(mutabilityKind)

  val getEntityDefinitionMap: Map[OWLClass, Entity] =
    ((aspects map (a => a.e -> a)) ++
      (concepts map (c => c.e -> c)) ++
      (reifiedRelationships map (r => r.e -> r))) toMap

  val getScalarDatatypeDefinitionMap: Map[OWLDatatype, DataRange]
  = sc.map(t => t.e -> t).toMap ++
    scalarOneOfRestrictions.map(t => t.e -> t).toMap ++
    binaryScalarRestrictions.map(t => t.e -> t).toMap ++
    iriScalarRestrictions.map(t => t.e -> t).toMap ++
    numericScalarRestrictions.map(t => t.e -> t).toMap ++
    plainLiteralScalarRestrictions.map(t => t.e -> t).toMap ++
    stringScalarRestrictions.map(t => t.e -> t).toMap ++
    synonymScalarRestrictions.map(t => t.e -> t).toMap ++
    timeScalarRestrictions.map(t => t.e -> t).toMap

  override protected val iri2typeTerm = {
    def term2pair[T <: OWLAPIOMF#Term](t: T) = t.iri -> t

    (aspects map term2pair) ++
      (concepts map term2pair) ++
      (reifiedRelationships map term2pair) ++
      (unreifiedRelationships map term2pair) ++
      (sc map term2pair) ++
      (st map term2pair) ++
      (scalarOneOfRestrictions map term2pair) ++
      (binaryScalarRestrictions map term2pair) ++
      (iriScalarRestrictions map term2pair) ++
      (numericScalarRestrictions map term2pair) ++
      (plainLiteralScalarRestrictions map term2pair) ++
      (stringScalarRestrictions map term2pair) ++
      (synonymScalarRestrictions map term2pair) ++
      (timeScalarRestrictions map term2pair) ++
      (e2sc map term2pair) ++
      (e2st map term2pair) ++
      (s2sc map term2pair) ++
      (s2st map term2pair) toMap
  }
}


object ImmutableBundle {

  def initialize
  (iri: IRI,
   uuid: UUID,
   name: LocalName,
   kind: TerminologyKind,
   ont: OWLOntology,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance],
   backbone: OMFBackbone,
   aspects: Vector[OWLAPIOMF#Aspect],
   concepts: Vector[OWLAPIOMF#Concept],
   reifiedRelationships: Vector[OWLAPIOMF#ReifiedRelationship],
   unreifiedRelationships: Vector[OWLAPIOMF#UnreifiedRelationship],
   sc: Vector[OWLAPIOMF#Scalar],
   st: Vector[OWLAPIOMF#Structure],

   scalarOneOfRestrictions: Vector[OWLAPIOMF#ScalarOneOfRestriction],
   binaryScalarRestrictions: Vector[OWLAPIOMF#BinaryScalarRestriction],
   iriScalarRestrictions: Vector[OWLAPIOMF#IRIScalarRestriction],
   numericScalarRestrictions: Vector[OWLAPIOMF#NumericScalarRestriction],
   plainLiteralScalarRestrictions: Vector[OWLAPIOMF#PlainLiteralScalarRestriction],
   stringScalarRestrictions: Vector[OWLAPIOMF#StringScalarRestriction],
   synonymScalarRestrictions: Vector[OWLAPIOMF#SynonymScalarRestriction],
   timeScalarRestrictions: Vector[OWLAPIOMF#TimeScalarRestriction],

   e2sc: Vector[OWLAPIOMF#EntityScalarDataProperty],
   e2st: Vector[OWLAPIOMF#EntityStructuredDataProperty],
   s2sc: Vector[OWLAPIOMF#ScalarDataProperty],
   s2st: Vector[OWLAPIOMF#StructuredDataProperty],
   ax: Vector[OWLAPIOMF#Axiom],
   gx: Vector[OWLAPIOMF#TerminologyBoxAxiom],

   cAxiom: Option[OWLAPIOMF#ConceptDesignationTerminologyAxiom],
   eAxioms: Set[OWLAPIOMF#TerminologyExtensionAxiom],
   nAxiom: Option[OWLAPIOMF#TerminologyNestingAxiom],
   bAxioms: Set[OWLAPIOMF#BundledTerminologyAxiom],
   rTAxioms: Set[OWLAPIOMF#RootConceptTaxonomyAxiom],
   aTAxioms: Set[OWLAPIOMF#AnonymousConceptTaxonomyAxiom],
   sTAxioms: Set[OWLAPIOMF#SpecificDisjointConceptAxiom],
   annotations: Map[AnnotationProperty, Seq[AnnotationEntry]])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ImmutableBundle
  = {
    import scalaz._

    val ib = ImmutableBundle(
      uuid, name, kind, ont, extraProvenanceMetadata, backbone,
      aspects, concepts, reifiedRelationships, unreifiedRelationships, sc, st,
      scalarOneOfRestrictions,
      binaryScalarRestrictions,
      iriScalarRestrictions,
      numericScalarRestrictions,
      plainLiteralScalarRestrictions,
      stringScalarRestrictions,
      synonymScalarRestrictions,
      timeScalarRestrictions,
      e2sc,
      e2st,
      s2sc,
      s2st,
      ax,
      gx,
      cAxiom,
      eAxioms,
      nAxiom,
      bAxioms,
      rTAxioms,
      aTAxioms,
      sTAxioms,
      annotations)(store.ops)

    // we cannot set the name & uuid annotations because the OWLAPI ontology object may be immutable.
    //      for {
    //        _ <- mg.setTerminologyGraphLocalName(Some(name))
    //        _ <- mg.setTerminologyGraphUUID(uuid)
    //      } yield mg
    \/-(ib)
  }
}