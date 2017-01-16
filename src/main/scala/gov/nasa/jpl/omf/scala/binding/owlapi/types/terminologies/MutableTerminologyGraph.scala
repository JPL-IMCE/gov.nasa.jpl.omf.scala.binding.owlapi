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

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.duplicateTerminologyGraphAxiomException
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms.TerminologyNestingAxiom
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.Concept
import gov.nasa.jpl.omf.scala.core.TerminologyKind
import org.semanticweb.owlapi.model._

import scala.collection.immutable._
import scala.{Any, Boolean, Int, Option, Some}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._, Scalaz._

object MutableTerminologyGraph {

  def initialize
  (iri: IRI,
   uuid: UUID,
   name: LocalName,
   kind: TerminologyKind,
   ont: OWLOntology,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance],
   backbone: OMFBackbone)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ MutableTerminologyGraph
  = {
    val mg = MutableTerminologyGraph(uuid, name, kind, ont, extraProvenanceMetadata, backbone)(store.ops)
    // we cannot set the name & uuid annotations because the OWLAPI ontology object may be immutable.
//      for {
//        _ <- mg.setTerminologyGraphLocalName(Some(name))
//        _ <- mg.setTerminologyGraphUUID(uuid)
//      } yield mg
    \/-(mg)
  }
}

case class MutableTerminologyGraph private
(override val uuid: UUID,
 override val name: LocalName,
 override val kind: TerminologyKind,
 override val ont: OWLOntology,
 override val extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance],
 override val backbone: OMFBackbone)
(override implicit val ops: OWLAPIOMFOps)
  extends TerminologyGraph with MutableTerminologyBox {

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: MutableTerminologyGraph => true
    case _ => false
  }

  override val hashCode: Int = (uuid, name, kind, extraProvenanceMetadata, ont).##

  override def equals(other: Any): Boolean = other match {
    case that: MutableTerminologyGraph =>
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
        (this.e2sc == that.e2sc) &&
        (this.e2st == that.e2st) &&
        (this.s2sc == that.s2sc) &&
        (this.s2st == that.s2st) &&
        (this.ax == that.ax) &&
        (this.gx == that.gx)
    case _ =>
      false
  }

  override val mutabilityKind: String = "mutableGraph"
  override val isImmutable = false
  override val isMutable = true

  override val kindIRI: IRI = makeKindIRI(mutabilityKind)

  def createTerminologyNestingAxiom
  (parentG: TerminologyBox,
   parentC: Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ TerminologyNestingAxiom
  = for {
    uuid <- ops.terminologyNestingAxiomUUID(parentC, this)
    ax <- createTerminologyNestingAxiom(uuid, parentG, parentC)
  } yield ax

  def createTerminologyNestingAxiom
  (uuid: UUID,
   parentG: TerminologyBox,
   parentC: Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ TerminologyNestingAxiom
  = nAxiom
    .fold[Set[java.lang.Throwable] \/ TerminologyNestingAxiom] {
    for {
      axiom <- store
        .createOMFTerminologyGraphDirectNestingAxiom(uuid, parentG, parentC, this)
    } yield {
      gx += axiom
      nAxiom = Some(axiom)
      axiom
    }
  } { other =>
    Set(
      duplicateTerminologyGraphAxiomException(AxiomExceptionKind.TerminologyGraphDirectNestingAxiomException, other)
    ).left
  }

  def addNestedTerminologyGraph
  (uuid: UUID,
   parentGraph: TerminologyBox,
   parentContext: Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ TerminologyNestingAxiom
  = for {
    axiom <- createTerminologyNestingAxiom(uuid, parentGraph, parentContext)
    _ <- applyOntologyChangesOrNoOp(ontManager,
      Seq(
        new AddImport(ont, owlDataFactory
          .getOWLImportsDeclaration(parentGraph.iri)),
        new AddOntologyAnnotation(ont, owlDataFactory
          .getOWLAnnotation(
            store.ANNOTATION_HAS_CONTEXT,
            parentContext.iri,
            java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid))))
      ),
      "addNestedTerminologyGraph error")
  } yield axiom

}