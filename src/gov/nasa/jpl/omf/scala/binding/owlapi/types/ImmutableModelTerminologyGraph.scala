/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
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
package gov.nasa.jpl.omf.scala.binding.owlapi.types

import java.lang.IllegalArgumentException
import java.lang.System

import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import org.semanticweb.owlapi.reasoner.{NodeSet, OWLReasoner}

import scala.collection.immutable._
import scala.util.Try
import scala.{Boolean, Enumeration, None, Option, Some, StringContext, Tuple3, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scala.language.postfixOps

object ImmutableOperation extends Enumeration {
  type ImmutableOperation = Value
  val Save,
  AddEntityAspect,
  AddEntityConcept,
  AddEntityReifiedRelationship,
  AddScalarDataType,
  AddDataRelationshipFromEntityToScalar,
  AddDataRelationshipFromEntityToStructure,
  AddDataRelationshipFromStructureToScalar,
  AddDataRelationshipFromStructureToStructure,
  AddEntityConceptSubClassAxiom,
  AddEntityDefinitionAspectSubClassAxiom = Value
}

import gov.nasa.jpl.omf.scala.binding.owlapi.types.ImmutableOperation._

sealed abstract class ImmutableModelTerminologyGraphException(val message: String)
  extends IllegalArgumentException(message) {
  require(null != message)
}

case class ReadOnlyImmutableTerminologyGraphException(operation: ImmutableOperation)
  extends ImmutableModelTerminologyGraphException(
    s"Operation '$operation' is illegal on an read-only ImmutableTerminologyGraph") {
  require(null != operation)
}

case class ImmutableModelTerminologyGraph
(override val kind: TerminologyKind,
 override val ont: OWLOntology,
 override val extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance],
 override protected val aspects: Vector[ModelEntityAspect],
 override protected val concepts: Vector[ModelEntityConcept],
 override protected val reifiedRelationships: Vector[ModelEntityReifiedRelationship],
 override protected val unreifiedRelationships: Vector[ModelEntityUnreifiedRelationship],
 override protected val sc: Vector[ModelScalarDataType],
 override protected val st: Vector[ModelStructuredDataType],
 override protected val e2sc: Vector[ModelDataRelationshipFromEntityToScalar],
 override protected val e2st: Vector[ModelDataRelationshipFromEntityToStructure],
 override protected val s2sc: Vector[ModelDataRelationshipFromStructureToScalar],
 override protected val s2st: Vector[ModelDataRelationshipFromStructureToStructure],
 override protected val ax: Vector[ModelTermAxiom])
(override implicit val ops: OWLAPIOMFOps)
  extends ModelTerminologyGraph(kind, ont, extraProvenanceMetadata)(ops) {

  require(null != kind)
  require(null != ont)
  require(null != aspects)
  require(null != concepts)
  require(null != reifiedRelationships)
  require(null != unreifiedRelationships)
  require(null != sc)
  require(null != st)
  require(null != e2sc)
  require(null != e2st)
  require(null != s2sc)
  require(null != s2st)
  require(null != ax)

  override val mutabilityKind: String = "immutable"
  override val isImmutableModelTerminologyGraph = true
  override val isMutableModelTerminologyGraph = false

  override val kindIRI: IRI = makeKindIRI("immutable")

  val getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition] =
    ((aspects map (a => a.e -> a)) ++
      (concepts map (c => c.e -> c)) ++
      (reifiedRelationships map (r => r.e -> r))) toMap

  val getScalarDatatypeDefinitionMap: Map[OWLDatatype, ModelScalarDataType] =
    sc map (t => t.sc -> t) toMap

  override protected val iri2typeTerm = {
    def term2pair[T <: ModelTypeTerm](t: T) = t.iri -> t

    (aspects map term2pair) ++
      (concepts map term2pair) ++
      (reifiedRelationships map term2pair) ++
      (sc map term2pair) ++
      (st map term2pair) ++
      (e2sc map term2pair) ++
      (e2st map term2pair) ++
      (s2sc map term2pair) ++
      (s2st map term2pair) toMap
  }

}
