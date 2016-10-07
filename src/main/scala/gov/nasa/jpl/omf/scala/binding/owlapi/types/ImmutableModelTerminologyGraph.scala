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

package gov.nasa.jpl.omf.scala.binding.owlapi.types

import java.lang.IllegalArgumentException
import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._

import scala.collection.immutable._
import scala.{Enumeration, Option, StringContext}
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
(override val uuid: UUID,
 override val name: LocalName,
 override val kind: TerminologyKind,
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
  extends ModelTerminologyGraph(uuid, name, kind, ont, extraProvenanceMetadata)(ops) {

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
