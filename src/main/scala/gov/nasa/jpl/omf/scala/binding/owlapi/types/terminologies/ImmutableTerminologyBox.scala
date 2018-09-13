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

import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMF
import gov.nasa.jpl.omf.scala.binding.owlapi.common.ImmutableModule
import gov.nasa.jpl.omf.scala.binding.owlapi.types.Term
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.core.ImmutableTerminologyBoxSignature
import org.semanticweb.owlapi.model.{OWLClass, OWLDatatype, OWLObjectProperty}

import scala.collection.immutable._
import scala.language.postfixOps
import scala.{Any, Boolean}
import scala.Predef.ArrowAssoc

trait ImmutableTerminologyBox
  extends TerminologyBox
    with ImmutableModule {

  override type MS = ImmutableTerminologyBoxSignature[OWLAPIOMF]

  override val sig: ImmutableTerminologyBoxSignature[OWLAPIOMF]

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ImmutableTerminologyBox => true
    case _ => false
  }

  val getEntityDefinitionMap
  : Map[OWLClass, Entity]
  = (sig.aspects.map(a => a.e -> a) ++
    sig.concepts.map(c => c.e -> c) ++
    sig.reifiedRelationships.map(r => r.e -> r)).toMap

  val getRestrictableRelationshipMap: Map[OWLObjectProperty, RestrictableRelationship] =
    sig.forwardProperties.map(p => p.e -> p).toMap ++
      sig.inverseProperties.map(p => p.e -> p).toMap ++
      sig.unreifiedRelationships.map(p => p.e -> p).toMap


  val getScalarDatatypeDefinitionMap: Map[OWLDatatype, DataRange]
  = sig.scalarDataTypes.map(t => t.e -> t).toMap ++
    sig.scalarOneOfRestrictions.map(t => t.e -> t).toMap ++
    sig.binaryScalarRestrictions.map(t => t.e -> t).toMap ++
    sig.iriScalarRestrictions.map(t => t.e -> t).toMap ++
    sig.numericScalarRestrictions.map(t => t.e -> t).toMap ++
    sig.plainLiteralScalarRestrictions.map(t => t.e -> t).toMap ++
    sig.stringScalarRestrictions.map(t => t.e -> t).toMap ++
    sig.synonymScalarRestrictions.map(t => t.e -> t).toMap ++
    sig.timeScalarRestrictions.map(t => t.e -> t).toMap

  override protected val iri2typeTerm = {
    def term2pair[T <: Term](t: T) = t.iri -> t

    (sig.aspects map term2pair) ++
      (sig.concepts map term2pair) ++
      (sig.reifiedRelationships map term2pair) ++
      (sig.unreifiedRelationships map term2pair) ++
      (sig.scalarDataTypes map term2pair) ++
      (sig.structuredDataTypes map term2pair) ++
      (sig.scalarOneOfRestrictions map term2pair) ++
      (sig.binaryScalarRestrictions map term2pair) ++
      (sig.iriScalarRestrictions map term2pair) ++
      (sig.numericScalarRestrictions map term2pair) ++
      (sig.plainLiteralScalarRestrictions map term2pair) ++
      (sig.stringScalarRestrictions map term2pair) ++
      (sig.synonymScalarRestrictions map term2pair) ++
      (sig.timeScalarRestrictions map term2pair) ++
      (sig.entityScalarDataProperties map term2pair) ++
      (sig.entityStructuredDataProperties map term2pair) ++
      (sig.scalarDataProperties map term2pair) ++
      (sig.structuredDataProperties map term2pair) toMap
  }

  override protected val reifiedRelation2forwardProperty
  : Map[ReifiedRelationship, ForwardProperty]
  = sig.reifiedRelationships.map { rr => rr -> rr.forwardProperty }.toMap

  override protected val reifiedRelation2inverseProperty
    : Map[ReifiedRelationship, InverseProperty]
  = sig.reifiedRelationships.flatMap { rr => rr.inverseProperty.map { inv => rr -> inv } }.toMap

}
