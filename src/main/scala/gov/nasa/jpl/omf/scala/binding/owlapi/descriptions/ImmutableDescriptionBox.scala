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

package gov.nasa.jpl.omf.scala.binding.owlapi.descriptions

import gov.nasa.jpl.imce.oml.resolver.api

import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue}
import gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName
import gov.nasa.jpl.omf.scala.binding.owlapi.common.ImmutableModule
import gov.nasa.jpl.omf.scala.binding.owlapi.{OMFBackbone, OWLAPIOMF, OWLAPIOMFGraphStore, OWLAPIOMFOps}
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.{DescriptionBoxSignature, DescriptionKind, ImmutableDescriptionBoxSignature}
import org.semanticweb.owlapi.model.{IRI, OWLOntology}

import scala.collection.immutable._
import scala.{Any,Boolean,Int}
import scalaz._
import Scalaz._

object ImmutableDescriptionBox {

  def initialize
  (uuid: api.taggedTypes.DescriptionBoxUUID,
   name: LocalName,
   iri: IRI,
   kind: DescriptionKind,
   ont: OWLOntology,
   backbone: OMFBackbone)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ImmutableDescriptionBox
  = initialize[Set](
    DescriptionBoxSignature[OWLAPIOMF, Set](
      uuid, name, iri, kind,
      descriptionBoxRefinements =
        Set.empty[DescriptionBoxRefinement],
      closedWorldDefinitions =
        Set.empty[DescriptionBoxExtendsClosedWorldDefinitions],
      conceptInstances =
        Set.empty[ConceptInstance],
      reifiedRelationshipInstances =
        Set.empty[ReifiedRelationshipInstance],
      reifiedRelationshipInstanceDomains =
        Set.empty[ReifiedRelationshipInstanceDomain],
      reifiedRelationshipInstanceRanges =
        Set.empty[ReifiedRelationshipInstanceRange],
      unreifiedRelationshipInstanceTuples =
        Set.empty[UnreifiedRelationshipInstanceTuple],
      instanceRelationshipEnumerationRestrictions =
        Set.empty[InstanceRelationshipEnumerationRestriction],
      instanceRelationshipValueRestrictions =
        Set.empty[InstanceRelationshipValueRestriction],
      instanceRelationshipExistentialRangeRestrictions =
        Set.empty[InstanceRelationshipExistentialRangeRestriction],
      instanceRelationshipUniversalRangeRestrictions =
        Set.empty[InstanceRelationshipUniversalRangeRestriction],
      singletonScalarDataPropertyValues =
        Set.empty[SingletonInstanceScalarDataPropertyValue],
      singletonStructuredDataPropertyValues =
        Set.empty[SingletonInstanceStructuredDataPropertyValue],
      scalarDataPropertyValues =
        Set.empty[ScalarDataPropertyValue],
      structuredDataPropertyTuples =
        Set.empty[StructuredDataPropertyTuple],
      annotationProperties =
        Set.empty[AnnotationProperty],
      annotationPropertyValues =
        Set.empty[AnnotationPropertyValue]),
    ont, backbone)


  def initialize[S[A] <: scala.collection.Iterable[A]]
  ( s: DescriptionBoxSignature[OWLAPIOMF, S],
    ont: OWLOntology,
   backbone: OMFBackbone)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ImmutableDescriptionBox
  = ImmutableDescriptionBox(
    DescriptionBoxSignature[OWLAPIOMF, Set](
      s.uuid, s.name, s.iri, s.kind,
      descriptionBoxRefinements =
        Set.empty[DescriptionBoxRefinement] ++ s.descriptionBoxRefinements,
      closedWorldDefinitions =
        Set.empty[DescriptionBoxExtendsClosedWorldDefinitions] ++ s.closedWorldDefinitions,
      conceptInstances =
        Set.empty[ConceptInstance] ++ s.conceptInstances,
      reifiedRelationshipInstances =
        Set.empty[ReifiedRelationshipInstance] ++ s.reifiedRelationshipInstances,
      reifiedRelationshipInstanceDomains =
        Set.empty[ReifiedRelationshipInstanceDomain] ++ s.reifiedRelationshipInstanceDomains,
      reifiedRelationshipInstanceRanges =
        Set.empty[ReifiedRelationshipInstanceRange] ++ s.reifiedRelationshipInstanceRanges,
      unreifiedRelationshipInstanceTuples =
        Set.empty[UnreifiedRelationshipInstanceTuple] ++ s.unreifiedRelationshipInstanceTuples,
      instanceRelationshipEnumerationRestrictions =
        Set.empty[InstanceRelationshipEnumerationRestriction] ++ s.instanceRelationshipEnumerationRestrictions,
      instanceRelationshipValueRestrictions =
        Set.empty[InstanceRelationshipValueRestriction] ++ s.instanceRelationshipValueRestrictions,
      instanceRelationshipExistentialRangeRestrictions =
        Set.empty[InstanceRelationshipExistentialRangeRestriction] ++ s.instanceRelationshipExistentialRangeRestrictions,
      instanceRelationshipUniversalRangeRestrictions =
        Set.empty[InstanceRelationshipUniversalRangeRestriction] ++ s.instanceRelationshipUniversalRangeRestrictions,
      singletonScalarDataPropertyValues =
        Set.empty[SingletonInstanceScalarDataPropertyValue] ++ s.singletonScalarDataPropertyValues,
      singletonStructuredDataPropertyValues =
        Set.empty[SingletonInstanceStructuredDataPropertyValue] ++ s.singletonStructuredDataPropertyValues,
      scalarDataPropertyValues =
        Set.empty[ScalarDataPropertyValue] ++ s.scalarDataPropertyValues,
      structuredDataPropertyTuples =
        Set.empty[StructuredDataPropertyTuple] ++ s.structuredDataPropertyTuples,
      annotationProperties =
        Set.empty[AnnotationProperty] ++ s.annotationProperties,
      annotationPropertyValues =
        Set.empty[AnnotationPropertyValue] ++
          s.annotationPropertyValues),
    ont, backbone)(store.ops).right
}

case class ImmutableDescriptionBox
(override val sig: ImmutableDescriptionBoxSignature[OWLAPIOMF],
 override val ont: OWLOntology,
 override val backbone: OMFBackbone)
(implicit override val ops: OWLAPIOMFOps)
  extends DescriptionBox
    with ImmutableModule {

  override type MS = ImmutableDescriptionBoxSignature[OWLAPIOMF]

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ImmutableDescriptionBox => true
    case _ => false
  }

  override val hashCode: Int = (sig, ont).##

  override def equals(other: Any): Boolean = other match {
    case that: ImmutableDescriptionBox =>
      (that canEqual this) &&
        (this.sig.uuid == that.sig.uuid) &&
        (this.sig == that.sig) &&
        (this.ont == that.ont)
    case _ =>
      false
  }

}