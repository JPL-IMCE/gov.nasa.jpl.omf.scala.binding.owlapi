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

package gov.nasa.jpl.omf.scala.binding.owlapi

import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue}
import gov.nasa.jpl.omf.scala.core.{DescriptionBoxSignature, ImmutableDescriptionBoxSignature, MutableDescriptionBoxSignature}

import scala.collection.immutable.{Map, Set}

package object descriptions {

  type Mutable2ImmutableDescriptionMap
  = Map[MutableDescriptionBox, ImmutableDescriptionBox]

  def toImmutableDescriptionBoxSignature
  (sig: MutableDescriptionBoxSignature[OWLAPIOMF])
  : ImmutableDescriptionBoxSignature[OWLAPIOMF]
  = DescriptionBoxSignature[OWLAPIOMF, Set](
    sig.uuid, sig.name, sig.iri, sig.kind,
    descriptionBoxRefinements =
      Set.empty[DescriptionBoxRefinement] ++ sig.descriptionBoxRefinements,
    closedWorldDefinitions =
      Set.empty[DescriptionBoxExtendsClosedWorldDefinitions] ++ sig.closedWorldDefinitions,
    conceptInstances =
      Set.empty[ConceptInstance] ++ sig.conceptInstances,
    reifiedRelationshipInstances =
      Set.empty[ReifiedRelationshipInstance] ++ sig.reifiedRelationshipInstances,
    reifiedRelationshipInstanceDomains =
      Set.empty[ReifiedRelationshipInstanceDomain] ++ sig.reifiedRelationshipInstanceDomains,
    reifiedRelationshipInstanceRanges =
      Set.empty[ReifiedRelationshipInstanceRange] ++ sig.reifiedRelationshipInstanceRanges,
    unreifiedRelationshipInstanceTuples =
      Set.empty[UnreifiedRelationshipInstanceTuple] ++ sig.unreifiedRelationshipInstanceTuples,
    singletonScalarDataPropertyValues =
      Set.empty[SingletonInstanceScalarDataPropertyValue] ++ sig.singletonScalarDataPropertyValues,
    singletonStructuredDataPropertyValues =
      Set.empty[SingletonInstanceStructuredDataPropertyValue] ++ sig.singletonStructuredDataPropertyValues,
    scalarDataPropertyValues =
      Set.empty[ScalarDataPropertyValue] ++ sig.scalarDataPropertyValues,
    structuredDataPropertyTuples =
      Set.empty[StructuredDataPropertyTuple] ++ sig.structuredDataPropertyTuples,
    annotationProperties =
      Set.empty[AnnotationProperty] ++ sig.annotationProperties,
    annotations =
      Set.empty[AnnotationPropertyValue] ++
        sig.annotations)
}
