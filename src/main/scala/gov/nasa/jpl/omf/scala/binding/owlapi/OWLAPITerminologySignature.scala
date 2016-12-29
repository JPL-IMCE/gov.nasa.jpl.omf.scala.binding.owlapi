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

import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.{Annotation, AnnotationProperty, LocalName}
import gov.nasa.jpl.omf.scala.core.TerminologySignature
import gov.nasa.jpl.omf.scala.core.TerminologyKind

import scala.collection.immutable.{Iterable,Map,Seq}
import scala.{Boolean, Option}
import scala.Predef.require

case class OWLAPITerminologySignature
(override val isBundle: Boolean,
 override val uuid: UUID,
 override val name: LocalName,
 override val iri: OWLAPIOMF#IRI,
 override val kind: TerminologyKind,
 override val imports: Iterable[OWLAPIOMF#TerminologyBox],
 override val nesting: Option[OWLAPIOMF#Concept],
 override val aspects: Iterable[OWLAPIOMF#Aspect],
 override val concepts: Iterable[OWLAPIOMF#Concept],
 override val reifiedRelationships: Iterable[OWLAPIOMF#ReifiedRelationship],
 override val unreifiedRelationships: Iterable[OWLAPIOMF#UnreifiedRelationship],
 override val scalarDataTypes: Iterable[OWLAPIOMF#Scalar],
 override val structuredDataTypes: Iterable[OWLAPIOMF#Structure],
 override val scalarOneOfRestrictions: Iterable[OWLAPIOMF#ScalarOneOfRestriction],
 override val binaryScalarRestrictions: Iterable[OWLAPIOMF#BinaryScalarRestriction],
 override val iriScalarRestrictions: Iterable[OWLAPIOMF#IRIScalarRestriction],
 override val numericScalarRestrictions: Iterable[OWLAPIOMF#NumericScalarRestriction],
 override val plainLiteralScalarRestrictions: Iterable[OWLAPIOMF#PlainLiteralScalarRestriction],
 override val stringScalarRestrictions: Iterable[OWLAPIOMF#StringScalarRestriction],
 override val synonymScalarRestrictions: Iterable[OWLAPIOMF#SynonymScalarRestriction],
 override val timeScalarRestrictions: Iterable[OWLAPIOMF#TimeScalarRestriction],

 override val entityScalarDataProperties: Iterable[OWLAPIOMF#EntityScalarDataProperty],
 override val entityStructuredDataProperties: Iterable[OWLAPIOMF#EntityStructuredDataProperty],
 override val scalarDataProperties: Iterable[OWLAPIOMF#ScalarDataProperty],
 override val structuredDataProperties: Iterable[OWLAPIOMF#StructuredDataProperty],
 override val axioms: Iterable[OWLAPIOMF#Axiom],
 override val gaxioms: Iterable[OWLAPIOMF#TerminologyBoxAxiom],

 override val bAxioms: Iterable[OWLAPIOMF#BundledTerminologyAxiom],
 override val rTAxioms: Iterable[OWLAPIOMF#RootConceptTaxonomyAxiom],
 override val aTAxioms: Iterable[OWLAPIOMF#AnonymousConceptTaxonomyAxiom],
 override val sTAxioms: Iterable[OWLAPIOMF#SpecificDisjointConceptAxiom],

 override val annotations: Map[AnnotationProperty, Seq[Annotation]])
  extends TerminologySignature[OWLAPIOMF]
{
  require(null != iri)
  require(null != kind)
  require(null != imports)
  require(null != nesting)
  require(null != aspects)
  require(null != concepts)
  require(null != reifiedRelationships)
  require(null != unreifiedRelationships)
  require(null != scalarDataTypes)
  require(null != structuredDataTypes)
  require(null != scalarOneOfRestrictions)
  require(null != binaryScalarRestrictions)
  require(null != iriScalarRestrictions)
  require(null != plainLiteralScalarRestrictions)
  require(null != stringScalarRestrictions)
  require(null != timeScalarRestrictions)

  require(null != entityScalarDataProperties)
  require(null != entityStructuredDataProperties)
  require(null != scalarDataProperties)
  require(null != structuredDataProperties)
  require(null != axioms)
  require(null != gaxioms)
  require(null != bAxioms)
  require(null != rTAxioms)
  require(null != aTAxioms)
  require(null != sTAxioms)
  require(null != annotations)
}
