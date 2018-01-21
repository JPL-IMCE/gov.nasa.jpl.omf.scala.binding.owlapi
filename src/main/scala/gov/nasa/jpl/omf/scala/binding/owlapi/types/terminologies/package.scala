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

import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue}
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMF
import gov.nasa.jpl.omf.scala.binding.owlapi.types.bundleStatements.{AnonymousConceptTaxonomyAxiom, RootConceptTaxonomyAxiom, SpecificDisjointConceptAxiom}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms.ScalarOneOfLiteralAxiom
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms.{BundledTerminologyAxiom, ConceptDesignationTerminologyAxiom, TerminologyExtensionAxiom, TerminologyNestingAxiom}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.core.{ImmutableTerminologyBoxSignature, MutableTerminologyBoxSignature, TerminologyBoxSignature}

import scala.collection.immutable._

package object terminologies {

  def toImmutableTerminologyBoxSignature
  (sig: MutableTerminologyBoxSignature[OWLAPIOMF])
  : ImmutableTerminologyBoxSignature[OWLAPIOMF]
  = TerminologyBoxSignature[OWLAPIOMF, Set](
    sig.isBundle,
    sig.uuid, sig.name, sig.iri, sig.kind,
    extensions = Set.empty[TerminologyExtensionAxiom] ++ sig.extensions,
    nesting = Set.empty[TerminologyNestingAxiom] ++ sig.nesting,
    conceptDesignation = Set.empty[ConceptDesignationTerminologyAxiom] ++ sig.conceptDesignation,
    bundledTerminologies = Set.empty[BundledTerminologyAxiom] ++ sig.bundledTerminologies,
    aspects = Set.empty[Aspect] ++ sig.aspects,
    concepts = Set.empty[Concept] ++ sig.concepts,
    reifiedRelationships = Set.empty[ReifiedRelationship] ++ sig.reifiedRelationships,
    forwardProperties = Set.empty[ForwardProperty] ++ sig.forwardProperties,
    inverseProperties = Set.empty[InverseProperty] ++ sig.inverseProperties,
    unreifiedRelationships = Set.empty[UnreifiedRelationship] ++ sig.unreifiedRelationships,
    scalarDataTypes = Set.empty[Scalar] ++ sig.scalarDataTypes,
    structuredDataTypes = Set.empty[Structure] ++ sig.structuredDataTypes,
    scalarOneOfRestrictions = Set.empty[ScalarOneOfRestriction] ++ sig.scalarOneOfRestrictions,
    scalarOneOfLiterals = Set.empty[ScalarOneOfLiteralAxiom] ++ sig.scalarOneOfLiterals,
    binaryScalarRestrictions = Set.empty[BinaryScalarRestriction] ++ sig.binaryScalarRestrictions,
    iriScalarRestrictions = Set.empty[IRIScalarRestriction] ++ sig.iriScalarRestrictions,
    numericScalarRestrictions = Set.empty[NumericScalarRestriction] ++ sig.numericScalarRestrictions,
    plainLiteralScalarRestrictions = Set.empty[PlainLiteralScalarRestriction] ++ sig.plainLiteralScalarRestrictions,
    stringScalarRestrictions = Set.empty[StringScalarRestriction] ++ sig.stringScalarRestrictions,
    synonymScalarRestrictions = Set.empty[SynonymScalarRestriction] ++ sig.synonymScalarRestrictions,
    timeScalarRestrictions = Set.empty[TimeScalarRestriction] ++ sig.timeScalarRestrictions,
    entityScalarDataProperties = Set.empty[EntityScalarDataProperty] ++ sig.entityScalarDataProperties,
    entityStructuredDataProperties = Set.empty[EntityStructuredDataProperty] ++ sig.entityStructuredDataProperties,
    scalarDataProperties = Set.empty[ScalarDataProperty] ++ sig.scalarDataProperties,
    structuredDataProperties = Set.empty[StructuredDataProperty] ++ sig.structuredDataProperties,
    restrictionStructuredDataPropertyTuples = Set.empty[RestrictionStructuredDataPropertyTuple] ++ sig.restrictionStructuredDataPropertyTuples,
    restrictionScalarDataPropertyValues = Set.empty[RestrictionScalarDataPropertyValue] ++ sig.restrictionScalarDataPropertyValues,
    chainRules = Set.empty[ChainRule] ++ sig.chainRules,
    ruleBodySegments = Set.empty[RuleBodySegment] ++ sig.ruleBodySegments,
    segmentPredicates = Set.empty[SegmentPredicate] ++ sig.segmentPredicates,
    axioms = Set.empty[Axiom] ++ sig.axioms,
    rTAxioms = Set.empty[RootConceptTaxonomyAxiom] ++ sig.rTAxioms,
    aTAxioms = Set.empty[AnonymousConceptTaxonomyAxiom] ++ sig.aTAxioms,
    sTAxioms = Set.empty[SpecificDisjointConceptAxiom] ++ sig.sTAxioms,
    bAxioms = Set.empty[BundledTerminologyAxiom] ++ sig.bAxioms,
    annotationProperties =
      Set.empty[AnnotationProperty] ++ sig.annotationProperties,
    annotationPropertyValues =
      Set.empty[AnnotationPropertyValue] ++
        sig.annotationPropertyValues)

}
