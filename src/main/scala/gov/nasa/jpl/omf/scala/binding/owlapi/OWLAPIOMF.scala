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

import gov.nasa.jpl.omf.scala.core._

trait OWLAPIOMF
  extends OMF
  with OWLAPIOMFstore
  with OWLAPIOMFiri
  with OWLAPIOMFtbox
  with OWLAPIOMFabox

trait OWLAPIOMFstore extends OMFstore {
  
  type Store = OWLAPIOMFGraphStore
  
}

trait OWLAPIOMFiri extends OMFiri {
  
  type IRI = org.semanticweb.owlapi.model.IRI
  
}

trait OWLAPIOMFtbox extends OMFtbox {

  override type Resource =
    types.Resource

  override type TerminologyThing =
    types.TerminologyThing

  override type TerminologyContext =
    types.TerminologyContext

  override type TerminologyStatement =
    types.TerminologyStatement

  override type TerminologyBoxStatement =
    types.TerminologyBoxStatement

  override type TerminologyBundleStatement =
    types.TerminologyBundleStatement

  override type TerminologyBox =
    types.terminologies.TerminologyBox

  override type Bundle =
    types.terminologies.Bundle

  override type TerminologyGraph =
    types.terminologies.TerminologyGraph

  override type ImmutableTerminologyBox =
    types.terminologies.ImmutableTerminologyBox

  override type ImmutableTerminologyGraph =
    types.terminologies.ImmutableTerminologyGraph

  override type ImmutableBundle =
    types.terminologies.ImmutableBundle

  override type MutableTerminologyBox =
    types.terminologies.MutableTerminologyBox

  override type MutableTerminologyGraph =
    types.terminologies.MutableTerminologyGraph

  override type MutableBundle =
    types.terminologies.MutableBundle

  override type Mutable2ImmutableTerminologyMap =
    types.Mutable2ImmutableTerminologyMap

  override type Term =
    types.Term

  override type Entity =
    types.terms.Entity

  override type Aspect =
    types.terms.Aspect

  override type Concept =
    types.terms.Concept

  override type EntityRelationship =
    types.terms.EntityRelationship

  override type UnreifiedRelationship =
    types.terms.UnreifiedRelationship

  override type ReifiedRelationship =
    types.terms.ReifiedRelationship

  override type Datatype =
    types.terms.Datatype

  override type Structure =
    types.terms.Structure

  override type DataRange =
    types.terms.DataRange

  override type Scalar =
    types.terms.Scalar

  override type RestrictedDataRange =
    types.terms.RestrictedDataRange

  override type BinaryScalarRestriction =
    types.terms.BinaryScalarRestriction

  override type IRIScalarRestriction =
    types.terms.IRIScalarRestriction

  override type NumericScalarRestriction =
    types.terms.NumericScalarRestriction

  override type PlainLiteralScalarRestriction =
    types.terms.PlainLiteralScalarRestriction

  override type ScalarOneOfRestriction =
    types.terms.ScalarOneOfRestriction

  override type StringScalarRestriction =
    types.terms.StringScalarRestriction

  override type SynonymScalarRestriction =
    types.terms.SynonymScalarRestriction

  override type TimeScalarRestriction =
    types.terms.TimeScalarRestriction

  override type DataRelationship =
    types.terms.DataRelationship

  override type DataRelationshipDomain =
    types.terms.DataRelationshipDomain

  override type DataRelationshipFromEntity =
    types.terms.DataRelationshipFromEntity

  override type DataRelationshipFromStructure =
    types.terms.DataRelationshipFromStructure

  override type DataRelationshipRange =
    types.terms.DataRelationshipRange

  override type DataRelationshipToScalar =
    types.terms.DataRelationshipToScalar

  override type DataRelationshipToStructure =
    types.terms.DataRelationshipToStructure

  override type EntityScalarDataProperty =
    types.terms.EntityScalarDataProperty

  override type EntityStructuredDataProperty =
    types.terms.EntityStructuredDataProperty

  override type ScalarDataProperty =
    types.terms.ScalarDataProperty

  override type StructuredDataProperty =
    types.terms.StructuredDataProperty

  override type Axiom =
    types.Axiom

  override type ScalarOneOfLiteralAxiom =
    types.termAxioms.ScalarOneOfLiteralAxiom

  override type TermAxiom =
    types.termAxioms.TermAxiom

  override type EntityRestrictionAxiom =
    types.termAxioms.EntityRestrictionAxiom

  override type EntityExistentialRestrictionAxiom =
    types.termAxioms.EntityExistentialRestrictionAxiom

  override type EntityUniversalRestrictionAxiom =
    types.termAxioms.EntityUniversalRestrictionAxiom

  override type EntityScalarDataPropertyRestrictionAxiom =
    types.termAxioms.EntityScalarDataPropertyRestrictionAxiom

  override type EntityScalarDataPropertyExistentialRestrictionAxiom =
    types.termAxioms.EntityScalarDataPropertyExistentialRestrictionAxiom

  override type EntityScalarDataPropertyParticularRestrictionAxiom =
    types.termAxioms.EntityScalarDataPropertyParticularRestrictionAxiom

  override type EntityScalarDataPropertyUniversalRestrictionAxiom =
    types.termAxioms.EntityScalarDataPropertyUniversalRestrictionAxiom

  override type SpecializationAxiom =
    types.termAxioms.SpecializationAxiom

  override type AspectSpecializationAxiom =
    types.termAxioms.AspectSpecializationAxiom

  override type ConceptSpecializationAxiom =
    types.termAxioms.ConceptSpecializationAxiom

  override type ReifiedRelationshipSpecializationAxiom =
    types.termAxioms.ReifiedRelationshipSpecializationAxiom

  override type TerminologyAxiom =
    types.terminologyAxioms.TerminologyAxiom

  override type TerminologyBoxAxiom =
    types.terminologyAxioms.TerminologyBoxAxiom

  override type TerminologyBundleAxiom =
    types.terminologyAxioms.TerminologyBundleAxiom

  override type BundledTerminologyAxiom =
    types.terminologyAxioms.BundledTerminologyAxiom

  override type ConceptDesignationTerminologyAxiom =
    types.terminologyAxioms.ConceptDesignationTerminologyAxiom

  override type TerminologyExtensionAxiom =
    types.terminologyAxioms.TerminologyExtensionAxiom

  override type TerminologyNestingAxiom =
    types.terminologyAxioms.TerminologyNestingAxiom

  override type ConceptTreeDisjunction =
    types.bundleStatements.ConceptTreeDisjunction

  override type DisjointUnionOfConceptsAxiom =
    types.bundleStatements.DisjointUnionOfConceptsAxiom

  override type AnonymousConceptTaxonomyAxiom =
    types.bundleStatements.AnonymousConceptTaxonomyAxiom

  override type RootConceptTaxonomyAxiom =
    types.bundleStatements.RootConceptTaxonomyAxiom

  override type SpecificDisjointConceptAxiom =
    types.bundleStatements.SpecificDisjointAxiom

}

trait OWLAPIOMFabox extends OMFabox {

  type ModelInstanceGraph =
  instances.ModelInstanceGraph

  type ImmutableModelInstanceGraph =
  instances.ImmutableModelInstanceGraph

  type MutableModelInstanceGraph =
  instances.MutableModelInstanceGraph

  type ModelInstanceAssertion =
  instances.ModelInstanceAssertion

  type ModelNamedIndividual =
  instances.ModelNamedIndividual

  type ModelEntityInstance =
  instances.ModelEntityInstance

  type ModelInstanceObject =
  instances.ModelInstanceObject

  type ModelInstanceRelation =
  instances.ModelInstanceRelation

  type ModelDataInstance =
  instances.ModelDataInstance

  type ModelInstanceDataLiteral =
  instances.ModelInstanceDataLiteral

  type ModelInstanceDataStructure =
  instances.ModelInstanceDataStructure

  type ModelInstanceDataRelationshipFromEntityToScalar =
  instances.ModelInstanceDataRelationshipFromEntityToScalar

  type ModelInstanceDataRelationshipFromEntityToStructure =
  instances.ModelInstanceDataRelationshipFromEntityToStructure

  type ModelInstanceDataRelationshipFromStructureToScalar =
  instances.ModelInstanceDataRelationshipFromStructureToScalar

  type ModelInstanceDataRelationshipFromStructureToStructure =
  instances.ModelInstanceDataRelationshipFromStructureToStructure

}

