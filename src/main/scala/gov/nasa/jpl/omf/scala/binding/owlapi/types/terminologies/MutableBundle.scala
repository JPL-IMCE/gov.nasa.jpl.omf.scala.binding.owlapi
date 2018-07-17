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

import gov.nasa.jpl.imce.oml.resolver.api.taggedTypes.{AnonymousConceptUnionAxiomUUID,BundleUUID,BundledTerminologyAxiomUUID,RootConceptTaxonomyAxiomUUID,SpecificDisjointConceptAxiomUUID}
import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue}
import gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.bundleStatements.{AnonymousConceptTaxonomyAxiom, ConceptTreeDisjunction, RootConceptTaxonomyAxiom, SpecificDisjointConceptAxiom}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{Axiom, RestrictionScalarDataPropertyValue, RestrictionStructuredDataPropertyTuple, duplicateTerminologyGraphAxiomException}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms.ScalarOneOfLiteralAxiom
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms.{BundledTerminologyAxiom, ConceptDesignationTerminologyAxiom, TerminologyExtensionAxiom, TerminologyNestingAxiom}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.{MutableTerminologyBoxSignature, TerminologyBoxSignature, TerminologyKind}
import org.semanticweb.owlapi.model._

import scala.collection.immutable._
import scala.{Any, Boolean, Int}
import scala.Predef.{???, String}
import scalaz._
import Scalaz._
import scala.collection.mutable.HashSet

case class MutableBundle
(override val sig: MutableTerminologyBoxSignature[OWLAPIOMF],
 override val ont: OWLOntology,
 backbone: OMFBackbone)
(override implicit val ops: OWLAPIOMFOps)
  extends Bundle with MutableTerminologyBox {

  override type MS = MutableTerminologyBoxSignature[OWLAPIOMF]

  override val mutabilityKind: String = "mutableBundle"
  override val isImmutable = false
  override val isMutable = true

  override val kindIRI: IRI = makeKindIRI(mutabilityKind)

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: MutableBundle => true
    case _ => false
  }

  override val hashCode: Int = (sig, ont).##

  override def equals(other: Any): Boolean = other match {
    case that: MutableBundle =>
      (that canEqual this) &&
        (this.sig.uuid == that.sig.uuid) &&
        (this.sig == that.sig) &&
        (this.ont == that.ont)
    case _ =>
      false
  }

  def createBundledTerminologyAxiom
  (bundledTerminology: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ BundledTerminologyAxiom
  = for {
    uuid <- ops.bundledTerminologyAxiomUUID(this, bundledTerminology)
    ax <- createBundledTerminologyAxiom(uuid, bundledTerminology)
  } yield ax

  def createBundledTerminologyAxiom
  (uuid: BundledTerminologyAxiomUUID,
   bundledTerminology: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ BundledTerminologyAxiom
  = sig.bAxioms
    .find { _.targetModuleIRI == bundledTerminology.iri }
    .fold[Set[java.lang.Throwable] \/ BundledTerminologyAxiom](
    for {
      axiom <- store
        .createOMFBundledTerminologyAxiom(uuid, this, bundledTerminology)
    } yield {
      sig.bAxioms += axiom
      axiom
    }
  ) { other =>
    Set(
      duplicateTerminologyGraphAxiomException(AxiomExceptionKind.BundledTerminologyAxiomException, other)
    ).left
  }

  def addBundledTerminologyAxiom
  (uuid: BundledTerminologyAxiomUUID,
   extendedG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ BundledTerminologyAxiom
  = for {
    axiom <- createBundledTerminologyAxiom(uuid, extendedG)
    _ <- applyOntologyChangeOrNoOp(ontManager,
      new AddImport(ont, owlDataFactory
        .getOWLImportsDeclaration(extendedG.iri)),
      "addBundledTerminologyAxiom error")
  } yield axiom

  def addAnonymousConceptTaxonomyAxiom
  (uuid: AnonymousConceptUnionAxiomUUID, name: String, disjointTerminologyParent: ConceptTreeDisjunction)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#AnonymousConceptTaxonomyAxiom
  = ???

  def addRootConceptTaxonomyAxiom
  (uuid: RootConceptTaxonomyAxiomUUID, root: ConceptKind)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#RootConceptTaxonomyAxiom
  = ???

  def addSpecificDisjointConceptAxiom
  (uuid: SpecificDisjointConceptAxiomUUID, disjointTerminologyParent: ConceptTreeDisjunction, disjointLeaf: ConceptKind)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#SpecificDisjointConceptAxiom
  = ???

}


object MutableBundle {

  def initialize
  (uuid: BundleUUID,
   name: LocalName,
   iri: IRI,
   kind: TerminologyKind,
   ont: OWLOntology,
   backbone: OMFBackbone)
  (implicit store: OWLAPIOMFGraphStore, ops: OWLAPIOMFOps)
  : Throwables \/ MutableBundle
  = MutableBundle(
    sig = TerminologyBoxSignature[OWLAPIOMF, HashSet](
      isBundle = false,
      uuid, name, iri, kind,
      extensions = HashSet.empty[TerminologyExtensionAxiom],
      nesting = HashSet.empty[TerminologyNestingAxiom],
      conceptDesignation = HashSet.empty[ConceptDesignationTerminologyAxiom],
      bundledTerminologies = HashSet.empty[BundledTerminologyAxiom],
      aspects = HashSet.empty[Aspect],
      cardinalityRestrictedAspects = HashSet.empty[CardinalityRestrictedAspect],
      concepts = HashSet.empty[Concept],
      cardinalityRestrictedConcepts = HashSet.empty[CardinalityRestrictedConcept],
      reifiedRelationshipRestrictions = HashSet.empty[ReifiedRelationshipRestriction],
      reifiedRelationships = HashSet.empty[ReifiedRelationship],
      forwardProperties = HashSet.empty[ForwardProperty],
      inverseProperties = HashSet.empty[InverseProperty],
      cardinalityRestrictedReifiedRelationships = HashSet.empty[CardinalityRestrictedReifiedRelationship],
      unreifiedRelationships = HashSet.empty[UnreifiedRelationship],
      scalarDataTypes = HashSet.empty[Scalar],
      structuredDataTypes = HashSet.empty[Structure],
      scalarOneOfRestrictions = HashSet.empty[ScalarOneOfRestriction],
      scalarOneOfLiterals = HashSet.empty[ScalarOneOfLiteralAxiom],
      binaryScalarRestrictions = HashSet.empty[BinaryScalarRestriction],
      iriScalarRestrictions = HashSet.empty[IRIScalarRestriction],
      numericScalarRestrictions = HashSet.empty[NumericScalarRestriction],
      plainLiteralScalarRestrictions = HashSet.empty[PlainLiteralScalarRestriction],
      stringScalarRestrictions = HashSet.empty[StringScalarRestriction],
      synonymScalarRestrictions = HashSet.empty[SynonymScalarRestriction],
      timeScalarRestrictions = HashSet.empty[TimeScalarRestriction],
      entityScalarDataProperties = HashSet.empty[EntityScalarDataProperty],
      entityStructuredDataProperties = HashSet.empty[EntityStructuredDataProperty],
      scalarDataProperties = HashSet.empty[ScalarDataProperty],
      structuredDataProperties = HashSet.empty[StructuredDataProperty],
      restrictionStructuredDataPropertyTuples = HashSet.empty[RestrictionStructuredDataPropertyTuple],
      restrictionScalarDataPropertyValues = HashSet.empty[RestrictionScalarDataPropertyValue],
      chainRules = HashSet.empty[ChainRule],
      ruleBodySegments = HashSet.empty[RuleBodySegment],
      segmentPredicates = HashSet.empty[SegmentPredicate],
      axioms = HashSet.empty[Axiom],
      rTAxioms = HashSet.empty[RootConceptTaxonomyAxiom],
      aTAxioms = HashSet.empty[AnonymousConceptTaxonomyAxiom],
      sTAxioms = HashSet.empty[SpecificDisjointConceptAxiom],
      bAxioms = HashSet.empty[BundledTerminologyAxiom],
      annotationProperties = HashSet.empty[AnnotationProperty],
      annotationPropertyValues = HashSet.empty[AnnotationPropertyValue]),
    ont = ont,
    backbone = backbone)(ops).right[Throwables]

}