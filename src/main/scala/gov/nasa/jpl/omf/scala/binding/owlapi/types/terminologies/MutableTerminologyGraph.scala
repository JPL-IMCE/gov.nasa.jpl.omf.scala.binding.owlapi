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

import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue}
import gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.types._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.bundleStatements._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.{MutableTerminologyBoxSignature, TerminologyBoxSignature, TerminologyKind}
import org.semanticweb.owlapi.model._

import scala.collection.immutable._
import scala.collection.mutable.HashSet
import scala.{Any, Boolean, Int, None, Some}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._

object MutableTerminologyGraph {

  def initialize
  (uuid: api.taggedTypes.TerminologyGraphUUID,
   name: LocalName,
   iri: IRI,
   kind: TerminologyKind,
   ont: OWLOntology,
   backbone: OMFBackbone)
  (implicit store: OWLAPIOMFGraphStore, ops: OWLAPIOMFOps)
  : Throwables \/ MutableTerminologyGraph
  = MutableTerminologyGraph(
    sig = TerminologyBoxSignature[OWLAPIOMF, HashSet](
      isBundle = false,
      uuid, name, iri, kind,
      extensions = HashSet.empty[TerminologyExtensionAxiom],
      nesting = HashSet.empty[TerminologyNestingAxiom],
      conceptDesignation = HashSet.empty[ConceptDesignationTerminologyAxiom],
      bundledTerminologies = HashSet.empty[BundledTerminologyAxiom],
      aspects = HashSet.empty[Aspect],
      concepts = HashSet.empty[Concept],
      reifiedRelationships = HashSet.empty[ReifiedRelationship],
      forwardProperties = HashSet.empty[ForwardProperty],
      inverseProperties = HashSet.empty[InverseProperty],
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

case class MutableTerminologyGraph
(override val sig: MutableTerminologyBoxSignature[OWLAPIOMF],
 override val ont: OWLOntology,
 override val backbone: OMFBackbone)
(override implicit val ops: OWLAPIOMFOps)
  extends TerminologyGraph with MutableTerminologyBox {

  override type MS = MutableTerminologyBoxSignature[OWLAPIOMF]

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: MutableTerminologyGraph => true
    case _ => false
  }

  override val hashCode: Int = (sig, ont).##

  override def equals(other: Any): Boolean = other match {
    case that: MutableTerminologyGraph =>
      (that canEqual this) &&
        (this.sig == that.sig) &&
        (this.ont == that.ont)
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
    uuid <- ops.terminologyNestingAxiomUUID(parentG, parentC, this)
    ax <- createTerminologyNestingAxiom(uuid, parentG, parentC)
  } yield ax

  def createTerminologyNestingAxiom
  (uuid: api.taggedTypes.TerminologyNestingAxiomUUID,
   parentG: TerminologyBox,
   parentC: Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ TerminologyNestingAxiom
  = sig.nesting.headOption match {
    case None =>
      for {
        axiom <- store
          .createOMFTerminologyGraphDirectNestingAxiom(uuid, parentG, parentC, this)
      } yield {
        sig.nesting += axiom
        axiom
      }
    case Some(other) =>
      Set(
        duplicateTerminologyGraphAxiomException(AxiomExceptionKind.TerminologyGraphDirectNestingAxiomException, other)
      ).left
  }

  def addNestedTerminologyGraph
  (uuid: api.taggedTypes.TerminologyNestingAxiomUUID,
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
            createOMLProvenanceAnnotations(uuid)))
      ),
      "addNestedTerminologyGraph error")
  } yield axiom

}