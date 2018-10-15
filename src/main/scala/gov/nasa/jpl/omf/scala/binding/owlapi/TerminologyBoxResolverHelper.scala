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

import java.lang.System

import gov.nasa.jpl.imce.oml.resolver.Filterable.filterable
import gov.nasa.jpl.imce.oml.tables.taggedTypes.localName
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.{RestrictableRelationship, _}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies._
import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner.{NodeSet, OWLReasoner}

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.{Boolean, None, Option, Some, StringContext}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.{CardinalityRestrictionKind, ExactCardinalityRestriction, MaxCardinalityRestriction, MinCardinalityRestriction}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms._
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables

object TerminologyBoxResolverHelper {

  def owlclassOfCE
  (ce: Option[OWLClassExpression])
  : Option[OWLClass]
  = ce match {
      case Some(subc: OWLClass) =>
        Some(subc)
      case _ =>
        None
    }

  def owlObjectPropertyOfPE
  (oe: Option[OWLObjectPropertyExpression])
  : Option[OWLObjectProperty]
  = oe match {
    case Some(op: OWLObjectProperty) =>
      Some(op)
    case _ =>
      None
  }

  def owlDataPropertyOfPE
  (de: Option[OWLDataPropertyExpression])
  : Option[OWLDataProperty]
  = de match {
    case Some(dp: OWLDataProperty) =>
      Some(dp)
    case _ =>
      None
  }

  type DOPInfo = (OWLDataProperty, OWLClass, OWLDatatype)

  def isDOPWarning(dop: DOPInfo)(implicit ops: OWLAPIOMFOps): Boolean
  = ops.isBackboneIRI(dop._1.getIRI) ||
    ops.isBackboneIRI(dop._2.getIRI) ||
    ops.isBackboneIRI(dop._3.getIRI)

  type ROPInfo = (IRI, OWLObjectProperty, OWLClass, OWLClass, Option[OWLObjectProperty])

  def isROPWarning(rop: ROPInfo)(implicit ops: OWLAPIOMFOps): Boolean
  = ops.isBackboneIRI(rop._1) ||
    ops.isBackboneIRI(rop._2.getIRI) ||
    ops.isBackboneIRI(rop._3.getIRI) ||
    ops.isBackboneIRI(rop._4.getIRI) ||
    rop._5.fold[Boolean](false) { inv => ops.isBackboneIRI(inv.getIRI) }

  type Chain = (OWLObjectProperty, OWLObjectProperty, OWLObjectProperty)
  type Chains = Set[Chain]

  def isChainWarning(chain: Chain)(implicit ops: OWLAPIOMFOps): Boolean
  = ops.isBackboneIRI(chain._1.getIRI) ||
    ops.isBackboneIRI(chain._2.getIRI) ||
    ops.isBackboneIRI(chain._3.getIRI)

  case class ResolvableUOPTuple
  (domain: Entity,
   range: Entity,
   tuple: (OWLClass, OWLObjectProperty, OWLClass),
   characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics])

  case class ResolvableROPTuple
  (rc: OWLClass,
   rop: ROPInfo,
   r_sourceDef: Entity,
   sourceRop: ROPInfo,
   r_targetDef: Entity,
   targetRop: ROPInfo,
   chain: Chain,
   characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics])

  def hasRelationshipCharacteristic
  (rc: RelationshipCharacteristics.RelationshipCharacteristics,
   required: Map[OWLObjectProperty, Boolean],
   optional: Option[(OWLObjectProperty, Boolean)] = None)
  : Option[RelationshipCharacteristics.RelationshipCharacteristics]
  = {
    if (required.values.forall(true == _)) {
      optional
        .fold[Option[RelationshipCharacteristics.RelationshipCharacteristics]](
          Some(rc)) {
          case (op, flag) =>
            if (!flag) {
              System.out.println(s"ERROR: inverse of $rc inconsistent for: $op")
            }
            Some(rc)
        }
    } else if (required.values.forall(false == _)) {
      optional
        .fold[Option[RelationshipCharacteristics.RelationshipCharacteristics]](
          None) {
          case (op, flag) =>
            if (flag) {
              System.out.println(s"ERROR: inverse of $rc inconsistent for: $op")
            }
            None
        }
    } else if (required.values.exists(true == _)) {
      required.foreach {
        case (_, true) =>
          ()
        case (r, false) =>
          System.out.println(s"WARNING: $rc missing for $r")
      }
      optional
        .fold[Option[RelationshipCharacteristics.RelationshipCharacteristics]](
          Some(rc)) {
          case (op, flag) =>
            if (!flag) {
              System.out.println(s"ERROR: inverse of $rc inconsistent for: $op")
            }
            Some(rc)
        }
    } else {
      optional
        .fold[Option[RelationshipCharacteristics.RelationshipCharacteristics]](
          None) {
          case (op, flag) =>
            if (flag) {
              System.out.println(s"ERROR: inverse of $rc inconsistent for: $op")
            }
            None
        }
    }
  }

  def asAspectKind(e: Entity): Option[AspectKind] = e match {
    case a: AspectKind =>
      Some(a)
    case _ =>
      None
  }

  def asConceptKind(e: Entity): Option[ConceptKind] = e match {
    case c: ConceptKind =>
      Some(c)
    case _ =>
      None
  }

  def asConceptualRelationship(e: Entity): Option[ConceptualRelationship] = e match {
    case cr: ConceptualRelationship =>
      Some(cr)
    case _ =>
      None
  }

  case class ResolvableCardinalityRestriction[D <: Entity](
      sub: OWLClass,
      ax: OWLSubClassOfAxiom,
      domain: D,
      restrictionKind: CardinalityRestrictionKind,
      restrictedRelationship: RestrictableRelationship,
      restrictedRange: Option[Entity],
      restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)

  case class ResolvableAspectSpecialization(c: OWLClass,
                                            ax: OWLSubClassOfAxiom,
                                            sub: Entity,
                                            sup: AspectKind)

  case class ResolvableConceptSpecialization(c: OWLClass,
                                             ax: OWLSubClassOfAxiom,
                                             sub: ConceptKind,
                                             sup: ConceptKind)

  case class ResolvableReifiedRelationshipRestrictionSpecialization
  (iri: IRI,
   c: OWLClass,
   source: Entity,
   target: Entity,
   specializations: Iterable[(ConceptualRelationship, Set[OWLSubClassOfAxiom])])

  sealed trait ResolvableConceptualRelationshipSpecialization {
    val c: OWLClass
    /**
      * To be removed from IncrementalResolverState.subClassAxioms
      */
    val axs: Set[OWLSubClassOfAxiom]
    /**
      * To be removed from IncrementalResolverState.subObjectPropertyAxioms
      */
    val oxs: Set[(OWLObjectProperty, OWLObjectProperty)]
    val sub: ConceptualRelationship
    val sup: ConceptualRelationship
  }

  case class Resolvable_RR_RR_Specialization
  (override val c: OWLClass,
   override val axs: Set[OWLSubClassOfAxiom],
   override val oxs: Set[(OWLObjectProperty, OWLObjectProperty)],
   override val sub: ReifiedRelationship,
   override val sup: ReifiedRelationship)
  extends ResolvableConceptualRelationshipSpecialization

  object Resolvable_RR_RR_Specialization {
    def make
    (rr_sub: ReifiedRelationship,
     rr_sup: ReifiedRelationship,
     ax: OWLSubClassOfAxiom)
    (op_rSources: (OWLObjectProperty, OWLObjectProperty),
     op_rTargets: (OWLObjectProperty, OWLObjectProperty),
     op_rUs: (OWLObjectProperty, OWLObjectProperty),
     op_rIs: Option[(OWLObjectProperty, OWLObjectProperty)])
    : Resolvable_RR_RR_Specialization
    = {
      val op_axs = Set(op_rSources, op_rTargets, op_rUs) ++ op_rIs
      Resolvable_RR_RR_Specialization(rr_sub.e, Set(ax), op_axs, rr_sub, rr_sup)
    }
  }

  case class Resolvable_RR_RRR_Specialization
  (override val c: OWLClass,
   override val axs: Set[OWLSubClassOfAxiom],
   override val oxs: Set[(OWLObjectProperty, OWLObjectProperty)],
   override val sub: ReifiedRelationship,
   override val sup: ReifiedRelationshipRestriction)
    extends ResolvableConceptualRelationshipSpecialization

  object Resolvable_RR_RRR_Specialization {
    def make
    (rr_sub: ReifiedRelationship,
     rr_sup: ReifiedRelationshipRestriction,
     ax: OWLSubClassOfAxiom)
    (op_rSources: Set[(OWLObjectProperty, OWLObjectProperty)],
     op_rTargets: Set[(OWLObjectProperty, OWLObjectProperty)],
     op_rUs: Set[(OWLObjectProperty, OWLObjectProperty)],
     op_rIs: Set[(OWLObjectProperty, OWLObjectProperty)])
    : Resolvable_RR_RRR_Specialization
    = {
      val op_axs = op_rSources ++ op_rTargets ++ op_rUs ++ op_rIs
      Resolvable_RR_RRR_Specialization(rr_sub.e, Set(ax), op_axs, rr_sub, rr_sup)
    }
  }

  case class Resolvable_RR_CRRR_Specialization
  (override val c: OWLClass,
   override val axs: Set[OWLSubClassOfAxiom],
   override val oxs: Set[(OWLObjectProperty, OWLObjectProperty)],
   override val sub: ReifiedRelationship,
   override val sup: CardinalityRestrictedReifiedRelationship)
    extends ResolvableConceptualRelationshipSpecialization

  object Resolvable_RR_CRRR_Specialization {
    def make
    (rr_sub: ReifiedRelationship,
     rr_sup: CardinalityRestrictedReifiedRelationship,
     ax: OWLSubClassOfAxiom)
    (op_rSources: Set[(OWLObjectProperty, OWLObjectProperty)],
     op_rTargets: Set[(OWLObjectProperty, OWLObjectProperty)],
     op_rUs: Set[(OWLObjectProperty, OWLObjectProperty)],
     op_rIs: Set[(OWLObjectProperty, OWLObjectProperty)])
    : Resolvable_RR_CRRR_Specialization
    = {
      val op_axs = op_rSources ++ op_rTargets ++ op_rUs ++ op_rIs
      Resolvable_RR_CRRR_Specialization(rr_sub.e, Set(ax), op_axs, rr_sub, rr_sup)
    }
  }

  case class Resolvable_RRR_RR_Specialization
  (override val c: OWLClass,
   override val axs: Set[OWLSubClassOfAxiom],
   override val oxs: Set[(OWLObjectProperty, OWLObjectProperty)],
   override val sub: ReifiedRelationshipRestriction,
   override val sup: ReifiedRelationship)
    extends ResolvableConceptualRelationshipSpecialization

  object Resolvable_RRR_RR_Specialization {

    def make
    (rr_sub: ReifiedRelationshipRestriction,
     rr_sup: ReifiedRelationship,
     ax: OWLSubClassOfAxiom)
    (s_ax: OWLSubClassOfAxiom,
     t_ax: OWLSubClassOfAxiom)
    : Resolvable_RRR_RR_Specialization
    = {
      val op_axs = Set(ax, s_ax, t_ax)
      Resolvable_RRR_RR_Specialization(rr_sub.e, op_axs, Set.empty, rr_sub, rr_sup)
    }
  }

  case class Resolvable_RRR_RRR_Specialization
  (override val c: OWLClass,
   override val axs: Set[OWLSubClassOfAxiom],
   override val oxs: Set[(OWLObjectProperty, OWLObjectProperty)],
   override val sub: ReifiedRelationshipRestriction,
   override val sup: ReifiedRelationshipRestriction)
    extends ResolvableConceptualRelationshipSpecialization

  object Resolvable_RRR_RRR_Specialization {

    def make
    (rr_sub: ReifiedRelationshipRestriction,
     rr_sup: ReifiedRelationshipRestriction,
     ax: OWLSubClassOfAxiom)
    (s_ax: Set[OWLSubClassOfAxiom],
     t_ax: Set[OWLSubClassOfAxiom])
    : Resolvable_RRR_RRR_Specialization
    = {
      val op_axs = Set(ax) ++ s_ax ++ t_ax
      Resolvable_RRR_RRR_Specialization(rr_sub.e, op_axs, Set.empty, rr_sub, rr_sup)
    }
  }

  case class Resolvable_RRR_CRRR_Specialization
  (override val c: OWLClass,
   override val axs: Set[OWLSubClassOfAxiom],
   override val oxs: Set[(OWLObjectProperty, OWLObjectProperty)],
   override val sub: ReifiedRelationshipRestriction,
   override val sup: CardinalityRestrictedReifiedRelationship)
    extends ResolvableConceptualRelationshipSpecialization

  object Resolvable_RRR_CRRR_Specialization {

    def make
    (rr_sub: ReifiedRelationshipRestriction,
     rr_sup: CardinalityRestrictedReifiedRelationship,
     ax: OWLSubClassOfAxiom)
    (s_ax: Set[OWLSubClassOfAxiom],
     t_ax: Set[OWLSubClassOfAxiom])
    : Resolvable_RRR_CRRR_Specialization
    = {
      val op_axs = Set(ax) ++ s_ax ++ t_ax
      Resolvable_RRR_CRRR_Specialization(rr_sub.e, op_axs, Set.empty, rr_sub, rr_sup)
    }
  }

  case class Resolvable_CRRR_RR_Specialization
  (override val c: OWLClass,
   override val axs: Set[OWLSubClassOfAxiom],
   override val oxs: Set[(OWLObjectProperty, OWLObjectProperty)],
   override val sub: CardinalityRestrictedReifiedRelationship,
   override val sup: ReifiedRelationship)
    extends ResolvableConceptualRelationshipSpecialization

  object Resolvable_CRRR_RR_Specialization {

    def make
    (rr_sub: CardinalityRestrictedReifiedRelationship,
     rr_sup: ReifiedRelationship,
     ax: OWLSubClassOfAxiom)
    (s_ax: Set[OWLSubClassOfAxiom],
     t_ax: Set[OWLSubClassOfAxiom])
    : Resolvable_CRRR_RR_Specialization
    = {
      val op_axs = Set(ax) ++ s_ax ++ t_ax
      Resolvable_CRRR_RR_Specialization(rr_sub.e, op_axs, Set.empty, rr_sub, rr_sup)
    }
  }


  case class Resolvable_CRRR_RRR_Specialization
  (override val c: OWLClass,
   override val axs: Set[OWLSubClassOfAxiom],
   override val oxs: Set[(OWLObjectProperty, OWLObjectProperty)],
   override val sub: CardinalityRestrictedReifiedRelationship,
   override val sup: ReifiedRelationshipRestriction)
    extends ResolvableConceptualRelationshipSpecialization

  object Resolvable_CRRR_RRR_Specialization {

    def make
    (rr_sub: CardinalityRestrictedReifiedRelationship,
     rr_sup: ReifiedRelationshipRestriction,
     ax: OWLSubClassOfAxiom)
    (s_ax: Set[OWLSubClassOfAxiom],
     t_ax: Set[OWLSubClassOfAxiom])
    : Resolvable_CRRR_RRR_Specialization
    = {
      val op_axs = Set(ax) ++ s_ax ++ t_ax
      Resolvable_CRRR_RRR_Specialization(rr_sub.e, op_axs, Set.empty, rr_sub, rr_sup)
    }
  }

  case class Resolvable_CRRR_CRRR_Specialization
  (override val c: OWLClass,
   override val axs: Set[OWLSubClassOfAxiom],
   override val oxs: Set[(OWLObjectProperty, OWLObjectProperty)],
   override val sub: CardinalityRestrictedReifiedRelationship,
   override val sup: CardinalityRestrictedReifiedRelationship)
    extends ResolvableConceptualRelationshipSpecialization

  object Resolvable_CRRR_CRRR_Specialization {

    def make
    (rr_sub: CardinalityRestrictedReifiedRelationship,
     rr_sup: CardinalityRestrictedReifiedRelationship,
     ax: OWLSubClassOfAxiom)
    (s_ax: Set[OWLSubClassOfAxiom],
     t_ax: Set[OWLSubClassOfAxiom])
    : Resolvable_CRRR_CRRR_Specialization
    = {
      val op_axs = Set(ax) ++ s_ax ++ t_ax
      Resolvable_CRRR_CRRR_Specialization(rr_sub.e, op_axs, Set.empty, rr_sub, rr_sup)
    }
  }

  case class ResolvableUnreifiedRelationshipSpecialization
  (sub: UnreifiedRelationship,
   sup: UnreifiedRelationship,
   tuple: (OWLObjectProperty, OWLObjectProperty))

  case class ResolvableEntityRestriction
  (ax: OWLSubClassOfAxiom,
   restrictedDomain: Entity,
   restrictedRelationship: RestrictableRelationship,
   restrictedRange: Entity)

  case class ResolvableDataRelationshipsFromEntity2Scalar
  (domain: Entity,
   rel: OWLDataProperty,
   isIdentityCriteria: Boolean,
   range: DataRange,
   tuple: DOPInfo)

  case class ResolvableEntityScaladSubDataPropertyAxiom
  (sub: EntityScalarDataProperty,
   sup: EntityScalarDataProperty,
   tuple: (OWLDataProperty, OWLDataProperty))

  case class ResolvableEntityScalarDataPropertyParticularRestrictionAxiom
  (ax: OWLSubClassOfAxiom,
   e: Entity,
   dp: EntityScalarDataProperty,
   literalValue: tables.LiteralValue,
   valueType: Option[DataRange])

  case class ResolvableEntityScalarDataPropertyDataRangeRestrictionAxiom
  (ax: OWLSubClassOfAxiom,
   e: Entity,
   dp: EntityScalarDataProperty,
   dr: DataRange)

  /**
    *
    * @param entityDefinitions                                    Input & Result
    * @param restrictableRelationships                            Input & Result
    * @param dataRanges                                           Input & Result
    * @param dataRelationshipsFromEntityToScalar                  Input & Result
    * @param RCs                                                  Empty on successful resolution
    * @param ROPs                                                 Empty on successful resolution
    * @param sourceROPs                                           Empty on successful resolution
    * @param targetROPs                                           Empty on successful resolution
    * @param chains                                               Empty on successful resolution
    * @param unreifiedObjectPropertyOPs                           Empty on successful resolution
    * @param cardinalityRestrictedAspectCIRIs                     Empty on successful resolution
    * @param cardinalityRestrictedConceptCIRIs                    Empty on successful resolution
    * @param cardinalityRestrictedReifiedRelationshipCIRIs        Empty on successful resolution
    * @param subClassAxioms                                       Empty on successful resolution
    * @param subObjectPropertyAxioms                              Empty on successful resolution
    * @param subDataPropertyAxioms                                Empty on successful resolution
    * @param dataPropertyDPIRIs                                   Empty on successful resolution
    * @param reifiedRelationships                                 Result
    * @param reifiedRelationshipSources                           Result
    * @param reifiedRelationshipTargets                           Result
    * @param forwardProperties                                    Result
    * @param inverseProperties                                    Result
    * @param unreifiedRelationships                               Result
    * @param cardinalityRestrictedAspects                         Result
    * @param cardinalityRestrictedConcepts                        Result
    * @param cardinalityRestrictedReifiedRelationships            Result
    * @param reifiedRelationshipRestrictions                      Result
    * @param aspectSpecializations                                Result
    * @param conceptSpecializations                               Result
    * @param reifiedRelationshipSpecializations                   Result
    * @param existentialRestrictionAxioms                         Result
    * @param universalRestrictionAxioms                           Result
    * @param entityScalarDataPropertyParticularRestrictionAxioms  Result
    * @param entityScalarDataPropertyUniversalRestrictionAxioms   Result
    * @param entityScalarDataPropertyExistentialRestrictionAxioms Result
    * @param rootCharacterizedEntityRelationships                 Result
    * @param subObjectProperties                                  Result
    * @param subEntityScalarDataProperties                        Result
    */
  case class IncrementalResolverState(
      entityDefinitions: SortedMap[IRI, Entity],
      restrictableRelationships: Map[OWLObjectProperty, RestrictableRelationship],
      dataRanges: Map[OWLDataRange, DataRange],
      dataRelationshipsFromEntityToScalar: Map[OWLDataProperty, EntityScalarDataProperty],

      RCs: Map[IRI, OWLClass],
      ROPs: Set[ROPInfo],
      sourceROPs: Set[ROPInfo],
      targetROPs: Set[ROPInfo],
      chains: Chains,
      unreifiedObjectPropertyOPs: Set[(OWLClass, OWLObjectProperty, OWLClass)],
      cardinalityRestrictedAspectCIRIs: Map[IRI, OWLClass],
      cardinalityRestrictedConceptCIRIs: Map[IRI, OWLClass],
      cardinalityRestrictedReifiedRelationshipCIRIs: Map[IRI, OWLClass],
      subClassAxioms: Map[OWLClass, Set[OWLSubClassOfAxiom]],
      subObjectPropertyAxioms: SortedSet[(OWLObjectProperty, OWLObjectProperty)],
      subDataPropertyAxioms: Set[(OWLDataProperty, OWLDataProperty)],
      dataPropertyDPIRIs: Set[DOPInfo],

      reifiedRelationships: Map[OWLClass, ReifiedRelationship] = Map.empty,
      reifiedRelationshipSources: Map[OWLObjectProperty, ReifiedRelationship] = Map.empty,
      reifiedRelationshipTargets: Map[OWLObjectProperty, ReifiedRelationship] = Map.empty,
      forwardProperties: Map[OWLObjectProperty, ForwardProperty] = Map.empty,
      inverseProperties: Map[OWLObjectProperty, InverseProperty] = Map.empty,
      unreifiedRelationships: Map[OWLObjectProperty, UnreifiedRelationship] = Map.empty,
      cardinalityRestrictedAspects: Map[OWLClass, CardinalityRestrictedAspect] = Map.empty,
      cardinalityRestrictedConcepts: Map[OWLClass, CardinalityRestrictedConcept] = Map.empty,
      cardinalityRestrictedReifiedRelationships: Map[OWLClass, CardinalityRestrictedReifiedRelationship] = Map.empty,
      reifiedRelationshipRestrictions: Map[OWLClass, ReifiedRelationshipRestriction] = Map.empty,
      aspectSpecializations: Map[OWLSubClassOfAxiom, AspectSpecializationAxiom] = Map.empty,
      conceptSpecializations: Map[OWLSubClassOfAxiom, ConceptSpecializationAxiom] = Map.empty,
      reifiedRelationshipSpecializations: Map[OWLSubClassOfAxiom, ReifiedRelationshipSpecializationAxiom] = Map.empty,
      existentialRestrictionAxioms: Map[OWLSubClassOfAxiom, EntityExistentialRestrictionAxiom] = Map.empty,
      universalRestrictionAxioms: Map[OWLSubClassOfAxiom, EntityUniversalRestrictionAxiom] = Map.empty,
      entityScalarDataPropertyParticularRestrictionAxioms: Map[OWLSubClassOfAxiom, EntityScalarDataPropertyParticularRestrictionAxiom] = Map.empty,
      entityScalarDataPropertyUniversalRestrictionAxioms: Map[OWLSubClassOfAxiom, EntityScalarDataPropertyUniversalRestrictionAxiom] = Map.empty,
      entityScalarDataPropertyExistentialRestrictionAxioms: Map[OWLSubClassOfAxiom, EntityScalarDataPropertyExistentialRestrictionAxiom] = Map.empty,
      rootCharacterizedEntityRelationships: Map[ConceptualRelationship, Set[_ <: CharacterizedEntityRelationship]] = Map.empty,
      subObjectProperties: Map[(OWLObjectProperty, OWLObjectProperty), SubObjectPropertyOfAxiom] = Map.empty,
      subEntityScalarDataProperties: Map[(OWLDataProperty, OWLDataProperty), SubDataPropertyOfAxiom] = Map.empty) {

    def removeSubClassAxioms(c: OWLClass, axs: Set[OWLSubClassOfAxiom]): IncrementalResolverState = {
      val result = subClassAxioms.getOrElse(c, Set.empty[OWLSubClassOfAxiom]) -- axs
      if (result.isEmpty)
        copy(subClassAxioms = this.subClassAxioms - c)
      else
        copy(subClassAxioms = this.subClassAxioms.updated(c, result))
    }

    def isResolved: Boolean =
      RCs.isEmpty &&
        ROPs.isEmpty &&
        sourceROPs.isEmpty &&
        targetROPs.isEmpty &&
        chains.isEmpty &&
        unreifiedObjectPropertyOPs.isEmpty &&
        cardinalityRestrictedAspectCIRIs.isEmpty &&
        cardinalityRestrictedConceptCIRIs.isEmpty &&
        cardinalityRestrictedReifiedRelationshipCIRIs.isEmpty &&
        subClassAxioms.isEmpty &&
        subObjectPropertyAxioms.isEmpty &&
        subDataPropertyAxioms.isEmpty &&
        dataPropertyDPIRIs.isEmpty

    def allEntities: Map[OWLClass, Entity]
    = entityDefinitions.map { case (_, e) => e.e -> e } ++
      reifiedRelationships ++
      cardinalityRestrictedAspects ++
      cardinalityRestrictedConcepts ++
      cardinalityRestrictedReifiedRelationships ++
      reifiedRelationshipRestrictions

    def lookup(c: OWLClass): Option[Entity] =
      entityDefinitions.get(c.getIRI) orElse
        reifiedRelationships.get(c) orElse
        cardinalityRestrictedAspects.get(c) orElse
        cardinalityRestrictedConcepts.get(c) orElse
        cardinalityRestrictedReifiedRelationships.get(c) orElse
        reifiedRelationshipRestrictions.get(c)

    def lookupRestrictableRelationship(op: OWLObjectProperty): Option[RestrictableRelationship]
    = restrictableRelationships.get(op) orElse
      forwardProperties.get(op) orElse
      inverseProperties.get(op) orElse
      unreifiedRelationships.get(op)

    def lookupConceptualRelationship(c: OWLClass)
    : Option[ConceptualRelationship]
    = entityDefinitions.get(c.getIRI).selectByKindOf { case cr: ConceptualRelationship => cr } orElse
      reifiedRelationships.get(c) orElse
      reifiedRelationshipRestrictions.get(c) orElse
      cardinalityRestrictedReifiedRelationships.get(c)

    def resolvableUOPs(ont: OWLOntology): Iterable[ResolvableUOPTuple]
    = if (chains.isEmpty)
      for {
        tuple <- unreifiedObjectPropertyOPs
        d <- lookup(tuple._1)
        r <- lookup(tuple._3)

        op = tuple._2
        maybeFunctional = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isFunctional,
          Map(
            op -> ont
              .functionalObjectPropertyAxioms(op)
              .iterator()
              .hasNext))

        maybeInverseFunctional = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isInverseFunctional,
          Map(
            op -> ont
              .inverseFunctionalObjectPropertyAxioms(op)
              .iterator()
              .hasNext))

        maybeSymmetric = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isSymmetric,
          Map(
            op -> ont
              .symmetricObjectPropertyAxioms(op)
              .iterator()
              .hasNext))

        maybeAsymmetric = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isAsymmetric,
          Map(
            op -> ont
              .asymmetricObjectPropertyAxioms(op)
              .iterator()
              .hasNext))

        maybeReflexive = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isReflexive,
          Map(
            op -> ont
              .reflexiveObjectPropertyAxioms(op)
              .iterator()
              .hasNext))

        maybeIrreflexive = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isIrreflexive,
          Map(
            op -> ont
              .irreflexiveObjectPropertyAxioms(op)
              .iterator()
              .hasNext))

        maybeEssential = ont
          .subClassAxiomsForSubClass(tuple._1)
          .toScala[Set]
          .flatMap { ax =>
            ax.getSuperClass match {
              case oex: OWLObjectExactCardinality
                if 1 == oex.getCardinality &&
                  op == oex.getProperty &&
                  r.e == oex.getFiller =>
                Some(RelationshipCharacteristics.isEssential)
              case _ =>
                None
            }
          }
          .headOption

        characteristics = Iterable() ++
          maybeFunctional ++ maybeInverseFunctional ++
          maybeSymmetric ++ maybeAsymmetric ++
          maybeReflexive ++ maybeIrreflexive ++
          maybeEssential

      } yield ResolvableUOPTuple(d, r, tuple, characteristics)
    else
      Iterable.empty[ResolvableUOPTuple]

    def resolvableROPs(ont: OWLOntology): Iterable[ResolvableROPTuple] = {
      val result = for {
        chain <- chains
        (chainOP, chainSource, chainTarget) = chain

        resolvedROP <- ROPs
        (r_iri, r_op, r_source, r_target, r_inv_op) = resolvedROP
        if chainOP.getIRI == r_iri

        resolvedSourceROP <- sourceROPs
        (s_iri, s_op, s_source, s_target, _) = resolvedSourceROP
        if s_iri == chainSource.getIRI && s_target.getIRI == r_source.getIRI

        resolvedTargetROP <- targetROPs
        (t_iri, t_op, t_source, t_target, _) = resolvedTargetROP
        if t_iri == chainTarget.getIRI && t_target.getIRI == r_target.getIRI

        r_sourceDef <- lookup(r_source)
        r_targetDef <- lookup(r_target)
        if s_source == t_source
        rc = s_source

        maybeFunctional = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isFunctional,
          Map(s_op -> ont
            .inverseFunctionalObjectPropertyAxioms(s_op)
            .iterator()
            .hasNext,
            r_op -> ont
              .functionalObjectPropertyAxioms(r_op)
              .iterator()
              .hasNext),
          r_inv_op.map { ui =>
            ui -> ont
              .inverseFunctionalObjectPropertyAxioms(ui)
              .iterator()
              .hasNext
          }
        )

        maybeInverseFunctional = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isInverseFunctional,
          Map(t_op -> ont
            .inverseFunctionalObjectPropertyAxioms(t_op)
            .iterator()
            .hasNext,
            r_op -> ont
              .inverseFunctionalObjectPropertyAxioms(r_op)
              .iterator()
              .hasNext),
          r_inv_op.map { ui =>
            ui -> ont.functionalObjectPropertyAxioms(ui).iterator().hasNext
          }
        )

        maybeSymmetric = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isSymmetric,
          Map(
            r_op -> ont.symmetricObjectPropertyAxioms(r_op).iterator().hasNext),
          r_inv_op.map { ui =>
            ui -> ont.symmetricObjectPropertyAxioms(ui).iterator().hasNext
          }
        )

        maybeAsymmetric = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isAsymmetric,
          Map(
            r_op -> ont
              .asymmetricObjectPropertyAxioms(r_op)
              .iterator()
              .hasNext),
          r_inv_op.map { ui =>
            ui -> ont.asymmetricObjectPropertyAxioms(ui).iterator().hasNext
          }
        )

        maybeReflexive = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isReflexive,
          Map(
            r_op -> ont.reflexiveObjectPropertyAxioms(r_op).iterator().hasNext),
          r_inv_op.map { ui =>
            ui -> ont.reflexiveObjectPropertyAxioms(ui).iterator().hasNext
          }
        )

        maybeIrreflexive = hasRelationshipCharacteristic(
          RelationshipCharacteristics.isIrreflexive,
          Map(
            r_op -> ont
              .irreflexiveObjectPropertyAxioms(r_op)
              .iterator()
              .hasNext),
          r_inv_op.map { ui =>
            ui -> ont.irreflexiveObjectPropertyAxioms(ui).iterator().hasNext
          }
        )

        maybeEssential = ont
          .subClassAxiomsForSubClass(r_source)
          .toScala[Set]
          .flatMap { ax =>
            ax.getSuperClass match {
              case oex: OWLObjectExactCardinality
                if 1 == oex.getCardinality &&
                  r_op == oex.getProperty &&
                  r_target == oex.getFiller =>
                Some(RelationshipCharacteristics.isEssential)
              case _ =>
                None
            }
          }
          .headOption

        maybeInverseEssential = r_inv_op.flatMap { ui =>
          ont
            .subClassAxiomsForSubClass(r_target)
            .toScala[Set]
            .flatMap { ax =>
              ax.getSuperClass match {
                case oex: OWLObjectExactCardinality
                  if 1 == oex.getCardinality &&
                    ui == oex.getProperty &&
                    r_source == oex.getFiller =>
                  Some(RelationshipCharacteristics.isEssential)
                case _ =>
                  None
              }
            }
            .headOption
        }

        characteristics = Iterable() ++
          maybeFunctional ++ maybeInverseFunctional ++
          maybeSymmetric ++ maybeAsymmetric ++
          maybeReflexive ++ maybeIrreflexive ++
          maybeEssential ++ maybeInverseEssential

        tuple = ResolvableROPTuple(rc,
          resolvedROP,
          r_sourceDef,
          resolvedSourceROP,
          r_targetDef,
          resolvedTargetROP,
          chain,
          characteristics)
      } yield tuple

      result
    }

    def lookupObjectCardinalityRestriction[D <: Entity]
    (sub: OWLClass,
     ax: OWLSubClassOfAxiom,
     restrictionKind: CardinalityRestrictionKind,
     ocr: OWLObjectCardinalityRestriction,
     domainLookup: Entity => Option[D])
    (implicit store: OWLAPIOMFGraphStore)
    : Option[ResolvableCardinalityRestriction[D]]
    = ocr.getProperty match {
      case op: OWLObjectProperty =>
        val card = tables.taggedTypes.positiveIntegerLiteral(ocr.getCardinality.toString)
        restrictableRelationships.get(op).flatMap { rr: RestrictableRelationship =>
          domainLookup(rr.domain()).flatMap { domain: D =>
            Option.apply(ocr.getFiller) match {
              case Some(rc: OWLClass) =>
                lookup(rc).map { range =>
                  ResolvableCardinalityRestriction[D](sub,
                    ax, domain,
                    restrictionKind,
                    rr,
                    Some(range),
                    card)
                }
              case _ =>
                Some(
                  ResolvableCardinalityRestriction[D](sub,
                    ax,
                    domain,
                    restrictionKind,
                    rr,
                    None,
                    card))
            }
          }
        }
      case _ =>
        None
    }

    def resolvableCardinalityRestrictedAspects
    (ont: OWLOntology)
    (implicit store: OWLAPIOMFGraphStore)
    : Iterable[ResolvableCardinalityRestriction[AspectKind]]
    = {
      val result = for {
        cr <- cardinalityRestrictedAspectCIRIs
        (_, cr_o) = cr
        rra <- ont.subClassAxiomsForSubClass(cr_o).toScala[Set].flatMap { ax =>
          ax.getSuperClass match {
            case ocr: OWLObjectMinCardinality =>
              lookupObjectCardinalityRestriction(cr_o,
                ax,
                MinCardinalityRestriction,
                ocr,
                asAspectKind)
            case ocr: OWLObjectMaxCardinality =>
              lookupObjectCardinalityRestriction(cr_o,
                ax,
                MaxCardinalityRestriction,
                ocr,
                asAspectKind)
            case ocr: OWLObjectExactCardinality =>
              lookupObjectCardinalityRestriction(cr_o,
                ax,
                ExactCardinalityRestriction,
                ocr,
                asAspectKind)
            case _ =>
              None
          }
        }
      } yield rra

      result
    }

    def resolvableCardinalityRestrictedConcepts
    (ont: OWLOntology)
    (implicit store: OWLAPIOMFGraphStore)
    : Iterable[ResolvableCardinalityRestriction[ConceptKind]]
    = {
      val result = for {
        cr <- cardinalityRestrictedConceptCIRIs
        (_, cr_o) = cr
        rrc <- ont.subClassAxiomsForSubClass(cr_o).toScala[Set].flatMap { ax =>
          ax.getSuperClass match {
            case ocr: OWLObjectMinCardinality =>
              lookupObjectCardinalityRestriction(cr_o,
                ax,
                MinCardinalityRestriction,
                ocr,
                asConceptKind)
            case ocr: OWLObjectMaxCardinality =>
              lookupObjectCardinalityRestriction(cr_o,
                ax,
                MaxCardinalityRestriction,
                ocr,
                asConceptKind)
            case ocr: OWLObjectExactCardinality =>
              lookupObjectCardinalityRestriction(cr_o,
                ax,
                ExactCardinalityRestriction,
                ocr,
                asConceptKind)
            case _ =>
              None
          }
        }
      } yield rrc

      result
    }

    def resolvableCardinalityRestrictedReifiedRelationships
    (ont: OWLOntology)
    (implicit store: OWLAPIOMFGraphStore)
    : Iterable[ResolvableCardinalityRestriction[ConceptualRelationship]]
    = {
      val result = for {
        cr <- cardinalityRestrictedReifiedRelationshipCIRIs
        (_, cr_o) = cr
        rrc <- ont.subClassAxiomsForSubClass(cr_o).toScala[Set].flatMap { ax =>
          ax.getSuperClass match {
            case ocr: OWLObjectMinCardinality =>
              lookupObjectCardinalityRestriction(cr_o,
                ax,
                MinCardinalityRestriction,
                ocr, asConceptualRelationship)
            case ocr: OWLObjectMaxCardinality =>
              lookupObjectCardinalityRestriction(cr_o,
                ax,
                MaxCardinalityRestriction,
                ocr, asConceptualRelationship)
            case ocr: OWLObjectExactCardinality =>
              lookupObjectCardinalityRestriction(cr_o,
                ax,
                ExactCardinalityRestriction,
                ocr, asConceptualRelationship)
            case _ =>
              None
          }
        }
      } yield rrc

      result
    }

    def resolvableAspectSubClassAxioms
    : Iterable[ResolvableAspectSpecialization]
    = {
      val result = for {
        cax <- subClassAxioms
        (c, axs) = cax
        ax <- axs
        sub <- lookup(c)
        supC <- TerminologyBoxResolverHelper.owlclassOfCE(
          Option.apply(ax.getSuperClass))
        sup <- lookup(supC).selectByKindOf { case ak: AspectKind => ak }
      } yield ResolvableAspectSpecialization(c, ax, sub, sup)
      result
    }

    def resolvableConceptSubClassAxioms
    : Iterable[ResolvableConceptSpecialization]
    = {
      val result = for {
        cax <- subClassAxioms
        (c, axs) = cax
        ax <- axs
        sub <- lookup(c).selectByKindOf { case ck: ConceptKind => ck }
        supC <- TerminologyBoxResolverHelper.owlclassOfCE(
          Option.apply(ax.getSuperClass))
        sup <- lookup(supC).selectByKindOf { case ck: ConceptKind => ck }
      } yield ResolvableConceptSpecialization(c, ax, sub, sup)
      result
    }

    def lookupSupConceptualRelationship
    (axs: Set[OWLSubClassOfAxiom])
    : Iterable[(OWLSubClassOfAxiom, ConceptualRelationship)]
    = for {
      ax <- axs
      o_sup <- TerminologyBoxResolverHelper.owlclassOfCE(
        Option.apply(ax.getSuperClass))
      cr_sup <- lookupConceptualRelationship(o_sup)
    } yield ax -> cr_sup

    def lookupConceptualRelationshipBy
    (op: OWLObjectProperty,
     f: ConceptualRelationship => OWLObjectProperty)
    : Option[ConceptualRelationship]
    = {
      reifiedRelationships.values.find { rr => f(rr) == op } orElse
        reifiedRelationshipRestrictions.values.find { rr => f(rr) == op } orElse
        cardinalityRestrictedReifiedRelationships.values.find { rr => f(rr) == op }
    }

    def lookupRestrictionBy
    (axs: Set[OWLSubClassOfAxiom],
     f: OWLObjectProperty => Boolean,
     r: OWLClass => Boolean)
    : Option[OWLSubClassOfAxiom]
    = axs.find { ax =>
      Option.apply(ax.getSuperClass) match {
        case Some(osv: OWLObjectSomeValuesFrom) =>
          (Option.apply(osv.getProperty), Option.apply(osv.getFiller)) match {
            case (Some(op: OWLObjectProperty), Some(c: OWLClass)) =>
              if (f(op) && r(c))
                true
              else
                false
            case _ =>
              false
          }
        case _ =>
          false
      }
    }

    def lookupRestrictionBy
    (current: Map[OWLClass, Set[OWLSubClassOfAxiom]],
     axs: Set[OWLSubClassOfAxiom],
     f: OWLObjectProperty => Boolean)
    : Map[OWLClass, Set[OWLSubClassOfAxiom]]
    = axs.foldLeft[Map[OWLClass, Set[OWLSubClassOfAxiom]]](current) { case (acc, ax) =>
      Option.apply(ax.getSuperClass) match {
        case Some(osv: OWLObjectSomeValuesFrom) =>
          (Option.apply(osv.getProperty), Option.apply(osv.getFiller)) match {
            case (Some(op: OWLObjectProperty), Some(c: OWLClass)) =>
              if (f(op))
                acc.updated(c, acc.getOrElse(c, Set.empty[OWLSubClassOfAxiom]) + ax)
              else
                acc
            case _ =>
              acc
          }
        case _ =>
          acc
      }
    }

    def lookupReifiedRelationshipRestrictionSpecializationAxioms
    (o_rrr: OWLClass, axs: Set[OWLSubClassOfAxiom])
    (implicit store: OWLAPIOMFGraphStore)
    : Iterable[((Entity, Entity), ConceptualRelationship, Set[OWLSubClassOfAxiom])]
    = for {
      (sup_ax, cr_sup) <- lookupSupConceptualRelationship(axs)
      root_rrs = cr_sup.rootReifiedRelationships()
      source_axs = root_rrs.foldLeft[Map[OWLClass, Set[OWLSubClassOfAxiom]]](Map.empty) { case (acc, root_rr) =>
        lookupRestrictionBy(acc, axs, root_rr.rSource == _)
      }
      if source_axs.size == 1
      (o_source, ax_sources) = source_axs.head
      if root_rrs.size == ax_sources.size
      e_source <- lookup(o_source)

      target_axs = root_rrs.foldLeft[Map[OWLClass, Set[OWLSubClassOfAxiom]]](Map.empty) { case (acc, root_rr) =>
        lookupRestrictionBy(acc, axs, root_rr.rTarget == _)
      }
      if target_axs.size == 1
      (o_target, ax_targets) = target_axs.head
      if root_rrs.size == ax_targets.size
      e_target <- lookup(o_target)

      axs = Set(sup_ax) ++ ax_sources ++ ax_targets
    } yield (e_source -> e_target, cr_sup, axs)

    def resolvableReifiedRelationshipRestrictionSubClassAxioms
    (implicit store: OWLAPIOMFGraphStore)
    : Iterable[ResolvableReifiedRelationshipRestrictionSpecialization]
    = for {
      (iri_rrr, o_rrr) <- RCs
      tuple <- subClassAxioms
      (o_sub, axs) = tuple
      if o_rrr == o_sub
      tuples = lookupReifiedRelationshipRestrictionSpecializationAxioms(o_rrr, axs)
      if tuples.nonEmpty
      bySourceTarget = tuples.groupBy(_._1).map { case (k, vs) => k -> vs.map { v => v._2 -> v._3 } }
      if bySourceTarget.size == 1
      ((source, target), specializations) = bySourceTarget.head
    } yield ResolvableReifiedRelationshipRestrictionSpecialization(iri_rrr, o_rrr, source, target, specializations)

    def resolvableConceptualRelationshipSubClassAxiom1
    (ax: OWLSubClassOfAxiom,
     rr_sub: ReifiedRelationship,
     rr_sup: ReifiedRelationship)
    : ValidationNel[String, ResolvableConceptualRelationshipSpecialization]
    = {
      val op_rSources: ValidationNel[String, (OWLObjectProperty, OWLObjectProperty)] = subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
        op_sub == rr_sub.rSource && op_sup == rr_sup.rSource
      }.fold[ValidationNel[String, (OWLObjectProperty, OWLObjectProperty)]](
        Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationship(${rr_sup.abbrevIRI}): Missing subObjectPropertyAxiom for the 'source' properties.")
      )(Success(_))
      val op_rTargets: ValidationNel[String, (OWLObjectProperty, OWLObjectProperty)] = subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
        op_sub == rr_sub.rTarget && op_sup == rr_sup.rTarget
      }.fold[ValidationNel[String, (OWLObjectProperty, OWLObjectProperty)]](
        Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationship(${rr_sup.abbrevIRI}): Missing subObjectPropertyAxiom for the 'target' properties.")
      )(Success(_))
      val op_rUs: ValidationNel[String, (OWLObjectProperty, OWLObjectProperty)] = subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
        op_sub == rr_sub.forwardProperty.e && op_sup == rr_sup.forwardProperty.e
      }.fold[ValidationNel[String, (OWLObjectProperty, OWLObjectProperty)]](
        Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationship(${rr_sup.abbrevIRI}): Missing subObjectPropertyAxiom for the 'forward' properties.")
      )(Success(_))
      val with_inverses = rr_sub.inverseProperty.isDefined && rr_sup.inverseProperty.isDefined
      val op_rIs: ValidationNel[String, Option[(OWLObjectProperty, OWLObjectProperty)]]
      = if (with_inverses)
        rr_sub.inverseProperty.flatMap { sub_i =>
          rr_sup.inverseProperty.flatMap { sup_i =>
            subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
              op_sub == sub_i.e && op_sup == sup_i.e
            }
          }
        }.fold[ValidationNel[String, Option[(OWLObjectProperty, OWLObjectProperty)]]](
          Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationship(${rr_sup.abbrevIRI}): Missing subObjectPropertyAxiom for the 'inverse' properties.")
        )(pair => Success(Some(pair)))
      else
        Success(None)

      ( op_rSources |@| op_rTargets |@| op_rUs |@| op_rIs)(Resolvable_RR_RR_Specialization.make(rr_sub, rr_sup, ax))
    }

    def resolvableConceptualRelationshipSubClassAxiom2
    (ax: OWLSubClassOfAxiom,
     rr_sub: ReifiedRelationship,
     rr_sup: ReifiedRelationshipRestriction)
    (implicit store: OWLAPIOMFGraphStore)
    : ValidationNel[String, ResolvableConceptualRelationshipSpecialization]
    = {
      val roots = rr_sup.rootReifiedRelationships()

      val op_rSources = {
        val tuples = roots.flatMap { root =>
          subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
            op_sub == rr_sub.rSource && op_sup == root.rSource
          }
        }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationshipRestriction(${rr_sup.abbrevIRI}): Missing 'source' sub-object property axioms: found ${tuples.size}, but ${roots.size} required, one for each root.")
      }

      val op_rTargets = {
        val tuples = roots.flatMap { root =>
          subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
            op_sub == rr_sub.rTarget && op_sup == root.rTarget
          }
        }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationshipRestriction(${rr_sup.abbrevIRI}): Missing 'target' sub-object property axioms: found ${tuples.size}, but ${roots.size} required, one for each root.")

      }
      val op_rUs = {
        val tuples = roots.flatMap { root =>
          subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
            op_sub == rr_sub.forwardProperty.e && op_sup == root.forwardProperty.e
          }
        }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationshipRestriction(${rr_sup.abbrevIRI}): Missing 'forward' sub-object property axioms: found ${tuples.size}, but ${roots.size} required, one for each root.")
      }

      val with_inverses = rr_sub.inverseProperty.isDefined && roots.all(_.inverseProperty.isDefined)

      val op_rIs = rr_sub.inverseProperty match {
        case Some(sub_i) =>
          val tuples = roots.flatMap { root =>
            root.inverseProperty.flatMap { sup_i =>
              subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
                op_sub == sub_i.e && op_sup == sup_i.e
              }
            }
          }
          if (tuples.size == roots.size)
            Success(tuples)
          else
            Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationshipRestriction(${rr_sup.abbrevIRI}): Missing 'inverse' sub-object property axioms: found ${tuples.size}, but ${roots.size} required, one for each root.")
        case _ =>
          Success(Set.empty[(OWLObjectProperty, OWLObjectProperty)])
      }

      ( op_rSources |@| op_rTargets |@| op_rUs |@| op_rIs)(Resolvable_RR_RRR_Specialization.make(rr_sub, rr_sup, ax))
    }

    def resolvableConceptualRelationshipSubClassAxiom3
    (ax: OWLSubClassOfAxiom,
     rr_sub: ReifiedRelationship,
     rr_sup: CardinalityRestrictedReifiedRelationship)
    (implicit store: OWLAPIOMFGraphStore)
    : ValidationNel[String, ResolvableConceptualRelationshipSpecialization]
    = {
      val roots = rr_sup.rootReifiedRelationships()

      val op_rSources = {
        val tuples = roots.flatMap { root =>
          subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
            op_sub == rr_sub.rSource && op_sup == root.rSource
          }
        }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < CardinalityRestrictedReifiedRelationship(${rr_sup.abbrevIRI}): Missing 'source' sub-object property axioms: found ${tuples.size}, but ${roots.size} required, one for each root.")
      }

      val op_rTargets = {
        val tuples = roots.flatMap { root =>
          subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
            op_sub == rr_sub.rTarget && op_sup == root.rTarget
          }
        }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < CardinalityRestrictedReifiedRelationship(${rr_sup.abbrevIRI}): Missing 'target' sub-object property axioms: found ${tuples.size}, but ${roots.size} required, one for each root.")

      }
      val op_rUs = {
        val tuples = roots.flatMap { root =>
          subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
            op_sub == rr_sub.forwardProperty.e && op_sup == root.forwardProperty.e
          }
        }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < CardinalityRestrictedReifiedRelationship(${rr_sup.abbrevIRI}): Missing 'forward' sub-object property axioms: found ${tuples.size}, but ${roots.size} required, one for each root.")
      }

      val with_inverses = rr_sub.inverseProperty.isDefined && roots.all(_.inverseProperty.isDefined)

      val op_rIs = rr_sub.inverseProperty match {
        case Some(sub_i) =>
          val tuples = roots.flatMap { root =>
            root.inverseProperty.flatMap { sup_i =>
              subObjectPropertyAxioms.find { case (op_sub, op_sup) =>
                op_sub == sub_i.e && op_sup == sup_i.e
              }
            }
          }
          if (tuples.size == roots.size)
            Success(tuples)
          else
            Validation.failureNel(s"ReifiedRelationship(${rr_sub.abbrevIRI}) < CardinalityRestrictedReifiedRelationship(${rr_sup.abbrevIRI}): Missing 'inverse' sub-object property axioms: found ${tuples.size}, but ${roots.size} required, one for each root.")
        case _ =>
          Success(Set.empty[(OWLObjectProperty, OWLObjectProperty)])
      }

      ( op_rSources |@| op_rTargets |@| op_rUs |@| op_rIs)(Resolvable_RR_CRRR_Specialization.make(rr_sub, rr_sup, ax))
    }

    def resolvableConceptualRelationshipSubClassAxiom4
    (axs: Set[OWLSubClassOfAxiom],
     ax: OWLSubClassOfAxiom,
     rr_sub: ReifiedRelationshipRestriction,
     rr_sup: ReifiedRelationship)
    (implicit store: OWLAPIOMFGraphStore)
    : ValidationNel[String, ResolvableConceptualRelationshipSpecialization]
    = {
      val s_ax = lookupRestrictionBy(axs, rr_sup.rSource == _, rr_sub.source.e == _).fold[ValidationNel[String, OWLSubClassOfAxiom]](
        Validation.failureNel(s"ReifiedRelationshipRestriction(${rr_sub.abbrevIRI}) < ReifiedRelationship(${rr_sup.abbrevIRI}): Missing restriction for the 'source' property")
      )(Success(_))

      val t_ax = lookupRestrictionBy(axs, rr_sup.rTarget == _, rr_sub.target.e == _).fold[ValidationNel[String, OWLSubClassOfAxiom]](
        Validation.failureNel(s"ReifiedRelationshipRestriction(${rr_sub.abbrevIRI}) < ReifiedRelationship(${rr_sup.abbrevIRI}): Missing restriction for the 'target' property")
      )(Success(_))

      (s_ax |@| t_ax)(Resolvable_RRR_RR_Specialization.make(rr_sub, rr_sup, ax))
    }

    def resolvableConceptualRelationshipSubClassAxiom5
    (axs: Set[OWLSubClassOfAxiom],
     ax: OWLSubClassOfAxiom,
     rr_sub: ReifiedRelationshipRestriction,
     rr_sup: ReifiedRelationshipRestriction)
    (implicit store: OWLAPIOMFGraphStore)
    : ValidationNel[String, ResolvableConceptualRelationshipSpecialization]
    = {
      val roots = rr_sup.rootReifiedRelationships()
      val s_ax = {
        val tuples = roots.flatMap { root => lookupRestrictionBy(axs, root.rSource == _, rr_sub.source.e == _) }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"ReifiedRelationshipRestriction(${rr_sub.abbrevIRI}) < ReifiedRelationshipRestriction(${rr_sup.abbrevIRI}): Missing 'source' sub-object property axioms: found ${tuples.size}, but ${roots.size} are required, one for each root.")
      }
      val t_ax = {
        val tuples = roots.flatMap { root => lookupRestrictionBy(axs, root.rTarget == _, rr_sub.target.e == _) }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"ReifiedRelationshipRestriction(${rr_sub.abbrevIRI}) < ReifiedRelationshipRestriction(${rr_sup.abbrevIRI}): Missing 'target' sub-object property axioms: found ${tuples.size}, but ${roots.size} are required, one for each root.")
      }
      (s_ax |@| t_ax) (Resolvable_RRR_RRR_Specialization.make(rr_sub, rr_sup, ax))
    }

    def resolvableConceptualRelationshipSubClassAxiom6
    (axs: Set[OWLSubClassOfAxiom],
     ax: OWLSubClassOfAxiom,
     rr_sub: ReifiedRelationshipRestriction,
     rr_sup: CardinalityRestrictedReifiedRelationship)
    (implicit store: OWLAPIOMFGraphStore)
    : ValidationNel[String, ResolvableConceptualRelationshipSpecialization]
    = {
      val roots = rr_sup.rootReifiedRelationships()
      val s_ax = {
        val tuples = roots.flatMap { root => lookupRestrictionBy(axs, root.rSource == _, rr_sub.source.e == _) }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"ReifiedRelationshipRestriction(${rr_sub.abbrevIRI}) < CardinalityRestrictedReifiedRelationship(${rr_sup.abbrevIRI}): Missing 'source' sub-object property axioms: found ${tuples.size}, but ${roots.size} are required, one for each root.")
      }
      val t_ax = {
        val tuples = roots.flatMap { root => lookupRestrictionBy(axs, root.rTarget == _, rr_sub.target.e == _) }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"ReifiedRelationshipRestriction(${rr_sub.abbrevIRI}) < CardinalityRestrictedReifiedRelationship(${rr_sup.abbrevIRI}): Missing 'target' sub-object property axioms: found ${tuples.size}, but ${roots.size} are required, one for each root.")
      }
      (s_ax |@| t_ax) (Resolvable_RRR_CRRR_Specialization.make(rr_sub, rr_sup, ax))
    }

    def resolvableConceptualRelationshipSubClassAxiom7
    (axs: Set[OWLSubClassOfAxiom],
     ax: OWLSubClassOfAxiom,
     rr_sub: CardinalityRestrictedReifiedRelationship,
     rr_sup: ReifiedRelationship)
    (implicit store: OWLAPIOMFGraphStore)
    : ValidationNel[String, ResolvableConceptualRelationshipSpecialization]
    = {
      val roots = rr_sup.rootReifiedRelationships()
      val s_ax = {
        val tuples = roots.flatMap { root => lookupRestrictionBy(axs, root.rSource == _, rr_sub.source.e == _) }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"CardinalityRestrictedReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationship(${rr_sup.abbrevIRI}): Missing 'source' sub-object property axioms: found ${tuples.size}, but ${roots.size} are required, one for each root.")
      }
      val t_ax = {
        val tuples = roots.flatMap { root => lookupRestrictionBy(axs, root.rTarget == _, rr_sub.target.e == _) }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"CardinalityRestrictedReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationship(${rr_sup.abbrevIRI}): Missing 'target' sub-object property axioms: found ${tuples.size}, but ${roots.size} are required, one for each root.")
      }
      (s_ax |@| t_ax) (Resolvable_CRRR_RR_Specialization.make(rr_sub, rr_sup, ax))
    }

    def resolvableConceptualRelationshipSubClassAxiom8
    (axs: Set[OWLSubClassOfAxiom],
     ax: OWLSubClassOfAxiom,
     rr_sub: CardinalityRestrictedReifiedRelationship,
     rr_sup: ReifiedRelationshipRestriction)
    (implicit store: OWLAPIOMFGraphStore)
    : ValidationNel[String, ResolvableConceptualRelationshipSpecialization]
    = {
      val roots = rr_sup.rootReifiedRelationships()
      val s_ax = {
        val tuples = roots.flatMap { root => lookupRestrictionBy(axs, root.rSource == _, rr_sub.source.e == _) }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"CardinalityRestrictedReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationshipRestriction(${rr_sup.abbrevIRI}): Missing 'source' sub-object property axioms: found ${tuples.size}, but ${roots.size} are required, one for each root.")
      }
      val t_ax = {
        val tuples = roots.flatMap { root => lookupRestrictionBy(axs, root.rTarget == _, rr_sub.target.e == _) }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"CardinalityRestrictedReifiedRelationship(${rr_sub.abbrevIRI}) < ReifiedRelationshipRestriction(${rr_sup.abbrevIRI}): Missing 'target' sub-object property axioms: found ${tuples.size}, but ${roots.size} are required, one for each root.")
      }
      (s_ax |@| t_ax) (Resolvable_CRRR_RRR_Specialization.make(rr_sub, rr_sup, ax))
    }

    def resolvableConceptualRelationshipSubClassAxiom9
    (axs: Set[OWLSubClassOfAxiom],
     ax: OWLSubClassOfAxiom,
     rr_sub: CardinalityRestrictedReifiedRelationship,
     rr_sup: CardinalityRestrictedReifiedRelationship)
    (implicit store: OWLAPIOMFGraphStore)
    : ValidationNel[String, ResolvableConceptualRelationshipSpecialization]
    = {
      val roots = rr_sup.rootReifiedRelationships()
      val s_ax = {
        val tuples = roots.flatMap { root => lookupRestrictionBy(axs, root.rSource == _, rr_sub.source.e == _) }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"CardinalityRestrictedReifiedRelationship(${rr_sub.abbrevIRI}) < CardinalityRestrictedReifiedRelationship(${rr_sup.abbrevIRI}): Missing 'source' sub-object property axioms: found ${tuples.size}, but ${roots.size} are required, one for each root.")
      }
      val t_ax = {
        val tuples = roots.flatMap { root => lookupRestrictionBy(axs, root.rTarget == _, rr_sub.target.e == _) }
        if (tuples.size == roots.size)
          Success(tuples)
        else
          Validation.failureNel(s"CardinalityRestrictedReifiedRelationship(${rr_sub.abbrevIRI}) < CardinalityRestrictedReifiedRelationship(${rr_sup.abbrevIRI}): Missing 'target' sub-object property axioms: found ${tuples.size}, but ${roots.size} are required, one for each root.")
      }
      (s_ax |@| t_ax) (Resolvable_CRRR_CRRR_Specialization.make(rr_sub, rr_sup, ax))
    }

    def resolvableConceptualRelationshipSubClassAxiom
    (ont: OWLOntology,
     cr_sub: ConceptualRelationship,
     cr_sup: ConceptualRelationship,
     ax: OWLSubClassOfAxiom,
     axs: Set[OWLSubClassOfAxiom])
    (implicit store: OWLAPIOMFGraphStore)
    : ValidationNel[String, ResolvableConceptualRelationshipSpecialization]
    = (cr_sub, cr_sup) match {
      case (rr_sub: ReifiedRelationship, rr_sup: ReifiedRelationship) =>
        /*
a|[source]
----
Class: sub
SubClassOf: sup

ObjectProperty: sub.rSource
SubPropertyOf: sup.rSource

ObjectProperty: sub.rTarget
SubPropertyOf: sup.rTarget

ObjectProperty: sub.rU
SubPropertyOf: sup.rU

ObjectProperty: sub.rI <1>
SubPropertyOf: sup.rI
----
<1> if `sub.rI` and `sup.rI` are defined.
           */
        resolvableConceptualRelationshipSubClassAxiom1(ax, rr_sub, rr_sup)

      case (rr_sub: ReifiedRelationship, rr_sup: ReifiedRelationshipRestriction) =>
        /*
a|[source]
----
Class: sub
SubClassOf: sup

ObjectProperty: sub.rSource
SubPropertyOf: root.rSource <1>

ObjectProperty: sub.rTarget
SubPropertyOf: root.rTarget <1>

ObjectProperty: sub.rU
SubPropertyOf: root.rU <1>

ObjectProperty: sub.rI <2>
SubPropertyOf: root.rI <1>
----
<1> for each `root: ReifiedRelationship in sup.rootCharacterizedEntityRelationships()`
<2> if `sub.rI` and `root.rI` are defined.

           */
        resolvableConceptualRelationshipSubClassAxiom2(ax, rr_sub, rr_sup)

      case (rr_sub: ReifiedRelationship, rr_sup: CardinalityRestrictedReifiedRelationship) =>
        /*
a|[source]
----
Class: sub
SubClassOf: sup

ObjectProperty: sub.rSource
SubPropertyOf: root.rSource <1>

ObjectProperty: sub.rTarget
SubPropertyOf: root.rTarget <1>

ObjectProperty: sub.rU
SubPropertyOf: root.rU <1>

ObjectProperty: sub.rI <2>
SubPropertyOf: root.rI <1>
----
<1> for each `root: ReifiedRelationship in sup.rootCharacterizedEntityRelationships()`
<2> if `sub.rI` and `root.rI` are defined.

           */
        resolvableConceptualRelationshipSubClassAxiom3(ax, rr_sub, rr_sup)

      case (rr_sub: ReifiedRelationshipRestriction, rr_sup: ReifiedRelationship) =>
        /*
a|[source]
----
Class: sub
SubClassOf: sup
SubClassOf: sup.rSource some sub.source
SubClassOf: sup.rTarget some sub.target
----
           */
        resolvableConceptualRelationshipSubClassAxiom4(axs, ax, rr_sub, rr_sup)

      case (rr_sub: ReifiedRelationshipRestriction, rr_sup: ReifiedRelationshipRestriction) =>
        /*
a|[source]
----
Class: sub
SubClassOf: sup
SubClassOf: root.rSource some sub.source <1>
SubClassOf: root.rTarget some sub.target <1>
----
<1> for each `root: ReifiedRelationship in sup.rootCharacterizedEntityRelationships()`

           */
        resolvableConceptualRelationshipSubClassAxiom5(axs, ax, rr_sub, rr_sup)

      case (rr_sub: ReifiedRelationshipRestriction, rr_sup: CardinalityRestrictedReifiedRelationship) =>
        /*
a|[source]
----
Class: sub
SubClassOf: sup
SubClassOf: root.rSource some sub.source <1>
SubClassOf: root.rTarget some sub.target <1>
----
<1> for each `root: ReifiedRelationship in sup.rootCharacterizedEntityRelationships()`

           */
        resolvableConceptualRelationshipSubClassAxiom6(axs, ax, rr_sub, rr_sup)
      case (rr_sub: CardinalityRestrictedReifiedRelationship, rr_sup: ReifiedRelationship) =>
        /*
a|[source]
----
Class: sub
SubClassOf: sup
SubClassOf: sup.rSource some domain(sub)
SubClassOf: sup.rTarget some range(sub)
----
           */
        resolvableConceptualRelationshipSubClassAxiom7(axs, ax, rr_sub, rr_sup)
      case (rr_sub: CardinalityRestrictedReifiedRelationship, rr_sup: ReifiedRelationshipRestriction) =>
        /*
a|[source]
----
Class: sub
SubClassOf: sup
SubClassOf: root.rSource some sub.source <1>
SubClassOf: root.rTarget some sub.target <1>
----
<1> for each `root: ReifiedRelationship in sup.rootCharacterizedEntityRelationships()`

           */
        resolvableConceptualRelationshipSubClassAxiom8(axs, ax, rr_sub, rr_sup)
      case (rr_sub: CardinalityRestrictedReifiedRelationship, rr_sup: CardinalityRestrictedReifiedRelationship) =>
        /*
a|[source]
----
Class: sub
SubClassOf: sup
SubClassOf: root.rSource some sub.source <1>
SubClassOf: root.rTarget some sub.target <1>
----
<1> for each `root: ReifiedRelationship in sup.rootCharacterizedEntityRelationships()`

           */
        resolvableConceptualRelationshipSubClassAxiom9(axs, ax, rr_sub, rr_sup)
    }

    def lookupSupConceptualRelationshipTriples
    (o_sub: OWLClass, axs: Set[OWLSubClassOfAxiom])
    : Option[Vector[(ConceptualRelationship, OWLSubClassOfAxiom, ConceptualRelationship)]]
    = lookupConceptualRelationship(o_sub).map { cr_sub =>
      val triples = for {
        ax <- axs
        o_sup <- TerminologyBoxResolverHelper.owlclassOfCE(
          Option.apply(ax.getSuperClass))
        cr_sup <- lookupConceptualRelationship(o_sup)
      } yield (cr_sub, ax, cr_sup)
      triples.to[Vector].sortBy(_._1.iri.toString)
    }

    /**
      *
      * @param ont
      * @param store
      * @return A triple: Resolved, Unresolved, Remaining
      */
    def partitionResolvableConceptualRelationshipSubClassAxioms
    (ont: OWLOntology)
    (implicit store: OWLAPIOMFGraphStore)
    : ValidationNel[String, Vector[ResolvableConceptualRelationshipSpecialization]]
    = subClassAxioms.foldLeft[ValidationNel[String, Vector[ResolvableConceptualRelationshipSpecialization]]] {
      Success(Vector.empty[ResolvableConceptualRelationshipSpecialization])
    } { case (acc1, (o_sub, axs)) =>
      lookupSupConceptualRelationshipTriples(o_sub, axs) match {
        case Some(triples) =>
          triples.foldLeft[ValidationNel[String, Vector[ResolvableConceptualRelationshipSpecialization]]](acc1) {
            case (acc2, (cr_sub, ax, cr_sup)) =>
              acc2 +++ resolvableConceptualRelationshipSubClassAxiom(ont, cr_sub, cr_sup, ax, axs).map(Vector(_))
          }
        case None =>
          acc1
      }
    }

    def resolvableConceptualRelationshipSubClassAxioms
    (ont: OWLOntology)
    (implicit store: OWLAPIOMFGraphStore)
    : Vector[ResolvableConceptualRelationshipSpecialization]
    = partitionResolvableConceptualRelationshipSubClassAxioms(ont)
      .fold[Vector[ResolvableConceptualRelationshipSpecialization]](
      (errors: NonEmptyList[String]) => {
        val buff = new scala.collection.mutable.StringBuilder
        buff ++= s"resolvableConceptualRelationshipSubClassAxioms: ${ont.getOntologyID.getOntologyIRI} has ${errors.size} resolution errors.\n"
        errors.foreach { e =>
          buff ++= e
          buff ++= "\n"
          ()
        }
        System.out.println(buff.toString)
        Vector.empty
      },
      identity)

    def resolvableUnreifiedRelationshipSpecializationAxioms
    : Iterable[ResolvableUnreifiedRelationshipSpecialization]
    = for {
      tuple <- subObjectPropertyAxioms
      (sub_op, sup_op) = tuple
      sub_ur <- unreifiedRelationships.get(sub_op)
      sup_ur <- unreifiedRelationships.get(sup_op)
    } yield ResolvableUnreifiedRelationshipSpecialization(sub_ur, sup_ur, tuple)

    def lookupExistentialEntityRestriction
    (ax: OWLSubClassOfAxiom)
    : Option[ResolvableEntityRestriction]
    = ax.getSuperClass match {
      case rax: OWLObjectSomeValuesFrom =>
        (TerminologyBoxResolverHelper
          .owlclassOfCE(Option.apply(ax.getSubClass))
          .flatMap(lookup),
          TerminologyBoxResolverHelper
            .owlObjectPropertyOfPE(Option.apply(rax.getProperty))
            .flatMap(lookupRestrictableRelationship),
          TerminologyBoxResolverHelper
            .owlclassOfCE(Option.apply(rax.getFiller))
            .flatMap(lookup)) match {
          case (Some(restrictedDomain), Some(restrictedRelationship), Some(restrictedRange)) =>
            Some(ResolvableEntityRestriction(ax, restrictedDomain, restrictedRelationship, restrictedRange))
          case _ =>
            None
        }
      case _ =>
        None
    }

    def lookupUniversalEntityRestriction
    (ax: OWLSubClassOfAxiom)
    : Option[ResolvableEntityRestriction]
    = ax.getSuperClass match {
      case rax: OWLObjectAllValuesFrom =>
        (TerminologyBoxResolverHelper
          .owlclassOfCE(Option.apply(ax.getSubClass))
          .flatMap(lookup),
          TerminologyBoxResolverHelper
            .owlObjectPropertyOfPE(Option.apply(rax.getProperty))
            .flatMap(lookupRestrictableRelationship),
          TerminologyBoxResolverHelper
            .owlclassOfCE(Option.apply(rax.getFiller))
            .flatMap(lookup)) match {
          case (Some(restrictedDomain), Some(restrictedRelationship), Some(restrictedRange)) =>
            Some(ResolvableEntityRestriction(ax, restrictedDomain, restrictedRelationship, restrictedRange))
          case _ =>
            None
        }
      case _ =>
        None
    }

    def resolvableExistentialEntityRestrictionAxioms
    : Iterable[ResolvableEntityRestriction]
    = for {
      tuple <- subClassAxioms
      (_, axs) = tuple
      tuple <- axs.flatMap(lookupExistentialEntityRestriction)
    } yield tuple

    def resolvableUniversalEntityRestrictionAxioms
    : Iterable[ResolvableEntityRestriction]
    = for {
      tuple <- subClassAxioms
      (_, axs) = tuple
      tuple <- axs.flatMap(lookupUniversalEntityRestriction)
    } yield tuple

    def resolvableDataRelationshipsFromEntity2Scalars
    (ont: OWLOntology)
    : Iterable[ResolvableDataRelationshipsFromEntity2Scalar]
    = for {
      tuple <- dataPropertyDPIRIs
      (e2sc_dp, e2sc_source, e2sc_target) = tuple
      isIdentityCriteria = ont.axioms(e2sc_dp).anyMatch {
        case _: OWLFunctionalDataPropertyAxiom =>
          true
        case _ =>
          false
      }
      domain <- lookup(e2sc_source)
      range <- dataRanges.get(e2sc_target)
    } yield ResolvableDataRelationshipsFromEntity2Scalar(domain, e2sc_dp, isIdentityCriteria, range, tuple)

    def resolvableEntityScalarSubDataPropertyAxioms
    : Iterable[ResolvableEntityScaladSubDataPropertyAxiom]
    = for {
      tuple <- subDataPropertyAxioms
      sub <- dataRelationshipsFromEntityToScalar.get(tuple._1)
      sup <- dataRelationshipsFromEntityToScalar.get(tuple._2)
    } yield ResolvableEntityScaladSubDataPropertyAxiom(sub, sup, tuple)

    def lookupEntityScalarDataPropertyParticularRestriction
    (ax: OWLSubClassOfAxiom)
    : Option[ResolvableEntityScalarDataPropertyParticularRestrictionAxiom]
    = ax.getSuperClass match {
      case hax: OWLDataHasValue =>
        (TerminologyBoxResolverHelper
          .owlclassOfCE(Option.apply(ax.getSubClass))
          .flatMap(lookup),
          TerminologyBoxResolverHelper
            .owlDataPropertyOfPE(Option.apply(hax.getProperty))
            .flatMap(dataRelationshipsFromEntityToScalar.get),
          Option.apply(hax.getFiller)) match {
          case (Some(e), Some(dp), Some(literal)) =>
            Option.apply(literal.getDatatype) match {
              case Some(dt) =>
                dataRanges.get(dt) match {
                  case Some(valueType) =>
                    val literalValue = tables.LiteralValue(tables.LiteralStringType, literal.getLiteral)
                    Some(ResolvableEntityScalarDataPropertyParticularRestrictionAxiom(ax, e, dp, literalValue, Some(valueType)))
                  case None =>
                    None
                }
              case None =>
                val literalValue = tables.LiteralValue(tables.LiteralStringType, literal.getLiteral)
                Some(ResolvableEntityScalarDataPropertyParticularRestrictionAxiom(ax, e, dp, literalValue, None))
            }
          case _ =>
            None
        }
      case _ =>
        None
    }

    def resolvableEntityScalarDataPropertyParticularRestrictionAxioms
    : Iterable[ResolvableEntityScalarDataPropertyParticularRestrictionAxiom]
    = for {
      tuple <- subClassAxioms
      (_, axs) = tuple
      tuple <- axs.flatMap(lookupEntityScalarDataPropertyParticularRestriction)
    } yield tuple

    def lookupEntityScalarDataPropertyUniversalRestriction
    (ax: OWLSubClassOfAxiom)
    : Option[ResolvableEntityScalarDataPropertyDataRangeRestrictionAxiom]
    = ax.getSuperClass match {
      case uax: OWLDataAllValuesFrom =>
        (TerminologyBoxResolverHelper
          .owlclassOfCE(Option.apply(ax.getSubClass))
          .flatMap(lookup),
          TerminologyBoxResolverHelper
            .owlDataPropertyOfPE(Option.apply(uax.getProperty))
            .flatMap(dataRelationshipsFromEntityToScalar.get),
          Option.apply(uax.getFiller).flatMap(dataRanges.get)) match {
          case (Some(e), Some(dp), Some(dr)) =>
            Some(ResolvableEntityScalarDataPropertyDataRangeRestrictionAxiom(ax, e, dp, dr))
          case _ =>
            None
        }
      case _ =>
        None
    }

    def resolvableEntityScalarDataPropertyUniversalRestrictionAxioms
    : Iterable[ResolvableEntityScalarDataPropertyDataRangeRestrictionAxiom]
    = for {
      tuple <- subClassAxioms
      (_, axs) = tuple
      tuple <- axs.flatMap(lookupEntityScalarDataPropertyUniversalRestriction)
    } yield tuple

    def lookupEntityScalarDataPropertyExistentialRestriction
    (ax: OWLSubClassOfAxiom)
    : Option[ResolvableEntityScalarDataPropertyDataRangeRestrictionAxiom]
    = {
      ax.getSuperClass match {
        case uax: OWLDataSomeValuesFrom =>
          (TerminologyBoxResolverHelper
            .owlclassOfCE(Option.apply(ax.getSubClass))
            .flatMap(lookup),
            TerminologyBoxResolverHelper
              .owlDataPropertyOfPE(Option.apply(uax.getProperty))
              .flatMap(dataRelationshipsFromEntityToScalar.get),
            Option.apply(uax.getFiller).flatMap(dataRanges.get)) match {
            case (Some(e), Some(dp), Some(dr)) =>
              Some(ResolvableEntityScalarDataPropertyDataRangeRestrictionAxiom(ax, e, dp, dr))
            case _ =>
              None
          }
        case _ =>
          None
      }
    }

    def resolvableEntityScalarDataPropertyExistentialRestrictionAxioms
    : Iterable[ResolvableEntityScalarDataPropertyDataRangeRestrictionAxiom]
    = {
      for {
        tuple <- subClassAxioms
        (_, axs) = tuple
        tuple <- axs.flatMap(lookupEntityScalarDataPropertyExistentialRestriction)
      } yield tuple
    }

    def splitWarningsAndErrors()(implicit ops: OWLAPIOMFOps)
    : (IncrementalResolverState, IncrementalResolverState)
    = {
      val (wRCs, eRCs) = RCs.partition { case (iri, owlC) =>
        ops.isBackboneIRI(iri) || ops.isBackboneIRI(owlC.getIRI)
      }

      val (wROPs, eROPs) = ROPs.partition(isROPWarning)
      val (wSourceROPs, eSourceROPs) = sourceROPs.partition(isROPWarning)
      val (wTargetROPs, eTargetROPs) = targetROPs.partition(isROPWarning)
      val (wChains, eChains) = chains.partition(isChainWarning)
      val (wUROPs, eUROPs) = unreifiedObjectPropertyOPs.partition { case (owlCd, owlOP, owlCr) =>
        ops.isBackboneIRI(owlCd.getIRI) ||
          ops.isBackboneIRI(owlOP.getIRI) ||
          ops.isBackboneIRI(owlCr.getIRI)
      }
      val (wcra, ecra) = cardinalityRestrictedAspectCIRIs.partition { case (iri, owlC) =>
        ops.isBackboneIRI(iri) || ops.isBackboneIRI(owlC.getIRI)
      }
      val (wcrc, ecrc) = cardinalityRestrictedConceptCIRIs.partition { case (iri, owlC) =>
        ops.isBackboneIRI(iri) || ops.isBackboneIRI(owlC.getIRI)
      }
      val (wcrr, ecrr) = cardinalityRestrictedReifiedRelationshipCIRIs.partition { case (iri, owlC) =>
        ops.isBackboneIRI(iri) || ops.isBackboneIRI(owlC.getIRI)
      }
      val (wSubC, eSubC) = subClassAxioms.partition { case (owlC, axs) =>
        ops.isBackboneIRI(owlC.getIRI) ||
          axs.exists { ax =>
            ax.getSuperClass match {
              case sup: OWLClass =>
                ops.isBackboneIRI(sup.getIRI)
              case _ =>
                false
            }
          }
      }
      val (wSubOP, eSubOP) = subObjectPropertyAxioms.partition { case (sub, sup) =>
        ops.isBackboneIRI(sub.getIRI) || ops.isBackboneIRI(sup.getIRI)
      }
      val (wSubDP, eSubDP) = subDataPropertyAxioms.partition { case (sub, sup) =>
        ops.isBackboneIRI(sub.getIRI) || ops.isBackboneIRI(sup.getIRI)
      }
      val (wDP, eDP) = dataPropertyDPIRIs.partition(isDOPWarning)

      val w = copy(
        RCs = wRCs,
        ROPs = wROPs,
        sourceROPs = wSourceROPs,
        targetROPs = wTargetROPs,
        chains = wChains,
        unreifiedObjectPropertyOPs = wUROPs,
        cardinalityRestrictedAspectCIRIs = wcra,
        cardinalityRestrictedConceptCIRIs = wcrc,
        cardinalityRestrictedReifiedRelationshipCIRIs = wcrr,
        subClassAxioms = wSubC,
        subObjectPropertyAxioms = wSubOP,
        subDataPropertyAxioms = wSubDP,
        dataPropertyDPIRIs = wDP
      )

      val e = copy(
        RCs = eRCs,
        ROPs = eROPs,
        sourceROPs = eSourceROPs,
        targetROPs = eTargetROPs,
        chains = eChains,
        unreifiedObjectPropertyOPs = eUROPs,
        cardinalityRestrictedAspectCIRIs = ecra,
        cardinalityRestrictedConceptCIRIs = ecrc,
        cardinalityRestrictedReifiedRelationshipCIRIs = ecrr,
        subClassAxioms = eSubC,
        subObjectPropertyAxioms = eSubOP,
        subDataPropertyAxioms = eSubDP,
        dataPropertyDPIRIs = eDP
      )

      w -> e
    }

    override def toString: String = {
      val buff = new scala.collection.mutable.StringBuilder
      buff ++= s"IncrementalResolverState: {\n"
      buff ++= s" entityDefinitions: ${entityDefinitions.size}\n"
      buff ++= s" restrictableRelationships: ${restrictableRelationships.size}\n"
      buff ++= s" // queues (empty when complete)\n"
      buff ++= s" RCs: ${RCs.size}\n"
      RCs.to[Seq].sortBy(_._1.toString).foreach { case (iri, owlC) =>
        buff ++= s" RC: $iri\n"
      }
      buff ++= s" ROPs: ${ROPs.size}\n"
      ROPs.to[Seq].sortBy(_._1.toString).foreach { case (iri, owlOP, owlC_domain, owlC_rage, owlOP_inv) =>
        buff ++= s" ROPs: $iri\n"
      }
      buff ++= s" sourceROPs: ${sourceROPs.size}\n"
      sourceROPs.to[Seq].sortBy(_._1.toString).foreach { case (iri, owlOP, owlC_domain, owlC_rage, owlOP_inv) =>
        buff ++= s" sourceROP: $iri\n"
      }
      buff ++= s" targetROPs: ${targetROPs.size}\n"
      targetROPs.to[Seq].sortBy(_._1.toString).foreach { case (iri, owlOP, owlC_domain, owlC_rage, owlOP_inv) =>
        buff ++= s" targetROP: $iri\n"
      }
      buff ++= s" chains: ${chains.size}\n"
      chains.to[Seq].sortBy(_._1.getIRI.toString).foreach { case (op1, op2, op3) =>
        buff ++= s" chain: op=${op1.getIRI} source=${op2.getIRI} target=${op3.getIRI}\n"
      }
      buff ++= s" unreifiedObjectPropertyOPs: ${unreifiedObjectPropertyOPs.size}\n"
      unreifiedObjectPropertyOPs.to[Seq].sortBy(_._2.getIRI.toString).foreach { case (owlC_domain, owlOP, owlC_range) =>
        buff ++= s" unreifiedObjectPropertyOP: op:${owlOP.getIRI} domain:${owlC_domain.getIRI} range:${owlC_range.getIRI}\n"
      }
      buff ++= s" cardinalityRestrictedAspectCIRIs: ${cardinalityRestrictedAspectCIRIs.size}\n"
      cardinalityRestrictedAspectCIRIs.to[Seq].sortBy(_._1.toString).foreach { case (iri, owlC) =>
        buff ++= s" cardinalityRestrictedAspectCIRI: $iri\n"
      }
      buff ++= s" cardinalityRestrictedConceptCIRIs: ${cardinalityRestrictedConceptCIRIs.size}\n"
      cardinalityRestrictedConceptCIRIs.to[Seq].sortBy(_._1.toString).foreach { case (iri, owlC) =>
        buff ++= s" cardinalityRestrictedConceptCIRI: $iri\n"
      }
      buff ++= s" cardinalityRestrictedReifiedRelationshipCIRIs: ${cardinalityRestrictedReifiedRelationshipCIRIs.size}\n"
      cardinalityRestrictedReifiedRelationshipCIRIs.to[Seq].sortBy(_._1.toString).foreach { case (iri, owlC) =>
        buff ++= s" cardinalityRestrictedReifiedRelationshipCIRI: $iri\n"
      }
      buff ++= s" subClassAxioms: ${subClassAxioms.size}\n"
      subClassAxioms.to[Seq].sortBy(_._1.getIRI.toString).foreach { case (iri, axs) =>
        buff ++= s" subClassAxiom: $iri = ${axs.size} axioms\n"
        axs.to[Seq].sortBy(_.getSuperClass.toString).foreach { ax =>
          buff ++= s" subClassAxiom: ... <= ${ax.getSuperClass}\n"
        }
      }
      buff ++= s" subObjectPropertyAxioms: ${subObjectPropertyAxioms.size}\n"
      subObjectPropertyAxioms.to[Seq].sortBy(_._1.getIRI.toString).foreach { case (sub, sup) =>
        buff ++= s" subObjectPropertyAxiom: ${sub.getIRI} <= ${sup.getIRI}\n"
      }
      buff ++= s" subDataPropertyAxioms: ${subDataPropertyAxioms.size}\n"
      subDataPropertyAxioms.to[Seq].sortBy(_._1.getIRI.toString).foreach { case (sub, sup) =>
        buff ++= s" subDataPropertyAxiom: ${sub.getIRI} <= ${sup.getIRI}\n"
      }
      buff ++= s" dataPropertyDPIRIs: ${dataPropertyDPIRIs.size}\n"
      dataPropertyDPIRIs.to[Seq].sortBy(_._1.getIRI.toString).foreach { case (owlDP, owlC, owlDT) =>
        buff ++= s" dataPropertyDPIRI: dp:${owlDP.getIRI}, C:${owlC.getIRI}, DT:${owlDT.getIRI}\n"
      }
      buff ++= s" // resolved\n"
      buff ++= s" reifiedRelationships: ${reifiedRelationships.size}\n"
      buff ++= s" reifiedRelationshipSources: ${reifiedRelationshipSources.size}\n"
      buff ++= s" reifiedRelationshipTargets: ${reifiedRelationshipTargets.size}\n"
      buff ++= s" forwardProperties: ${forwardProperties.size}\n"
      buff ++= s" inverseProperties: ${inverseProperties.size}\n"
      buff ++= s" unreifiedRelationships: ${unreifiedRelationships.size}\n"
      buff ++= s" cardinalityRestrictedAspects: ${cardinalityRestrictedAspects.size}\n"
      buff ++= s" cardinalityRestrictedConcepts: ${cardinalityRestrictedConcepts.size}\n"
      buff ++= s" cardinalityRestrictedReifiedRelationships: ${cardinalityRestrictedReifiedRelationships.size}\n"
      buff ++= s" reifiedRelationshipRestrictions: ${reifiedRelationshipRestrictions.size}\n"
      buff ++= s" aspectSpecializations: ${aspectSpecializations.size}\n"
      buff ++= s" conceptSpecializations: ${conceptSpecializations.size}\n"
      buff ++= s" reifiedRelationshipSpecializations: ${reifiedRelationshipSpecializations.size}\n"
      buff ++= s" existentialRestrictionAxioms: ${existentialRestrictionAxioms.size}\n"
      buff ++= s" universalRestrictionAxioms: ${universalRestrictionAxioms.size}\n"
      buff ++= s" entityScalarDataPropertyParticularRestrictionAxioms: ${entityScalarDataPropertyParticularRestrictionAxioms.size}\n"
      buff ++= s" entityScalarDataPropertyUniversalRestrictionAxioms: ${entityScalarDataPropertyUniversalRestrictionAxioms.size}\n"
      buff ++= s" entityScalarDataPropertyExistentialRestrictionAxioms: ${entityScalarDataPropertyExistentialRestrictionAxioms.size}\n"
      buff ++= s" rootCharacterizedEntityRelationships: ${rootCharacterizedEntityRelationships.size}\n"
      buff ++= s" subObjectProperties: ${subObjectProperties.size}\n"
      buff ++= s" subEntityScalarDataProperties: ${subEntityScalarDataProperties.size}\n"
      buff ++= "}"
      buff.toString()
    }
  }
}

class TerminologyBoxResolverHelper(val tboxG: MutableTerminologyBox,
                                   val ont: OWLOntology,
                                   val omfStore: OWLAPIOMFGraphStore,
                                   val om: OntologyMapping,
                                   val ontOps: OWLOntologyOps) {

  import TerminologyBoxResolverHelper._

  require(null != ont)
  require(null != omfStore)

  import omfStore.ops._

  val LOG: Boolean = "true" equalsIgnoreCase java.lang.System
    .getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ResolverHelper1")

  val LOG1: Boolean = "true" equalsIgnoreCase java.lang.System
    .getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.ResolverHelper2")

  implicit val store = omfStore

  val getOntologyIRI: IRI = tboxG.iri

  val importClosure: Set[TerminologyBox] =
    omfStore.terminologyClosureCache.get(tboxG)

  val provenance = s"load($getOntologyIRI)"

  // ------

  def resolveDataPropertyDPIRIs
  (subDPs: NodeSet[OWLDataProperty],
   tDPs: Set[OWLDataProperty])
  (implicit reasoner: OWLReasoner)
  : Set[DOPInfo]
  = for {
      dataPropertyN <- subDPs.to[Set]
      dataPropertyDP <- dataPropertyN flatMap { dp: OWLDataProperty =>
        Some(dp)
      }
      if tDPs.contains(dataPropertyDP)
      dataPropertyDomain <- reasoner
        .getDataPropertyDomains(dataPropertyDP, true)
        .entities()
        .toScala[Set]
      dataPropertyRange <- tboxG.ont
        .dataPropertyRangeAxioms(dataPropertyDP)
        .toScala[Set]
      dataPropertyType = dataPropertyRange.getRange.asOWLDatatype
    } yield (dataPropertyDP, dataPropertyDomain, dataPropertyType)

  // ----------

  def ropInfoToString(ropInfo: ROPInfo): String = s"""|ROPInfo
        |iri=${ropInfo._1}
        |obj. prop=${ropInfo._2}
        |domain=${ropInfo._3}
        |range=${ropInfo._4}
        |inv o. p.=${ropInfo._5}
        |""".stripMargin('|')

  def chainToString(
      chain: (OWLObjectProperty, OWLObjectProperty, OWLObjectProperty))
    : String = s"""|Chain
        |obj. prop=${chain._1}
        |hasSource=${chain._2}
        |hasTarget=${chain._3}
        |""".stripMargin('|')

  def updateUnreifiedRelationships
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableUOPTuple)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = current.flatMap {
    s =>
      tboxG
        .createEntityUnreifiedRelationship(
          tboxG.uuid,
          r = t.tuple._2,
          source = t.domain,
          target = t.range,
          characteristics = t.characteristics
        )
        .map { ur =>
          s.copy(
            unreifiedObjectPropertyOPs = s.unreifiedObjectPropertyOPs - t.tuple,
            unreifiedRelationships = s.unreifiedRelationships + (t.tuple._2 -> ur)
          )
        }
  }

  def updateReifiedRelationships
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableROPTuple)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = current.flatMap {
    s =>
      tboxG
        .createEntityReifiedRelationship(
          tboxG.uuid,
          r = t.rc,
          u = t.rop._2,
          ui = t.rop._5,
          source = t.r_sourceDef,
          rSource = t.chain._2,
          target = t.r_targetDef,
          rTarget = t.chain._3,
          characteristics = t.characteristics
        )
        .map { rr =>
          s.copy(
            RCs = s.RCs - t.rc.getIRI,
            ROPs = s.ROPs - t.rop,
            sourceROPs = s.sourceROPs - t.sourceRop,
            targetROPs = s.targetROPs - t.targetRop,
            chains = s.chains - t.chain,
            reifiedRelationships = s.reifiedRelationships + (t.rc -> rr),
            reifiedRelationshipSources = s.reifiedRelationshipSources + (rr.rSource -> rr),
            reifiedRelationshipTargets = s.reifiedRelationshipTargets + (rr.rTarget -> rr),
            forwardProperties = s.forwardProperties + (rr.forwardProperty.e -> rr.forwardProperty),
            inverseProperties = s.inverseProperties ++ rr.inverseProperty.map { inv => inv.e -> inv },
            rootCharacterizedEntityRelationships = s.rootCharacterizedEntityRelationships + (rr -> rr
              .rootCharacterizedEntityRelationships())
          )
        }
  }

  def updateCardinalityRestrictedAspects
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableCardinalityRestriction[AspectKind])
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = current.flatMap {
    s =>
      tboxG
        .createCardinalityRestrictedAspect(tboxG.uuid,
                                           t.sub,
                                           t.restrictionKind,
                                           t.restrictedRelationship,
                                           t.restrictedRange,
                                           t.restrictedCardinality)
        .map { a =>
          s.copy(
            cardinalityRestrictedAspectCIRIs = s.cardinalityRestrictedAspectCIRIs - t.sub.getIRI,
            cardinalityRestrictedAspects = s.cardinalityRestrictedAspects + (t.sub -> a)
          )
        }
  }

  def updateCardinalityRestrictedConcepts
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableCardinalityRestriction[ConceptKind])
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = current.flatMap {
    s =>
      tboxG
        .createCardinalityRestrictedConcept(tboxG.uuid,
                                            t.sub,
                                            t.restrictionKind,
                                            t.restrictedRelationship,
                                            t.restrictedRange,
                                            t.restrictedCardinality)
        .map { c =>
          s.copy(
            cardinalityRestrictedConceptCIRIs = s.cardinalityRestrictedConceptCIRIs - t.sub.getIRI,
            cardinalityRestrictedConcepts = s.cardinalityRestrictedConcepts + (t.sub -> c)
          )
        }
  }

  def updateCardinalityRestrictedReifiedRelationships
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableCardinalityRestriction[ConceptualRelationship])
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = current.flatMap {
    s =>
      tboxG
        .createCardinalityRestrictedReifiedRelationship(
          tboxG.uuid,
          t.sub,
          t.restrictionKind,
          t.restrictedRelationship,
          t.restrictedRange,
          t.restrictedCardinality)
        .map { rr =>
          s.copy(
            cardinalityRestrictedReifiedRelationshipCIRIs = s.cardinalityRestrictedReifiedRelationshipCIRIs - t.sub.getIRI,
            cardinalityRestrictedReifiedRelationships = s.cardinalityRestrictedReifiedRelationships + (t.sub -> rr),
            rootCharacterizedEntityRelationships = s.rootCharacterizedEntityRelationships + (rr -> rr
              .rootCharacterizedEntityRelationships())
          )
        }
  }

  def updateAspectSubClassAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableAspectSpecialization)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
      s <- current
      uuid <- aspectSpecializationAxiomUUID(tboxG, t.sub, t.sup)
      ax <- tboxG.createEntityDefinitionAspectSubClassAxiom(uuid, t.sub, t.sup)
      next = s.removeSubClassAxioms(t.c, Set(t.ax)).copy(
        aspectSpecializations = s.aspectSpecializations + (t.ax -> ax)
      )
    } yield next

  def updateConceptSubClassAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableConceptSpecialization)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
      s <- current
      uuid <- conceptSpecializationAxiomUUID(tboxG, t.sub, t.sup)
      ax <- tboxG.createEntityConceptSubClassAxiom(uuid, t.sub, t.sup)
      next = s.removeSubClassAxioms(t.c, Set(t.ax)).copy(
        conceptSpecializations = s.conceptSpecializations + (t.ax -> ax)
      )
    } yield next

  def updateReifiedRelationshipRestrictionSubClassAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableReifiedRelationshipRestrictionSpecialization)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    rrr <- tboxG.createReifiedRelationshipRestriction(tboxG.uuid, t.c, t.source, t.target)
    s <- t.specializations.foldLeft[Set[java.lang.Throwable] \/ IncrementalResolverState](current)(updateReifiedRelationshipRestrictionSubClassAxioms(t.c, rrr))
    next = s.copy(RCs = s.RCs - t.iri)
  } yield next

  def updateReifiedRelationshipRestrictionSubClassAxioms
  (c: OWLClass, rrr: ReifiedRelationshipRestriction)
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   specialization: (ConceptualRelationship, Set[OWLSubClassOfAxiom]))
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    s <- current
    uuid <- reifiedRelationshipSpecializationAxiomUUID(tboxG, rrr, specialization._1)
    ax <- tboxG.createReifiedRelationshipSpecializationAxiom(uuid, rrr, specialization._1)
    next = s.removeSubClassAxioms(c, specialization._2).copy(
      reifiedRelationshipSpecializations = s.reifiedRelationshipSpecializations ++ specialization._2.map(_ -> ax)
    )
  } yield next

  def updateReifiedRelationshipSubClassAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableConceptualRelationshipSpecialization)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
      s <- current
      uuid <- reifiedRelationshipSpecializationAxiomUUID(tboxG, t.sub, t.sup)
      ax <- tboxG.createReifiedRelationshipSpecializationAxiom(uuid, t.sub, t.sup)
      next = s.removeSubClassAxioms(t.c, t.axs).copy(
        subObjectPropertyAxioms = s.subObjectPropertyAxioms -- t.oxs,
        reifiedRelationshipSpecializations = s.reifiedRelationshipSpecializations ++ t.axs.map(_ -> ax)
      )
    } yield next

  def updateUnreifiedRelationshipSpecializationAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableUnreifiedRelationshipSpecialization)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    s <- current
    uuid <- subObjectPropertyOfAxiomUUID(tboxG, t.sub, t.sup)
    ax <- tboxG.createSubObjectPropertyOfAxiom(uuid,
      t.sub,
      t.sup)
    next = s.copy(
      subObjectPropertyAxioms = s.subObjectPropertyAxioms - t.tuple,
      subObjectProperties = s.subObjectProperties + (t.tuple -> ax)
    )
  } yield next

  def updateUniversalEntityRestrictionAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableEntityRestriction)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    s <- current
    uuid <- entityUniversalRestrictionAxiomUUID(tboxG, t.restrictedDomain, t.restrictedRelationship, t.restrictedRange)
    ax <- tboxG.createEntityDefinitionUniversalRestrictionAxiom(uuid,
      t.restrictedDomain,
      t.restrictedRelationship,
      t.restrictedRange)
    next = s.removeSubClassAxioms(t.restrictedDomain.e, Set(t.ax)).copy(
      universalRestrictionAxioms = s.universalRestrictionAxioms + (t.ax -> ax)
    )
  } yield next

  def updateExistentialEntityRestrictionAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableEntityRestriction)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    s <- current
    uuid <- entityExistentialRestrictionAxiomUUID(tboxG, t.restrictedDomain, t.restrictedRelationship, t.restrictedRange)
    ax <- tboxG.createEntityDefinitionExistentialRestrictionAxiom(uuid,
      t.restrictedDomain,
      t.restrictedRelationship,
      t.restrictedRange)
    next = s.removeSubClassAxioms(t.restrictedDomain.e, Set(t.ax)).copy(
      existentialRestrictionAxioms = s.existentialRestrictionAxioms + (t.ax -> ax)
    )
  } yield next

  def updateDataRelationshipsFromEntity2Scalars
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableDataRelationshipsFromEntity2Scalar)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    s <- current
    ax <- tboxG
      .createDataRelationshipFromEntityToScalar(
        tboxG.uuid,
        t.rel,
        t.isIdentityCriteria,
        t.domain,
        t.range)
    next = s.copy(
      dataPropertyDPIRIs = s.dataPropertyDPIRIs - t.tuple,
      dataRelationshipsFromEntityToScalar = s.dataRelationshipsFromEntityToScalar + (t.rel -> ax)
    )
  } yield next

  def updateEntityScalarSubDataPropertyAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableEntityScaladSubDataPropertyAxiom)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    s <- current
    uuid <- subDataPropertyOfAxiomUUID(tboxG, t.sub, t.sup)
    ax <- tboxG.createSubDataPropertyOfAxiom(uuid, t.sub, t.sup)
    next = s.copy(
      subDataPropertyAxioms = s.subDataPropertyAxioms - t.tuple,
      subEntityScalarDataProperties = s.subEntityScalarDataProperties + (t.tuple -> ax)
    )
  } yield next

  def updateEntityScalarDataPropertyParticularRestrictionAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableEntityScalarDataPropertyParticularRestrictionAxiom)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    s <- current
    uuid <- entityScalarDataPropertyParticularRestrictionAxiomUUID(tboxG, t.e, t.dp, t.literalValue)
    ax <- tboxG.createEntityScalarDataPropertyParticularRestrictionAxiom(uuid, t.e, t.dp, t.literalValue, t.valueType)
    next = s.removeSubClassAxioms(t.e.e, Set(t.ax)).copy(
      entityScalarDataPropertyParticularRestrictionAxioms = s.entityScalarDataPropertyParticularRestrictionAxioms + (t.ax -> ax)
    )
  } yield next

  def updateEntityScalarDataPropertyUniversalRestrictionAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableEntityScalarDataPropertyDataRangeRestrictionAxiom)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    s <- current
    uuid <- entityScalarDataPropertyUniversalRestrictionAxiomUUID(tboxG, t.e, t.dp, t.dr)
    ax <- tboxG.createEntityScalarDataPropertyUniversalRestrictionAxiom(uuid, t.e, t.dp, t.dr)
    next = s.removeSubClassAxioms(t.e.e, Set(t.ax)).copy(
      entityScalarDataPropertyUniversalRestrictionAxioms = s.entityScalarDataPropertyUniversalRestrictionAxioms + (t.ax -> ax)
    )
  } yield next

  def updateEntityScalarDataPropertyExistentialRestrictionAxioms
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   t: ResolvableEntityScalarDataPropertyDataRangeRestrictionAxiom)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    s <- current
    uuid <- entityScalarDataPropertyExistentialRestrictionAxiomUUID(tboxG, t.e, t.dp, t.dr)
    ax <- tboxG.createEntityScalarDataPropertyExistentialRestrictionAxiom(uuid, t.e, t.dp, t.dr)
    next = s.removeSubClassAxioms(t.e.e, Set(t.ax)).copy(
      entityScalarDataPropertyExistentialRestrictionAxioms = s.entityScalarDataPropertyExistentialRestrictionAxioms + (t.ax -> ax)
    )
  } yield next
  
  // @TODO need a similar process for resolveDataRelationshipsFromEntity2Structures

  @scala.annotation.tailrec
  final def resolveEntityDefinitionsForRelationshipsRestrictionsAndSpecializations
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = current match {
    case \/-(s0) =>
      if (s0.isResolved)
        \/-(s0)
      else {
        import s0._

        val r1 = resolvableUOPs(ont)
        val r2 = resolvableROPs(ont)
        val r3 = resolvableCardinalityRestrictedAspects(ont)
        val r4 = resolvableCardinalityRestrictedConcepts(ont)
        val r5 = resolvableCardinalityRestrictedReifiedRelationships(ont)
        val r6 = resolvableAspectSubClassAxioms
        val r7 = resolvableConceptSubClassAxioms
        val r8 = resolvableReifiedRelationshipRestrictionSubClassAxioms
        val r9 = resolvableConceptualRelationshipSubClassAxioms(ont)
        val rA = resolvableUnreifiedRelationshipSpecializationAxioms
        val rB = resolvableExistentialEntityRestrictionAxioms
        val rC = resolvableUniversalEntityRestrictionAxioms
        val rD = resolvableDataRelationshipsFromEntity2Scalars(ont)
        val rE = resolvableEntityScalarSubDataPropertyAxioms
        val rF = resolvableEntityScalarDataPropertyParticularRestrictionAxioms
        val rG = resolvableEntityScalarDataPropertyUniversalRestrictionAxioms
        val rH = resolvableEntityScalarDataPropertyExistentialRestrictionAxioms

        val s1 = r1.foldLeft(s0.right[Throwables])(updateUnreifiedRelationships)
        val s2 = r2.foldLeft(s1)(updateReifiedRelationships)
        val s3 = r3.foldLeft(s2)(updateCardinalityRestrictedAspects)
        val s4 = r4.foldLeft(s3)(updateCardinalityRestrictedConcepts)
        val s5 = r5.foldLeft(s4)(updateCardinalityRestrictedReifiedRelationships)
        val s6 = r6.foldLeft(s5)(updateAspectSubClassAxioms)
        val s7 = r7.foldLeft(s6)(updateConceptSubClassAxioms)
        val s8 = r8.foldLeft(s7)(updateReifiedRelationshipRestrictionSubClassAxioms)
        val s9 = r9.foldLeft(s8)(updateReifiedRelationshipSubClassAxioms)
        val sA = rA.foldLeft(s9)(updateUnreifiedRelationshipSpecializationAxioms)
        val sB = rB.foldLeft(sA)(updateExistentialEntityRestrictionAxioms)
        val sC = rC.foldLeft(sB)(updateUniversalEntityRestrictionAxioms)
        val sD = rD.foldLeft(sC)(updateDataRelationshipsFromEntity2Scalars)
        val sE = rE.foldLeft(sD)(updateEntityScalarSubDataPropertyAxioms)
        val sF = rF.foldLeft(sE)(updateEntityScalarDataPropertyParticularRestrictionAxioms)
        val sG = rG.foldLeft(sF)(updateEntityScalarDataPropertyUniversalRestrictionAxioms)
        val sH = rH.foldLeft(sG)(updateEntityScalarDataPropertyExistentialRestrictionAxioms)

        val more =
          r1.nonEmpty || r2.nonEmpty || r3.nonEmpty ||
          r4.nonEmpty || r5.nonEmpty || r6.nonEmpty ||
          r7.nonEmpty || r8.nonEmpty || r9.nonEmpty ||
          rA.nonEmpty || rB.nonEmpty || rC.nonEmpty ||
          rD.nonEmpty || rE.nonEmpty || rF.nonEmpty ||
          rG.nonEmpty || rH.nonEmpty

        if (more) {
          sH match {
            case \/-(h) =>
              val (sW, sE) = h.splitWarningsAndErrors()(omfStore.ops)
              if (!sW.isResolved) {
                val message =
                  s"resolveEntityDefinitionsForRelationshipsRestrictionsAndSpecializations: Warning: ignoring unresolved backbone statements!\n$sW"
                System.out.println(message)
              }
              resolveEntityDefinitionsForRelationshipsRestrictionsAndSpecializations(sE.right)
            case -\/(errors) =>
              errors.left
          }
        } else {
          val (sW, sE) = s0.splitWarningsAndErrors()(omfStore.ops)
          if (sE.isResolved) {
            val message =
              s"resolveEntityDefinitionsForRelationshipsRestrictionsAndSpecializations: Warning: ignoring unresolved backbone statements!\n$sW"
            System.out.println(message)
            resolveEntityDefinitionsForRelationshipsRestrictionsAndSpecializations(sE.right)

          } else {
            val message =
              s"resolveEntityDefinitionsForRelationshipsRestrictionsAndSpecializations: Incomplete resolution!\n$s0"
            Set(
              OMFError.omfOpsError(omfStore.ops, message)
            ).left
          }
        }
      }
    case -\/(errors) =>
      -\/(errors)
  }

  // ---------

  def resolveThingCIRIs
  (thingSubClasses: NodeSet[OWLClass],
   tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      thingN <- thingSubClasses
      if !thingN.isBottomNode
      thingC = thingN.getRepresentativeElement
      if tCs.contains(thingC)
      thingIRI = thingC.getIRI
    } yield thingIRI -> thingC).toMap

  def resolveAspectKindCIRIs
  (entitySubClasses: NodeSet[OWLClass],
   tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      aspectN <- entitySubClasses
      if !aspectN.isBottomNode
      aspectC = aspectN.getRepresentativeElement
      if tCs.contains(aspectC)
      aspectIRI = aspectC.getIRI
    } yield aspectIRI -> aspectC).toMap

  def resolveConceptCIRIs
  (entitySubClasses: NodeSet[OWLClass],
   tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      conceptN <- entitySubClasses
      if !conceptN.isBottomNode
      conceptC = conceptN.getRepresentativeElement
      if tCs.contains(conceptC)
      conceptIRI = conceptC.getIRI
    } yield conceptIRI -> conceptC).toMap

  def resolveConceptRelationshipCIRIs
  (conceptRelationshipKindSubClasses: NodeSet[OWLClass],
   tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      conceptRelationshipN <- conceptRelationshipKindSubClasses
      if !conceptRelationshipN.isBottomNode
      conceptRelationshipC = conceptRelationshipN.getRepresentativeElement
      if tCs.contains(conceptRelationshipC)
      conceptRelationshipCIRI = conceptRelationshipC.getIRI
    } yield conceptRelationshipCIRI -> conceptRelationshipC).toMap

  def resolveReifiedStructuredDataPropertyCIRIs
  (reifiedStructuredDataPropertySubClasses: NodeSet[OWLClass],
   tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      reifiedStructuredDataPropertyN <- reifiedStructuredDataPropertySubClasses
      if !reifiedStructuredDataPropertyN.isBottomNode
      reifiedStructuredDataPropertyC = reifiedStructuredDataPropertyN.getRepresentativeElement
      if tCs.contains(reifiedStructuredDataPropertyC)
      reifiedStructuredDataPropertyCIRI = reifiedStructuredDataPropertyC.getIRI
    } yield
      reifiedStructuredDataPropertyCIRI -> reifiedStructuredDataPropertyC).toMap

  def resolveStructuredDatatypeCIRIs
  (structuredDatatypeSubClasses: NodeSet[OWLClass],
   tCs: Set[OWLClass])
  : Map[IRI, OWLClass]
  = (for {
      structuredDatatypeN <- structuredDatatypeSubClasses
      if !structuredDatatypeN.isBottomNode
      structuredDatatypeC = structuredDatatypeN.getRepresentativeElement
      if tCs.contains(structuredDatatypeC)
      structuredDatatypeCIRI = structuredDatatypeC.getIRI
    } yield structuredDatatypeCIRI -> structuredDatatypeC).toMap

  def resolveDomainRangeForObjectProperties
  (subOPs: NodeSet[OWLObjectPropertyExpression],
   tOPs: Set[OWLObjectProperty],
   ignore: Set[OWLObjectProperty] = Set.empty)
  (implicit reasoner: OWLReasoner)
  : Set[ROPInfo]
  = (for {
      _n_ <- subOPs
      _op_ <- _n_ flatMap {
        case op: OWLObjectProperty =>
          if (!ignore.contains(op) &&
              tOPs.contains(op) &&
              !isAnnotatedDerived(tboxG.ont, op.getIRI).fold(_ => false,
                                                             identity))
            Some(op)
          else
            None
        case inv: OWLObjectInverseOf =>
          inv.getInverse match {
            case op: OWLObjectProperty =>
              if (!ignore.contains(op) &&
                  tOPs.contains(op) &&
                  !isAnnotatedDerived(tboxG.ont, op.getIRI).fold(_ => false,
                                                                 identity))
                Some(op)
              else
                None
            case _ =>
              None
          }
      }
      _d_ <- reasoner
        .getObjectPropertyDomains(_op_, true)
        .entities()
        .toScala[Set]
      _r_ <- reasoner
        .getObjectPropertyRanges(_op_, true)
        .entities()
        .toScala[Set]
    } yield {
      val INV = (_n_ flatMap {
        case op: OWLObjectProperty =>
          if (tOPs.contains(op) && isAnnotatedDerived(tboxG.ont, op.getIRI)
                .fold(_ => false, identity))
            Some(op)
          else
            None
        case inv: OWLObjectInverseOf =>
          inv.getInverse match {
            case op: OWLObjectProperty =>
              if (tOPs.contains(op) && isAnnotatedDerived(tboxG.ont, op.getIRI)
                    .fold(_ => false, identity))
                Some(op)
              else
                None
            case _ =>
              None
          }
      }).headOption
      (_op_.getIRI, _op_, _d_, _r_, INV)
    }).toSet

  def owlDataPropertyOfPE
  (de: Option[OWLDataPropertyExpression])
  : Option[OWLDataProperty]
  = de match {
      case Some(dp: OWLDataProperty) =>
        Some(dp)
      case _ =>
        None
    }

  @scala.annotation.tailrec
  protected final def reorderAtoms
  (v1: SWRLIArgument,
   cs: Set[SWRLClassAtom],
   ps: Set[SWRLObjectPropertyAtom],
   v2: SWRLIArgument,
   as: Seq[SWRLAtom])
  : Option[Seq[SWRLAtom]]
  = if (v1 == v2 && cs.isEmpty && ps.isEmpty)
      Some(as)
    else
      cs.find(_.getArgument == v1) match {
        case Some(a) =>
          reorderAtoms(v1, cs - a, ps, v2, as :+ a)
        case None =>
          ps.find(_.getFirstArgument == v1) match {
            case Some(a) =>
              reorderAtoms(a.getSecondArgument, cs, ps - a, v2, as :+ a)
            case None =>
              None
          }
      }

  protected def resolveRule
  (hs: List[SWRLAtom],
   bs: Seq[SWRLAtom],
   vs: Seq[SWRLVariable])
  : Option[(SWRLAtom, Seq[SWRLAtom])]
  = hs match {
    case (rh: SWRLObjectPropertyAtom) :: Nil =>
      val (cs: Set[SWRLClassAtom],
           ps: Set[SWRLObjectPropertyAtom],
           os: Seq[SWRLAtom]) =
        bs.foldLeft(
            (Set.empty[SWRLClassAtom],
             Set.empty[SWRLObjectPropertyAtom],
             Seq.empty[SWRLAtom])) {
            case ((ci, pi, oi), c: SWRLClassAtom) =>
              (ci + c, pi, oi)
            case ((ci, pi, oi), p: SWRLObjectPropertyAtom) =>
              (ci, pi + p, oi)
            case ((ci, pi, oi), o) =>
              (ci, pi, oi :+ o)
          }
      if (os.nonEmpty)
        None
      else {
        reorderAtoms(rh.getFirstArgument,
                     cs,
                     ps,
                     rh.getSecondArgument,
                     Seq.empty).map(rh -> _)
      }
    case _ =>
      None
  }

  def resolveImplicationRule
  (current: Set[java.lang.Throwable] \/ IncrementalResolverState,
   r: SWRLRule)
  : Set[java.lang.Throwable] \/ IncrementalResolverState
  = for {
    s <- current
    head: List[SWRLAtom] = r.head.toScala[List]
    body: Seq[SWRLAtom] = r.body().toScala[Seq]
    as: Seq[OWLAnnotation] = r
      .annotations(ontOps.df.getRDFSLabel())
      .toScala[Seq]
    vs: Seq[SWRLVariable] = r.variables().toScala[Seq]

    _ <- if (1 != head.size || body.isEmpty || as.size > 1)
      -\/(Set[java.lang.Throwable](OMFError.omfError(
          s"An OML chain rule in OWL2-DL + SWRL must have exactly 1 head (got: ${head.size}), " +
            s"at least one body (got ${body.size}) and exactly one rdfs:label annotation (got ${as.size}):\n$r")))
    else
      \/-(())

    tuple <- resolveRule(head, body, vs) match {
        case Some((h, bs)) =>
          (h -> bs).right[Set[java.lang.Throwable]]
        case None =>
          -\/(Set[java.lang.Throwable](OMFError.omfError(
            s"An OML chain rule in OWL2-DL + SWRL must have exactly 1 head (got: ${head.size}), " +
              s"at least one body (got ${body.size})")))
      }

    (rh, rbs) = tuple

    label <- as.headOption match {
        case Some(a) =>
          a.getValue match {
            case lit: OWLLiteral =>
              \/-(localName(lit.getLiteral))
            case other =>
              -\/(Set[java.lang.Throwable](OMFError.omfError(
                s"An OML chain rule in OWL2-DL + SWRL must have exactly 1 rdfs:label literal annotation, got $other"
              )))
          }
        case None =>
          \/-(localName("R" + scala.util.Random.nextInt(100000).toString))
      }

    chainRule <- rh.getPredicate match {
        case op: OWLObjectProperty =>
          s.unreifiedRelationships.get(op) match {
            case Some(ur) =>
              tboxG.createChainRule(r, label, ur)
            case None =>
              -\/(
                Set[java.lang.Throwable](OMFError.omfError(
                  s"${op.getIRI} is an unrecognized OML UnreifiedRelationship predicate for the head of an An OML chain rule in OWL2-DL + SWRL"
                )))
          }
        case other =>
          -\/(Set[java.lang.Throwable](OMFError.omfError(
            s"An OML chain rule in OWL2-DL + SWRL must have exactly 1 OWLObjectProperty head predicate, got $other"
          )))
      }

    firstSegment = resolveBodyAtom(rbs.head, Some(chainRule), None, s)

    _ <- rbs.tail
      .foldLeft[Set[java.lang.Throwable] \/ RuleBodySegment](firstSegment) {
          case (prev, atom) =>
            for {
              previousSegment <- prev
              nextSegment <- resolveBodyAtom(atom, None, Some(previousSegment), s)
            } yield nextSegment
        }

    } yield s

  def resolveBodyAtom
  (atom: SWRLAtom,
   rule: Option[ChainRule],
   previousSegment: Option[RuleBodySegment],
   s: IncrementalResolverState)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ RuleBodySegment
  = for {
      bodySegment <- tboxG.createRuleBodySegment(rule, previousSegment)
      segmentPredicate <- atom match {
        case a: SWRLClassAtom =>
          a.getPredicate match {
            case cls: OWLClass =>
              s.lookup(cls) match {
                case Some(oA: Aspect) =>
                  tboxG.createSegmentPredicate(bodySegment,
                                               predicate = Some(oA))
                case Some(oC: Concept) =>
                  tboxG.createSegmentPredicate(bodySegment,
                                               predicate = Some(oC))
                case Some(oRR: ReifiedRelationship) =>
                  tboxG.createSegmentPredicate(bodySegment,
                                               predicate = Some(oRR))
                case other =>
                  -\/(Set[java.lang.Throwable](OMFError.omfError(
                    s"Invalid body class atom predicate: $cls as OML: $other")))
              }
            case ce =>
              -\/(
                Set[java.lang.Throwable](OMFError.omfError(
                  s"Unrecognized body class atom predicate: $ce")))
          }
        case a: SWRLObjectPropertyAtom =>
          a.getPredicate match {
            case op: OWLObjectProperty =>
              ( s.lookupRestrictableRelationship(op),
                s.forwardProperties.get(op),
                s.inverseProperties.get(op),
                s.unreifiedRelationships.get(op),
                s.reifiedRelationshipSources.get(op),
                s.reifiedRelationshipTargets.get(op)) match {
                case (Some(rrp), _, _, _, _, _) =>
                  tboxG.createSegmentPredicate(bodySegment, predicate = Some(rrp))
                case (_, Some(fwd), _, _, _, _) =>
                  tboxG.createSegmentPredicate(bodySegment, predicate = Some(fwd))
                case (_, _, Some(inv), _, _, _) =>
                  tboxG.createSegmentPredicate(bodySegment, predicate = Some(inv))
                case (_, _, _, Some(ur), _, _) =>
                  tboxG.createSegmentPredicate(bodySegment, predicate = Some(ur))
                case (_, _, _, _, Some(rr), _) =>
                  tboxG.createSegmentPredicate(bodySegment, reifiedRelationshipSource = Some(rr))
                case (_, _, _, _, _, Some(rr)) =>
                  tboxG.createSegmentPredicate(bodySegment, reifiedRelationshipTarget = Some(rr))
                case _ =>
                  -\/(Set[java.lang.Throwable](OMFError.omfError(
                    s"Unrecognized body object property atom predicate: $op")))
              }
            case oinv: OWLObjectInverseOf =>
              oinv.getInverseProperty match {
                case op: OWLObjectProperty =>
                  ( s.forwardProperties.get(op),
                    s.inverseProperties.get(op),
                    s.unreifiedRelationships.get(op),
                    s.reifiedRelationshipSources.get(op),
                    s.reifiedRelationshipTargets.get(op)) match {
                    case (Some(fwd), _, _, _, _) =>
                      store.reifiedRelationshipOf(fwd) match {
                        case Some(rr) =>
                          rr.inverseProperty match {
                            case Some(inv) =>
                              tboxG.createSegmentPredicate(bodySegment, predicate = Some(inv))
                            case None =>
                              -\/(Set[java.lang.Throwable](OMFError.omfError(
                                s"Unsupported rule refers to the inverse of the forward property of a ReifiedRelationship, ${fwd.abbrevIRI}, that does not define an inverse property! $oinv")))
                          }
                        case None =>
                          -\/(Set[java.lang.Throwable](OMFError.omfError(
                            s"Unsupported rule refers to the inverse of the forward property of a ReifiedRelationship, ${fwd.abbrevIRI}, that does not define an inverse property! $oinv")))
                      }
                    case (_, Some(inv), _, _, _) =>
                      store.reifiedRelationshipOf(inv) match {
                        case Some(rr) =>
                          tboxG.createSegmentPredicate(bodySegment, predicate = Some(rr.forwardProperty))
                        case None =>
                          -\/(Set[java.lang.Throwable](OMFError.omfError(
                            s"Unsupported rule refers to the inverse of the inverse property of a ReifiedRelationship, ${inv.abbrevIRI} that is ill-formed because it has no forward property! $oinv")))
                      }
                    case (_, _, Some(ur), _, _) =>
                      tboxG.createSegmentPredicate(
                        bodySegment,
                        unreifiedRelationshipInverse = Some(ur))
                    case (_, _, _, Some(rr), _) =>
                      tboxG.createSegmentPredicate(
                        bodySegment,
                        reifiedRelationshipInverseSource = Some(rr))
                    case (_, _, _, _, Some(rr)) =>
                      tboxG.createSegmentPredicate(
                        bodySegment,
                        reifiedRelationshipInverseTarget = Some(rr))
                    case _ =>
                      -\/(Set[java.lang.Throwable](OMFError.omfError(
                        s"Unrecognized body inverse object property atom predicate: $oinv")))
                  }
                case ope =>
                  -\/(Set[java.lang.Throwable](OMFError.omfError(
                    s"Unrecognized body inverse object property atom predicate: $ope")))
              }
            case ope =>
              -\/(
                Set[java.lang.Throwable](OMFError.omfError(
                  s"Unrecognized body object property atom predicate: $ope")))
          }
        case other =>
          -\/(
            Set[java.lang.Throwable](
              OMFError.omfError(s"Unrecognized body atom predicate: $other")))
      }

      _ = {
        val ns = tboxG.sig.ruleBodySegments.size
        val ps = tboxG.sig.segmentPredicates.size
        if (ns != ps)
          \/-(())
        else
          -\/(
            Set[java.lang.Throwable](OMFError.omfError(
              s"ChainRule construction problem for $bodySegment and $segmentPredicate:\n" +
                s"$ns segments vs. $ps predicates\n"
            )))
      }
    } yield bodySegment

}
