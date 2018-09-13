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
import AxiomExceptionKind._
import AxiomScopeAccessKind._
import ElementExceptionKind._
import RelationshipScopeAccessKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{MutableModule, Resource}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.ImmutableTerminologyBox
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms.TerminologyAxiom
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import org.semanticweb.owlapi.model.parameters.{Imports, Navigation}
import org.semanticweb.owlapi.model._

import scala.{Boolean, None, Option, Some, Tuple2, Unit}
import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.Predef.classOf
import scalaz._

package object types {

  type UnitNES
  = Throwables \/ Unit

  val rightUnitNES
  : UnitNES
  = \/-(())

  type NestingConceptAndGraphOptionNES
  = Throwables \/ Option[(Concept, ImmutableTerminologyBox)]

  val emptyNestingConceptAndGraphNES
  : NestingConceptAndGraphOptionNES
  = \/-(None)

  def entityAlreadyDefinedException
  (kind: ElementExceptionKind,
   iri: IRI,
   term: Term)
  : java.lang.Throwable
  = ResouceAlreadyDefinedException(kind, iri, term)

  def entityConflictException
  (kind: ElementExceptionKind,
   iri: IRI,
   conflictingTerm: Term)
  : java.lang.Throwable
  = ResourceConflictException(kind, iri, conflictingTerm)

  def entityScopeException
  (kind: ElementExceptionKind,
   iri: IRI,
   unaccessibleTerms: Map[RelationshipScopeAccessKind, Term])
  : java.lang.Throwable
  = ResourceScopeException(kind, iri, unaccessibleTerms)

  def axiomScopeException
  (kind: AxiomExceptionKind,
   unaccessibleTerms: Map[AxiomScopeAccessKind, Resource])
  : java.lang.Throwable
  = AxiomScopeException(kind, unaccessibleTerms)

  def duplicateModelTermAxiomException
  (kind: AxiomExceptionKind,
   axiom: Axiom)
  : java.lang.Throwable
  = DuplicateTermAxiomException(kind, axiom)

  def duplicateTerminologyGraphAxiomException
  (kind: AxiomExceptionKind,
   axiom: TerminologyAxiom)
  : java.lang.Throwable
  = DuplicateTerminologyBoxAxiomException(kind, axiom)

  // loading mutable graphs (incl. converting extended mutable graphs => immutable graphs)

  type ModuleLoadState =
    (Set[MutableModule], Set[MutableModule], OntologyMapping)

  implicit def OWLClass2ModelEntityDefinitionSemigroup
  : Semigroup[Map[OWLClass, Entity]]
  = Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelEntityAspectSemigroup
  : Semigroup[Map[OWLClass, Aspect]]
  = Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelEntityAspectKindSemigroup
  : Semigroup[Map[OWLClass, AspectKind]]
  = Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelEntityConceptSemigroup
  : Semigroup[Map[OWLClass, Concept]]
  = Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelEntityConceptKindSemigroup
  : Semigroup[Map[OWLClass, ConceptKind]]
  = Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelEntityReifiedRelationshipSemigroup
  : Semigroup[Map[OWLClass, ReifiedRelationship]]
  = Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelStructuredDataTypeSemigroup
  : Semigroup[Map[OWLClass, Structure]]
  = Semigroup.instance(_ ++ _)

  implicit def OWLDatatype2DataRangeSemigroup
  : Semigroup[Map[OWLDatatype, DataRange]]
  = Semigroup.instance(_ ++ _)

  /**
    * Collects the object property restriction subclass axioms for a particular class in an ontology
    * @param ont
    * @param entity
    * @return A tuple of:
    *         - the object property involved in that restriction
    *         - true if the restriction is on the object property; false if it is on the inverse object property
    *         - the restricted range
    *         - the kind of restriction (existential or universal)
    */
  def getObjectPropertyRestrictionsIfAny
  ( ont: OWLOntology, entity: OWLClass )
  : Set[(OWLObjectProperty, Boolean, OWLClass, ObjectRestrictionKind)]
  = ont
    .axioms[OWLSubClassOfAxiom](classOf[OWLSubClassOfAxiom], entity, Imports.EXCLUDED, Navigation.IN_SUB_POSITION)
    .toScala[Set]
    .flatMap { ax =>

      // @todo Check if there is a known bug in the OWL API for getAxioms() w.r.t. the 'forSubPosition' argument.
      // The filter below should be unecessay if forSubPosition == Navigation.IN_SUB_POSITION worked as intended...
      if (ax.getSubClass != entity)
        None

      else
        ax.getSuperClass match {

          case restriction: OWLObjectSomeValuesFrom =>

            val restrictingProperty
            : Option[(OWLObjectProperty, Boolean)]
            = restriction.getProperty match {
              case op: OWLObjectProperty =>
                Some(Tuple2(op, false))

              case inv: OWLObjectInverseOf =>
                inv.getInverse match {
                  case op: OWLObjectProperty =>
                    Some(Tuple2(op, true))
                  case _ =>
                    None
                }

              case _ =>
                None
            }

            val rangeRestriction
            : Option[OWLClass]
            = restriction.getFiller match {
              case c: OWLClass =>
                Some(c)

              case _ =>
                None
            }

            (restrictingProperty, rangeRestriction) match {
              case (Some(Tuple2(op, isInverse)), Some(range)) =>
                Some((op, isInverse, range, ExistentialObjectRestrictionKind))

              case _ =>
                None
            }

          case restriction: OWLObjectAllValuesFrom =>

            val restrictingProperty
            : Option[(OWLObjectProperty, Boolean)]
            = restriction.getProperty match {
              case op: OWLObjectProperty =>
                Some(Tuple2(op, false))

              case inv: OWLObjectInverseOf =>
                inv.getInverse match {
                  case op: OWLObjectProperty =>
                    Some(Tuple2(op, true))
                  case _ =>
                    None
                }

              case _ =>
                None
            }

            val rangeRestriction
            : Option[OWLClass]
            = restriction.getFiller match {
              case c: OWLClass =>
                Some(c)

              case _ =>
                None
            }

            (restrictingProperty, rangeRestriction) match {
              case (Some(Tuple2(op, isInverse)), Some(range)) =>
                Some((op, isInverse, range, UniversalObjectRestrictionKind))

              case _ =>
                None
            }

          case _ =>
            None
        }
    }

  /**
    * Collects the data property restriction subclass axioms for a particular class in an ontology
    * @param ont
    * @param entity
    * @return A tuple of:
    *         - the data property involved in that restriction
    *         - the data type involved in the universal or existensial restriction or none for a particular restriction
    *         - the kind of restriction (existential or universal)
    */

  def getDataPropertyRestrictionsIfAny
  ( ont: OWLOntology, entity: OWLClass )
  : Set[(OWLDataProperty, OWLDataRestrictionKind)]
  = ont
    .axioms[OWLSubClassOfAxiom](classOf[OWLSubClassOfAxiom], entity, Imports.EXCLUDED, Navigation.IN_SUB_POSITION)
    .toScala[Set]
    .flatMap { ax =>

      // @todo Check if there is a known bug in the OWL API for getAxioms() w.r.t. the 'forSubPosition' argument.
      // The filter below should be unecessay if forSubPosition == Navigation.IN_SUB_POSITION worked as intended...
      if (ax.getSubClass != entity)
        None

      else
        ax.getSuperClass match {

          case restriction: OWLDataSomeValuesFrom =>

            val restrictingProperty
            : Option[OWLDataProperty]
            = restriction.getProperty match {
              case dp: OWLDataProperty =>
                Some(dp)

              case _ =>
                None
            }

            val rangeRestriction
            : Option[OWLDatatype]
            = restriction.getFiller match {
              case dt: OWLDatatype =>
                Some(dt)

              case _ =>
                None
            }

            (restrictingProperty, rangeRestriction) match {
              case (Some(dp), Some(range)) =>
                Some(Tuple2(dp, ExistentialOWLDataRestrictionKind(range)))

              case _ =>
                None
            }

          case restriction: OWLDataAllValuesFrom =>

            val restrictingProperty
            : Option[OWLDataProperty]
            = restriction.getProperty match {
              case dp: OWLDataProperty =>
                Some(dp)

              case _ =>
                None
            }

            val rangeRestriction
            : Option[OWLDatatype]
            = restriction.getFiller match {
              case dt: OWLDatatype =>
                Some(dt)

              case _ =>
                None
            }

            (restrictingProperty, rangeRestriction) match {
              case (Some(dp), Some(range)) =>
                Some(Tuple2(dp, UniversalOWLDataRestrictionKind(range)))

              case _ =>
                None
            }

          case restriction: OWLDataHasValue =>

            val restrictingProperty
            : Option[OWLDataProperty]
            = restriction.getProperty match {
              case dp: OWLDataProperty =>
                Some(dp)

              case _ =>
                None
            }

            restrictingProperty match {
              case (Some(dp)) =>
                Some(Tuple2(dp, ParticularOWLDataRestrictionKind(
                  restriction.getFiller.getLiteral,
                  Option.apply(restriction.getFiller.getDatatype))))

              case _ =>
                None
            }

          case _ =>
            None
        }
    }
}