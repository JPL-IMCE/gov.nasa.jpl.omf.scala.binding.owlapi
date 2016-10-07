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

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader.OntologyLoadedState
import gov.nasa.jpl.omf.scala.binding.owlapi.types.AxiomExceptionKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.AxiomScopeAccessKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.EntityExceptionKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.RelationshipScopeAccessKind._
import gov.nasa.jpl.omf.scala.core.OMFError
import gov.nasa.jpl.omf.scala.core.TerminologyKind
import org.semanticweb.owlapi.model.parameters.{Imports, Navigation}
import org.semanticweb.owlapi.model._

import scala.{Boolean, None, Option, Some, StringContext, Tuple2, Tuple5, Unit}
import scala.collection.immutable._
import scala.compat.java8.StreamConverters._
import scala.Predef.{String, classOf}
import scalaz._
import Scalaz._

package object types {

  type UnitNES
  = Set[java.lang.Throwable] \/ Unit

  val rightUnitNES
  : UnitNES
  = \/-(())

  type NestingConceptAndGraphOptionNES
  = Set[java.lang.Throwable] \/ Option[(types.ModelEntityConcept, types.ImmutableModelTerminologyGraph)]

  val emptyNestingConceptAndGraphNES
  : NestingConceptAndGraphOptionNES
  = \/-(None)

  type ImmutableModelTerminologyGraphsNES
  = Set[java.lang.Throwable] \/ Set[types.ImmutableModelTerminologyGraph]

  val emptyImmutableTerminologyGraphsNES
  : ImmutableModelTerminologyGraphsNES
  = \/-(Set())

  type Mutable2IMutableTerminologyMap
  = Map[MutableModelTerminologyGraph, ImmutableModelTerminologyGraph]

  val emptyMutable2ImmutableTerminologyMapNES
  : Set[java.lang.Throwable] \/ types.Mutable2IMutableTerminologyMap
  = \/-(Map())

  def entityAlreadyDefinedException
  (kind: EntityExceptionKind,
   iri: IRI,
   term: ModelTypeTerm)
  : java.lang.Throwable =
    EntityAlreadyDefinedException(kind, iri, term)

  def entityConflictException
  (kind: EntityExceptionKind,
   iri: IRI,
   conflictingTerm: ModelTypeTerm)
  : java.lang.Throwable =
    EntityConflictException(kind, iri, conflictingTerm)

  def entityScopeException
  (kind: EntityExceptionKind,
   iri: IRI,
   unaccessibleTerms: Map[RelationshipScopeAccessKind, ModelTypeTerm])
  : java.lang.Throwable =
    EntityScopeException(kind, iri, unaccessibleTerms)

  def axiomScopeException
  (kind: AxiomExceptionKind,
   unaccessibleTerms: Map[AxiomScopeAccessKind, ModelTypeTerm])
  : java.lang.Throwable =
    AxiomScopeException(kind, unaccessibleTerms)

  def duplicateModelTermAxiomException
  (kind: AxiomExceptionKind,
   axiom: ModelTermAxiom)
  : java.lang.Throwable =
    DuplicateModelTermAxiomException(kind, axiom)

  def immutableModelTerminologyGraphResolver
  (omfMetadata: OWLOntology,
   s: OntologyLoadedState,
   ont: OWLOntology,
   extensions: Set[ImmutableModelTerminologyGraph],
   nesting: Option[(ModelEntityConcept, ImmutableModelTerminologyGraph)],
   m2i: Mutable2IMutableTerminologyMap,
   omfStore: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ImmutableModelTerminologyGraphResolver
  = {
    implicit val ops = omfStore.ops

    val ontOps = new OWLOntologyOps(ont)

    val isTboxDef = ontOps.isOntologyTBoxDefinition
    val kind =
      if (isTboxDef)
        TerminologyKind.isDefinition
      else
        TerminologyKind.isDesignation

    val localName
    : Set[java.lang.Throwable] \/ LocalName
    = ont
        .annotations
        .toScala[Set]
        .find(_.getProperty.getIRI == ops.rdfs_label)
        .fold {
          ops.lastSegment(ont.getOntologyID.getOntologyIRI.get)
        } { l =>
          l.getValue match {
            case l: OWLLiteral =>
              \/-(l.getLiteral)
            case _ =>
              -\/(Set[java.lang.Throwable](OMFError.omfBindingError(s"invalid rdfs:label on OMF terminology graph: ${ont.getOntologyID}")))
          }
        }

    val getOntologyUUID: Option[String] =
      ont
        .annotations
        .toScala[Set]
        .find(_.getProperty.getIRI == ops.AnnotationHasUUID)
        .flatMap(_.getValue match {
          case l: OWLLiteral =>
            Some(l.getLiteral)
          case _ =>
            None
        })

    val getOntologyRelativeIRI: Option[String] =
      ont
        .annotations
        .toScala[Set]
        .find(_.getProperty.getIRI == ops.AnnotationHasRelativeIRI)
        .flatMap(_.getValue match {
          case l: OWLLiteral =>
            Some(l.getLiteral)
          case _ =>
            None
        })

    val getOntologyIRIHashPrefix: Option[String] =
      ont
        .annotations
        .toScala[Set]
        .find(_.getProperty.getIRI == ops.AnnotationHasIRIHashPrefix)
        .flatMap(_.getValue match {
          case l: OWLLiteral =>
            Some(l.getLiteral)
          case _ =>
            None
        })

    val getOntologyIRIHashSuffix: Option[String] =
      ont
        .annotations
        .toScala[Set]
        .find(_.getProperty.getIRI == ops.AnnotationHasIRIHashSuffix)
        .flatMap(_.getValue match {
          case l: OWLLiteral =>
            Some(l.getLiteral)
          case _ =>
            None
        })

    ops
      .createOMFTerminologyGraph(
      o = omfMetadata,
      ont = ont,
      relativeIRIPath=getOntologyRelativeIRI,
      relativeIRIHashPrefix=getOntologyIRIHashPrefix,
      kind = kind,
      extraProvenanceMetadata = OTI2OMFModelTerminologyGraphProvenance.asOMFGraphOntologyProvenance(ont)
    )(omfStore)
    .flatMap { g: MutableModelTerminologyGraph =>

      for {
        _ <- {
          (().right[Set[java.lang.Throwable]] /: extensions) {
            (acc: Set[java.lang.Throwable] \/ Unit, importG: ImmutableModelTerminologyGraph) =>
            acc +++
            omfStore
              .createTerminologyGraphDirectExtensionAxiom(extendingG=g, extendedG=importG)
              .map(_ => ())
          }
        }
        _ <- nesting.fold[Set[java.lang.Throwable] \/ Unit](\/-(())){ case (nestingC, nestingG) =>
          omfStore
            .createTerminologyGraphDirectNestingAxiom(parentG=nestingG, parentC=nestingC, childG=g)
            .map(_ => ())
        }
        resolver = ResolverHelper(omfMetadata, g, extensions, ont, omfStore)
      } yield
        ImmutableModelTerminologyGraphResolver(resolver)
    }


  }

  implicit def OWLClass2ModelEntityDefinitionSemigroup
  : Semigroup[Map[OWLClass, ModelEntityDefinition]] =
    Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelEntityAspectSemigroup
  : Semigroup[Map[OWLClass, ModelEntityAspect]] =
    Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelEntityConceptSemigroup
  : Semigroup[Map[OWLClass, ModelEntityConcept]] =
    Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelEntityReifiedRelationshipSemigroup
  : Semigroup[Map[OWLClass, ModelEntityReifiedRelationship]] =
    Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelStructuredDataTypeSemigroup
  : Semigroup[Map[OWLClass, ModelStructuredDataType]] =
    Semigroup.instance(_ ++ _)

  implicit def OWLDatatype2ModelScalarDataTypeSemigroup
  : Semigroup[Map[OWLDatatype, ModelScalarDataType]] =
    Semigroup.instance(_ ++ _)

  def getSingleDataPropertyRestrictionsIfAny
  ( ont: OWLOntology, entity: OWLClass )
  : Set[java.lang.Throwable] \/ Set[(OWLClass, OWLDataProperty, String)]
  = {
    val restrictions
    : Set[java.lang.Throwable] \/ Set[(OWLClass, OWLDataProperty, String)]
    = ont.subClassAxiomsForSubClass(entity).toScala[Set]
      .map { sup =>
        val restriction
        : Set[java.lang.Throwable] \/ Set[(OWLClass, OWLDataProperty, String)]
        = sup.getSuperClass match {

          case restriction: OWLDataSomeValuesFrom =>

            val restrictingProperty
            : Option[OWLDataProperty]
            = restriction.getProperty match {
              case dp: OWLDataProperty =>
                Some(dp)
              case _ =>
                None
            }

            val literalRestriction
            : Option[String]
            = restriction.getFiller match {
              case oneOf: OWLDataOneOf =>
                val values = oneOf.values.toScala[Set]
                if (1 == values.size)
                  Some(values.head.getLiteral)
                else
                  None
              case _ =>
                None
            }

            (restrictingProperty, literalRestriction) match {
              case (Some(dp), Some(literal)) =>
                \/-(Set((entity, dp, literal)))
              case (_, _) =>
                -\/(Set(OMFError.omfError(s"Ill-formed data property restriction: $restriction}")))
            }

          case _ =>
            \/-(Set())
        }

        restriction
      }
      .fold[Set[java.lang.Throwable] \/ Set[(OWLClass, OWLDataProperty, String)]](\/-(Set())){ _ +++ _ }

    restrictions
  }

  def getObjectPropertyRestrictionsIfAny
  ( ont: OWLOntology, entity: OWLClass )
  : Set[(OWLClass, OWLObjectProperty, Boolean, OWLClass, RestrictionKind)]
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
                Some(Tuple5(entity, op, isInverse, range, ExistentialRestrictionKind))

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
                Some(Tuple5(entity, op, isInverse, range, UniversalRestrictionKind))

              case _ =>
                None
            }

          case _ =>
            None
        }
    }
}