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

import gov.nasa.jpl.imce.oml.tables.LocalName
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader.OntologyLoadedState
import AxiomExceptionKind._
import AxiomScopeAccessKind._
import EntityExceptionKind._
import RelationshipScopeAccessKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{ImmutableTerminologyBox, ImmutableTerminologyGraph, MutableTerminologyBox, MutableTerminologyGraph}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms.TerminologyAxiom
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
  = Set[java.lang.Throwable] \/ Option[(Concept, ImmutableTerminologyBox)]

  val emptyNestingConceptAndGraphNES
  : NestingConceptAndGraphOptionNES
  = \/-(None)

  type MutableTerminologyBoxesNES
  = Set[java.lang.Throwable] \/ Set[MutableTerminologyBox]

  val emptyMutableTerminologyBoxesNES
  : MutableTerminologyBoxesNES
  = \/-(Set())

  type ImmutableTerminologyBoxesNES
  = Set[java.lang.Throwable] \/ Set[ImmutableTerminologyBox]

  val emptyImmutableTerminologyBoxesNES
  : ImmutableTerminologyBoxesNES
  = \/-(Set())

  type Mutable2ImmutableTerminologyMap
  = Map[MutableTerminologyBox, ImmutableTerminologyBox]

  val emptyMutable2ImmutableTerminologyMapNES
  : Set[java.lang.Throwable] \/ types.Mutable2ImmutableTerminologyMap
  = \/-(Map())

  def entityAlreadyDefinedException
  (kind: EntityExceptionKind,
   iri: IRI,
   term: Term)
  : java.lang.Throwable
  = EntityAlreadyDefinedException(kind, iri, term)

  def entityConflictException
  (kind: EntityExceptionKind,
   iri: IRI,
   conflictingTerm: Term)
  : java.lang.Throwable
  = EntityConflictException(kind, iri, conflictingTerm)

  def entityScopeException
  (kind: EntityExceptionKind,
   iri: IRI,
   unaccessibleTerms: Map[RelationshipScopeAccessKind, Term])
  : java.lang.Throwable
  = EntityScopeException(kind, iri, unaccessibleTerms)

  def axiomScopeException
  (kind: AxiomExceptionKind,
   unaccessibleTerms: Map[AxiomScopeAccessKind, Term])
  : java.lang.Throwable
  = AxiomScopeException(kind, unaccessibleTerms)

  def duplicateModelTermAxiomException
  (kind: AxiomExceptionKind,
   axiom: Axiom)
  : java.lang.Throwable
  = DuplicateModelTermAxiomException(kind, axiom)

  def duplicateTerminologyGraphAxiomException
  (kind: AxiomExceptionKind,
   axiom: TerminologyAxiom)
  : java.lang.Throwable
  = DuplicateTerminologyGraphAxiomException(kind, axiom)

  def createOMFTerminologyGraph
  (ont: OWLOntology,
   relativeIRIPath: Option[String],
   relativeIRIHashPrefix: Option[String],
   kind: TerminologyKind,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.terminologies.MutableTerminologyGraph
  = store.createOMFTerminologyGraph(
    ont.getOntologyID.getOntologyIRI.get,
    relativeIRIPath, relativeIRIHashPrefix, ont, kind, extraProvenanceMetadata)

  def mutableModelTerminologyGraphResolver
  (omfMetadata: Option[OWLOntology],
   s: OntologyLoadedState,
   ont: OWLOntology,
   mgraphs: Set[MutableTerminologyGraph],
   omfStore: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ (MutableTerminologyGraph, Set[MutableTerminologyGraph])
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

    createOMFTerminologyGraph(
        ont = ont,
        relativeIRIPath = getOntologyRelativeIRI,
        relativeIRIHashPrefix = getOntologyIRIHashPrefix,
        kind = kind,
        extraProvenanceMetadata = OTI2OMFModelTerminologyGraphProvenance.asOMFGraphOntologyProvenance(ont)
      )(omfStore)
      .map { g: MutableTerminologyGraph =>
        (g, mgraphs + g)
      }
  }

  // loading mutable graphs (incl. converting extended mutable graphs => immutable graphs)

  type ModelTerminologyGraphsLoadState =
    (Set[MutableTerminologyBox], Set[MutableTerminologyBox], Mutable2ImmutableTerminologyMap)

  def modelTerminologyGraphsInitialState
  (mgraphs: Set[MutableTerminologyBox])
  : ModelTerminologyGraphsLoadState
  = (mgraphs, Set.empty[MutableTerminologyBox], Map.empty[MutableTerminologyBox, ImmutableTerminologyGraph])

  def loadMutableModelTerminologyGraphResolver
  (omfMetadata: OWLOntology,
   s: OntologyLoadedState,
   mGraph: MutableTerminologyBox,
   extIGraphs: Set[ImmutableTerminologyBox],
   otherMGraphs: Set[MutableTerminologyBox],
   resultMGraphs: Set[MutableTerminologyBox],
   m2i: Mutable2ImmutableTerminologyMap,
   omfStore: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ ModelTerminologyGraphsLoadState
  = {
    implicit val ops = omfStore.ops
    for {
      _ <- extIGraphs.foldLeft {
        ().right[Set[java.lang.Throwable]]
      } { (acc: Set[java.lang.Throwable] \/ Unit,
           extIGraph: ImmutableTerminologyBox) =>

        acc.flatMap { _ =>
          mGraph
            .createTerminologyExtensionAxiom(extendedG = extIGraph)(omfStore)
            .map(_ => ())
        }
      }

      result = (otherMGraphs, resultMGraphs, m2i)
    } yield result
  }

  def immutableModelTerminologyGraphResolver
  (omfMetadata: Option[OWLOntology],
   s: OntologyLoadedState,
   ont: OWLOntology,
   as: Seq[OWLAnnotation],
   extensions: Set[ImmutableTerminologyBox],
   nesting: Option[(Concept, ImmutableTerminologyBox)],
   m2i: Mutable2ImmutableTerminologyMap,
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

    createOMFTerminologyGraph(
        ont = ont,
        relativeIRIPath=getOntologyRelativeIRI,
        relativeIRIHashPrefix=getOntologyIRIHashPrefix,
        kind = kind,
        extraProvenanceMetadata = OTI2OMFModelTerminologyGraphProvenance.asOMFGraphOntologyProvenance(ont)
      )(omfStore)
      .flatMap { g: MutableTerminologyGraph =>

        for {
          _ <- as.foldLeft[types.UnitNES](types.rightUnitNES) { case (acc, a) =>
              for {
                _ <- acc
                _ <- omfStore.ops.addAnnotation(g, g, a)(omfStore)
              } yield ()
          }

          _ <- extensions.foldLeft[types.UnitNES](types.rightUnitNES) { case (acc, importG) =>
               for {
                 _ <- acc
               _ <- g.createTerminologyExtensionAxiom(extendedG=importG)(omfStore)
            } yield ()
          }

          _ <- nesting.fold[types.UnitNES](types.rightUnitNES){ case (nestingC, nestingG) =>
            g
              .createTerminologyNestingAxiom(parentG=nestingG, parentC=nestingC)(omfStore)
              .map(_ => ())
          }

          resolver = ResolverHelper(omfMetadata, g, extensions, ont, omfStore, m2i)
        } yield
          ImmutableModelTerminologyGraphResolver(resolver)
      }
  }

  type ImmutableTerminologyConversionMap =
    (ImmutableTerminologyBox, Mutable2ImmutableTerminologyMap)

  type ModelTerminologyGraphsConversionState = (Set[MutableTerminologyGraph], Mutable2ImmutableTerminologyMap)

  implicit def OWLClass2ModelEntityDefinitionSemigroup
  : Semigroup[Map[OWLClass, Entity]]
  = Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelEntityAspectSemigroup
  : Semigroup[Map[OWLClass, Aspect]]
  = Semigroup.instance(_ ++ _)

  implicit def OWLClass2ModelEntityConceptSemigroup
  : Semigroup[Map[OWLClass, Concept]]
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
    *         - a class that has an object property restriction subclass axiom
    *         - the object property involved in that restriction
    *         - true if the restriction is on the object property; false if it is on the inverse object property
    *         - the restricted range
    *         - the kind of restriction (existential or universal)
    */
  def getObjectPropertyRestrictionsIfAny
  ( ont: OWLOntology, entity: OWLClass )
  : Set[(OWLClass, OWLObjectProperty, Boolean, OWLClass, ObjectRestrictionKind)]
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
                Some(Tuple5(entity, op, isInverse, range, ExistentialObjectRestrictionKind))

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
                Some(Tuple5(entity, op, isInverse, range, UniversalObjectRestrictionKind))

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
                Some(Tuple2(dp, ParticularOWLDataRestrictionKind(restriction.getFiller.getLiteral)))

              case _ =>
                None
            }

          case _ =>
            None
        }
    }
}