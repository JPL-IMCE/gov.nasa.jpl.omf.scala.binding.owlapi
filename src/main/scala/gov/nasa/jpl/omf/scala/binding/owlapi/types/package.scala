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
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader.OntologyLoadedState
import AxiomExceptionKind._
import AxiomScopeAccessKind._
import ElementExceptionKind._
import RelationshipScopeAccessKind._
import gov.nasa.jpl.imce.oml.tables.AnnotationProperty
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{ImmutableModule, MutableModule}
import gov.nasa.jpl.omf.scala.binding.owlapi.descriptions.{ImmutableDescriptionBox, MutableDescriptionBox}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.{ImmutableTerminologyBox, ImmutableTerminologyGraph, MutableBundle, MutableTerminologyGraph}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms.TerminologyAxiom
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
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
  = EntityAlreadyDefinedException(kind, iri, term)

  def entityConflictException
  (kind: ElementExceptionKind,
   iri: IRI,
   conflictingTerm: Term)
  : java.lang.Throwable
  = EntityConflictException(kind, iri, conflictingTerm)

  def entityScopeException
  (kind: ElementExceptionKind,
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
  = DuplicateTermAxiomException(kind, axiom)

  def duplicateTerminologyGraphAxiomException
  (kind: AxiomExceptionKind,
   axiom: TerminologyAxiom)
  : java.lang.Throwable
  = DuplicateTerminologyBoxAxiomException(kind, axiom)

  // loading mutable graphs (incl. converting extended mutable graphs => immutable graphs)

  type ModuleLoadState =
    (Set[MutableModule], Set[MutableModule], Mutable2ImmutableModuleMap)

  def moduleInitialLoadState
  (ms: Set[MutableModule])
  : ModuleLoadState
  = (ms, Set.empty[MutableModule], emptyMutable2ImmutableModuleMap)

  def immutableModuleResolver
  (omfMetadata: Option[OWLOntology],
   s: OntologyLoadedState,
   ont: OWLOntology,
   as: Set[AnnotationProperty],
   extensions: Set[ImmutableModule],
   nesting: Option[(Concept, ImmutableTerminologyBox)],
   om: OntologyMapping,
   omfStore: OWLAPIOMFGraphStore)
  : Throwables \/ ImmutableResolver
  = {
    implicit val ops = omfStore.ops
    implicit val store = omfStore

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

    val ontOps = new OWLOntologyOps(ont)

    if (ontOps.isBundleOntology) {
      // Bundle
      val isOpen = ontOps.isOpenWorldDefinitionTerminologyBoxOntology
      val kind =
        if (isOpen)
          TerminologyKind.isDefinition
        else
          TerminologyKind.isDesignation

      omfStore.createOMFBundle(
        ont.getOntologyID.getOntologyIRI.get,
        relativeIRIPath = getOntologyRelativeIRI,
        relativeIRIHashPrefix = getOntologyIRIHashPrefix,
        ont, kind,
        extraProvenanceMetadata = OTI2OMFModelTerminologyGraphProvenance.asOMFGraphOntologyProvenance(ont))
        .flatMap { g: MutableBundle =>

          for {

            _ <- as.foldLeft(types.rightUnitNES) { case (acc, ap) =>
              for {
                _ <- acc
                _ <- g.addAnnotationProperty(ap)
              } yield ()
            }

            _ <- getRelevantOntologyAnnotations(ont).foldLeft(types.rightUnitNES) { case (acc, a) =>
              for {
                _ <- acc
                av <- getAnnotationValueFromOWLAnnotation(a.getValue)
                ap <- getAnnotationPropertyFromOWLAnnotation(a)
                _ <- g.addAnnotation(g, ap, av)(omfStore)
              } yield ()
            }

            tboxExtensions <- extensions.foldLeft(Set.empty[ImmutableTerminologyBox].right[Throwables]) {
              case (acc, importG: ImmutableTerminologyGraph) =>
                for {
                  next <- acc
                  _ <- g.createTerminologyExtensionAxiom(extendedG = importG)(omfStore)
                } yield next + importG
              case (_, importG) =>
                Set[java.lang.Throwable](OMFError.omfError(
                  s"Create TerminologyGraph(${g.iri} cannot extend a non-terminology graph module: ${importG.iri}"
                )).left[Set[ImmutableTerminologyBox]]
            }

            resolver = TerminologyBoxResolverHelper(omfMetadata, g, tboxExtensions, ont, omfStore, om)
          } yield
            ImmutableTerminologyBoxResolver(resolver)
        }
    } else if (ontOps.isDescriptionBoxOntology) {
      // DescriptionBox
      val isPartial = ontOps.isFinalDescriptionBoxOntology
      val kind = if (isPartial)
        DescriptionKind.isPartial
      else
        DescriptionKind.isFinal

      omfStore.createOMFDescriptionBox(
        ont.getOntologyID.getOntologyIRI.get,
        relativeIRIPath = getOntologyRelativeIRI,
        relativeIRIHashPrefix = getOntologyIRIHashPrefix,
        ont, kind)
        .flatMap { g: MutableDescriptionBox =>

          for {

            _ <- as.foldLeft(types.rightUnitNES) { case (acc, ap) =>
              for {
                _ <- acc
                _ <- g.addAnnotationProperty(ap)
              } yield ()
            }

            _ <- getRelevantOntologyAnnotations(ont).foldLeft(types.rightUnitNES) { case (acc, a) =>
              for {
                _ <- acc
                av <- getAnnotationValueFromOWLAnnotation(a.getValue)
                ap <- getAnnotationPropertyFromOWLAnnotation(a)
                _ <- g.addAnnotation(g, ap, av)(omfStore)
              } yield ()
            }

            tboxImports <- extensions.foldLeft{
              Set.empty[ImmutableTerminologyBox].right[Throwables]
            } {
              case (acc, importG: ImmutableTerminologyBox) =>
                // ClosedWorld...
                for {
                  next <- acc
                } yield next + importG
              case (acc, _) =>
                // DescriptionBoxRefinement
                acc
            }

            dboxImports <- extensions.foldLeft{
              Set.empty[ImmutableDescriptionBox].right[Throwables]
            } {
              case (acc, importG: ImmutableDescriptionBox) =>
                // DescriptionBoxRefinement
                for {
                  next <- acc
                } yield next + importG
              case (acc, _) =>
                // ClosedWorld...
                acc
            }

            resolver = DescriptionBoxResolverHelper(g, tboxImports, dboxImports, ont, omfStore, om)
          } yield
            ImmutableDescriptionBoxResolver(resolver)
        }
    } else {
      // TerminologyGraph
      val isOpen = ontOps.isOpenWorldDefinitionTerminologyBoxOntology
      val kind =
        if (isOpen)
          TerminologyKind.isDefinition
        else
          TerminologyKind.isDesignation

      omfStore.createOMFTerminologyGraph(
        ont.getOntologyID.getOntologyIRI.get,
        relativeIRIPath = getOntologyRelativeIRI,
        relativeIRIHashPrefix = getOntologyIRIHashPrefix,
        ont, kind,
        extraProvenanceMetadata = OTI2OMFModelTerminologyGraphProvenance.asOMFGraphOntologyProvenance(ont))
        .flatMap { g: MutableTerminologyGraph =>

          for {

            _ <- as.foldLeft(types.rightUnitNES) { case (acc, ap) =>
              for {
                _ <- acc
                _ <- g.addAnnotationProperty(ap)
              } yield ()
            }

            _ <- getRelevantOntologyAnnotations(ont).foldLeft(types.rightUnitNES) { case (acc, a) =>
              for {
                _ <- acc
                av <- getAnnotationValueFromOWLAnnotation(a.getValue)
                ap <- getAnnotationPropertyFromOWLAnnotation(a)
                _ <- g.addAnnotation(g, ap, av)(omfStore)
              } yield ()
            }

            tboxExtensions <- extensions.foldLeft(Set.empty[ImmutableTerminologyBox].right[Throwables]) {
              case (acc, importG: ImmutableTerminologyGraph) =>
                for {
                  next <- acc
                  _ <- g.createTerminologyExtensionAxiom(extendedG = importG)(omfStore)
                } yield next + importG
              case (_, importG) =>
                Set[java.lang.Throwable](OMFError.omfError(
                  s"Create TerminologyGraph(${g.iri} cannot extend a non-terminology graph module: ${importG.iri}"
                )).left[Set[ImmutableTerminologyBox]]
            }

            _ <- nesting.fold[types.UnitNES](types.rightUnitNES) { case (nestingC, nestingG) =>
              g
                .createTerminologyNestingAxiom(parentG = nestingG, parentC = nestingC)(omfStore)
                .map(_ => ())
            }

            resolver = TerminologyBoxResolverHelper(omfMetadata, g, tboxExtensions, ont, omfStore, om)
          } yield
            ImmutableTerminologyBoxResolver(resolver)
        }
    }
  }

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