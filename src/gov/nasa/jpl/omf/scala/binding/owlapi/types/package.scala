/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.binding.owlapi

import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFLoader.OntologyLoadedState
import gov.nasa.jpl.omf.scala.binding.owlapi.types.AxiomExceptionKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.AxiomScopeAccessKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.EntityExceptionKind._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.RelationshipScopeAccessKind._
import gov.nasa.jpl.omf.scala.core.OMFError
import gov.nasa.jpl.omf.scala.core.TerminologyKind
import org.semanticweb.owlapi.model._

import scala.{None, Option, Some, StringContext, Unit}
import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.Predef.String
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
  : Set[java.lang.Throwable] \/ ImmutableModelTerminologyGraphResolver = {
    implicit val ops = omfStore.ops

    val ontOps = new OWLOntologyOps(ont)

    val isTboxDef = ontOps.isOntologyTBoxDefinition
    val IsTboxTop = ontOps.isOntologyTBoxToplevel
    val kind =
      if (isTboxDef)
        if (IsTboxTop)
          TerminologyKind.isToplevelDefinition
        else
          TerminologyKind.isDefinition
      else
      if (IsTboxTop)
        TerminologyKind.isToplevelDesignation
      else
        TerminologyKind.isDesignation

    val getOntologyShortName: Option[String] =
      ont
        .getAnnotations
        .find(_.getProperty.getIRI == ops.rdfs_label)
        .flatMap(_.getValue match {
          case l: OWLLiteral =>
            Some(l.getLiteral)
          case _ =>
            None
        })

    val getOntologyUUID: Option[String] =
      ont
        .getAnnotations
        .find(_.getProperty.getIRI == ops.AnnotationHasUUID)
        .flatMap(_.getValue match {
          case l: OWLLiteral =>
            Some(l.getLiteral)
          case _ =>
            None
        })

    val getOntologyRelativeIRI: Option[String] =
      ont
        .getAnnotations
        .find(_.getProperty.getIRI == ops.AnnotationHasRelativeIRI)
        .flatMap(_.getValue match {
          case l: OWLLiteral =>
            Some(l.getLiteral)
          case _ =>
            None
        })

    val getOntologyIRIHashPrefix: Option[String] =
      ont
        .getAnnotations
        .find(_.getProperty.getIRI == ops.AnnotationHasIRIHashPrefix)
        .flatMap(_.getValue match {
          case l: OWLLiteral =>
            Some(l.getLiteral)
          case _ =>
            None
        })

    val getOntologyIRIHashSuffix: Option[String] =
      ont
        .getAnnotations
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
        _ <- omfStore.ops.setTerminologyGraphShortName(g, getOntologyShortName)(omfStore)
        _ <- omfStore.ops.setTerminologyGraphUUID(g, getOntologyUUID)(omfStore)
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
    = ont.getSubClassAxiomsForSubClass(entity).to[Set]
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
                val values = oneOf.getValues
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
}