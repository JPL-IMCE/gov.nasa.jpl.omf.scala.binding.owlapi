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

import java.lang.Integer
import java.util.{Collections, UUID}

import gov.nasa.jpl.imce.oml.resolver.Extent2Tables.toUUIDString
import gov.nasa.jpl.imce.oml.resolver.Filterable.filterable
import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.{resolver, tables}
import gov.nasa.jpl.imce.oml.tables.taggedTypes.{LocalName, StringDataType, localName}
import gov.nasa.jpl.omf.scala.binding.owlapi.AxiomExceptionKind
import gov.nasa.jpl.omf.scala.binding.owlapi.AxiomScopeAccessKind.AxiomScopeAccessKind
import gov.nasa.jpl.omf.scala.binding.owlapi.ElementExceptionKind
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{axiomScopeException, duplicateModelTermAxiomException, duplicateTerminologyGraphAxiomException, entityAlreadyDefinedException, entityConflictException, entityScopeException, terms}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{MutableModule, Resource}
import gov.nasa.jpl.omf.scala.core.OMFError
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics.RelationshipCharacteristics
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWLFacet

import scala.collection.JavaConversions._
import scala.compat.java8.StreamConverters._
import scala.collection.immutable.{Iterable, Map, Seq, Set}
import scala.{Any, Boolean, Int, None, Option, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String, require}
import scalaz._
import Scalaz._
import gov.nasa.jpl.imce.oml.tables.{CardinalityRestrictionKind, ExactCardinalityRestriction, MaxCardinalityRestriction, MinCardinalityRestriction}

trait MutableTerminologyBox
  extends TerminologyBox
    with MutableModule {

  import ops._

  val chainRule2SWRLRule = new scala.collection.mutable.HashMap[ChainRule, SWRLRule]()

  def makeVariable(index: Int)
  : OMFError.Throwables \/ SWRLIArgument
  = for {
    viri <- withFragment(iri, localName(s"v$index"))
    v = owlDataFactory.getSWRLVariable(viri)
  } yield v

  override type MS = MutableTerminologyBoxSignature[OWLAPIOMF]

  override val sig: MS

  override protected val iri2typeTerm = scala.collection.mutable.HashMap[IRI, OWLAPIOMF#Term]()
  override protected val reifiedRelation2forwardProperty = scala.collection.mutable.HashMap[ReifiedRelationship, ForwardProperty]()
  override protected val reifiedRelation2inverseProperty = scala.collection.mutable.HashMap[ReifiedRelationship, InverseProperty]()

  val LOG: Boolean = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.MutableTerminologyBox")

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: MutableTerminologyBox => true
    case _ => false
  }

  def addAnnotationProperty
  (ap: tables.AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ tables.AnnotationProperty
  = for {
    _ <- (sig.annotationProperties += ap).right[OMFError.Throwables]
    ont_ap = owlDataFactory.getOWLAnnotationProperty(ap.iri)
    _ <- applyOntologyChangeOrNoOp(ontManager,
      new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
        ont_ap,
        createOMLProvenanceAnnotations(ap.uuid))),
      "addAnnotationProperty error")
  } yield ap

  def addAnnotation
  (subject: OWLAPIOMF#LogicalElement,
   property: tables.AnnotationProperty,
   value: StringDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ tables.AnnotationPropertyValue
  = subject match {
    case rsubject: OWLAPIOMF#ReifiedRelationship =>
      addReifiedRelationshipAnnotation(rsubject, property, value)
    case _ =>
      addLogicalElementAnnotation(subject, property, value)
  }

  def addLogicalElementAnnotation
  (subject: OWLAPIOMF#LogicalElement,
   property: tables.AnnotationProperty,
   value: StringDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ tables.AnnotationPropertyValue
  = for {
    a <- new tables.AnnotationPropertyValue(
      oug = uuidG,
      subjectUUID = subject.uuid,
      propertyUUID = property.uuid,
      value = value).right[OMFError.Throwables]
    _ = sig.annotationPropertyValues += a
    ont_ap = owlDataFactory.getOWLAnnotationProperty(property.iri)
    ont_lit = owlDataFactory.getOWLLiteral(value, owlDataFactory.getStringOWLDatatype)
    _ <- subject match {
      case m: MutableModule =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddOntologyAnnotation(ont, owlDataFactory.getOWLAnnotation(ont_ap, ont_lit)),
          "addAnnotation error")
      case rr: OWLAPIOMF#ReifiedRelationship =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
            ont_ap,
            rr.forwardProperty.e.getIRI,
            ont_lit)),
          "addAnnotation error")
      case r: OWLAPIOMF#Resource =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
            ont_ap,
            r.iri,
            ont_lit)),
          "addAnnotation error")
      case _ =>
        Set[java.lang.Throwable](
          OMFError.omfError(s"addAnnotation is not supported for a non-resource subject: $subject")
        ).left
    }
  } yield a

  def addReifiedRelationshipAnnotation
  (subject: OWLAPIOMF#ReifiedRelationship,
   property: tables.AnnotationProperty,
   value: StringDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ tables.AnnotationPropertyValue
  = for {
    a <- new tables.AnnotationPropertyValue(
      oug = uuidG,
      subjectUUID = subject.uuid,
      propertyUUID = property.uuid,
      value = value).right[OMFError.Throwables]
    _ = sig.annotationPropertyValues += a
    ont_lit = owlDataFactory.getOWLLiteral(value, owlDataFactory.getStringOWLDatatype)
    _ <- if (property.iri == store.ops.omlHasReificationLabelIRI)
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
            store.RDFS_LABEL,
            subject.iri,
            ont_lit)),
          "addAnnotation error")
      else if (property.iri == store.ops.omlHasPropertyLabelIRI)
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
            store.RDFS_LABEL,
            subject.forwardProperty.e.getIRI,
            ont_lit)),
          "addAnnotation error")
      else if (property.iri == store.ops.omlHasInverseLabelIRI)
        subject.inverseProperty.fold[OMFError.Throwables \/ Unit] {
          Set[java.lang.Throwable](
            OMFError.omfError(s"addAnnotation oml:hasInverseLabel is not applicable to a ReifiedProperty without an inverse: $subject")
          ).left
        } { inv =>
          applyOntologyChangeOrNoOp(
            ontManager,
            new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
              store.RDFS_LABEL,
              inv.e.getIRI,
              ont_lit)),
            "addAnnotation error")
        }
      else
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
            owlDataFactory.getOWLAnnotationProperty(property.iri),
            subject.forwardProperty.e.getIRI,
            ont_lit)),
          "addAnnotation error")
  } yield a

  def removeAnnotations
  (subject: OWLAPIOMF#LogicalElement,
   property: tables.AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Set[tables.AnnotationPropertyValue]
  = for {
    sUUID <- subject.uuid.toString.right[OMFError.Throwables]
    ae <- sig.annotationPropertyValues.find { a => a.subjectUUID == sUUID && a.propertyUUID == property.uuid } match {
      case Some(a0) =>
        sig.annotationPropertyValues -= a0
        Set(a0).right
      case None =>
        Set.empty[tables.AnnotationPropertyValue].right
    }
    ont_ap = owlDataFactory.getOWLAnnotationProperty(property.iri)
    _ <- subject match {
      case r: OWLAPIOMF#Resource =>
        val aaas =
          ont
            .annotationAssertionAxioms(r.iri)
            .toScala[Seq]
            .filter { aaa => aaa.getAnnotation.getProperty.getIRI == ont_ap.getIRI }
            .map { aaa => new RemoveAxiom(ont, aaa) }
        if (aaas.nonEmpty)
          applyOntologyChangesOrNoOp(ontManager, aaas, "removeAnnotations error")
        else
          ().right
      case _ =>
        Set[java.lang.Throwable](
          OMFError.omfError(s"removeAnnotations is not supported for a non-resource subject: $subject")
        ).left
    }
  } yield ae

  // Terminology Axioms

  def createTerminologyExtensionAxiom
  (extendedG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ TerminologyExtensionAxiom
  = for {
    uuid <- ops.terminologyExtensionUUID(this, extendedG)
    ax <- createTerminologyExtensionAxiom(uuid, extendedG)
  } yield ax

  def createTerminologyExtensionAxiom
  (uuid: api.taggedTypes.TerminologyExtensionAxiomUUID,
   extendedG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ TerminologyExtensionAxiom
  = sig.extensions
    .find { _.targetModuleIRI == extendedG.iri }
    .fold[OMFError.Throwables \/ TerminologyExtensionAxiom](
    for {
      axiom <- store
        .createOMFTerminologyGraphDirectExtensionAxiom(uuid, this, extendedG)
    } yield {
      sig.extensions += axiom
      axiom
    }
  ) { other =>
    other.right
  }

  def addTerminologyGraphExtension
  (uuid: api.taggedTypes.TerminologyExtensionAxiomUUID,
   extendedG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ TerminologyExtensionAxiom
  = for {
    axiom <- createTerminologyExtensionAxiom(uuid, extendedG)
    _ <- if (extendedG.builtInVocabulary)
      ().right[OMFError.Throwables]
    else applyOntologyChangeOrNoOp(ontManager,
      new AddImport(ont, owlDataFactory
        .getOWLImportsDeclaration(extendedG.iri)),
      "addTerminologyGraphExtension error")
  } yield axiom

  protected def makeOWLCardinalityRestriction
  (restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  : OWLClassExpression
  = {
    val n = Integer.valueOf(restrictedCardinality).intValue()
    restrictionKind match {
      case MinCardinalityRestriction =>
        restrictedRange match {
          case Some(range) =>
            owlDataFactory.getOWLObjectMinCardinality(n, restrictedRelationship.e, range.e)
          case None =>
            owlDataFactory.getOWLObjectMinCardinality(n, restrictedRelationship.e)
        }
      case MaxCardinalityRestriction =>
        restrictedRange match {
          case Some(range) =>
            owlDataFactory.getOWLObjectMaxCardinality(n, restrictedRelationship.e, range.e)
          case None =>
            owlDataFactory.getOWLObjectMaxCardinality(n, restrictedRelationship.e)
        }
      case ExactCardinalityRestriction =>
        restrictedRange match {
          case Some(range) =>
            owlDataFactory.getOWLObjectExactCardinality(n, restrictedRelationship.e, range.e)
          case None =>
            owlDataFactory.getOWLObjectExactCardinality(n, restrictedRelationship.e)
        }
    }
  }

  /**
    * Constructs an OMF Aspect as part of resolving its representation as an OWL Class
    * @param a An OWL Class representing an OMF Aspect
    * @param store
    * @return The OMF Aspect corresponding to its OWL Class `a` representation
    */
  def createModelEntityAspect
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID, a: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Aspect
  = for {
    n <- getFragment(a.getIRI)
    u = api.taggedTypes.aspectUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createModelEntityAspect(a, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, a.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelEntityAspect
  (a: OWLClass, name: LocalName, uuid: api.taggedTypes.AspectUUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Aspect
  = iri2typeTerm
    .get(a.getIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#Aspect] {
    val _a = terms.Aspect(a, a.getIRI, name, uuid)
    sig.aspects += _a
    iri2typeTerm += a.getIRI -> _a
    \/-(_a)
  } {
    case t: OWLAPIOMF#Aspect =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityAspect, a.getIRI, t)
      ).left
  }

  def createCardinalityRestrictedAspect
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   a: OWLClass,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedAspect
  = for {
    n <- getFragment(a.getIRI)
    u = api.taggedTypes.cardinalityRestrictedAspectUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createCardinalityRestrictedAspect(a, n, u, restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
    aas = getRelevantSubjectAnnotationAssertions(ont, a.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createCardinalityRestrictedAspect
  (a: OWLClass, name: LocalName, uuid: api.taggedTypes.CardinalityRestrictedAspectUUID,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedAspect
  = iri2typeTerm
    .get(a.getIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedAspect] {
    val _a = terms.CardinalityRestrictedAspect(a, a.getIRI, name, uuid, restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
    sig.cardinalityRestrictedAspects += _a
    iri2typeTerm += a.getIRI -> _a
    \/-(_a)
  } {
    case t: OWLAPIOMF#CardinalityRestrictedAspect =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityCardinalityRestrictedAspect, a.getIRI, t)
      ).left
  }

  def addEntityAspect
  (aspectIRI: IRI, name: LocalName, uuid: api.taggedTypes.AspectUUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Aspect
  = iri2typeTerm
    .get(aspectIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#Aspect]{
    val aspectC = owlDataFactory.getOWLClass(aspectIRI)
    for {
      result <- createModelEntityAspect(aspectC, name, uuid)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            aspectC,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
            aspectC,
            backbone.AspectKindC,
            createOMLProvenanceAnnotations(uuid)))),
        "addAspect Error")
    } yield result
  } {
    case t: OWLAPIOMF#Aspect =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityAspect, aspectIRI, t)
      ).left
  }

  def addCardinalityRestrictedAspect
  (aspectIRI: IRI, name: LocalName, uuid: api.taggedTypes.CardinalityRestrictedAspectUUID,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedAspect
  = iri2typeTerm
    .get(aspectIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedAspect]{
    val aspectC = owlDataFactory.getOWLClass(aspectIRI)
    for {
      result <- createCardinalityRestrictedAspect(aspectC, name, uuid, restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
      sup = makeOWLCardinalityRestriction(restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            aspectC,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
            aspectC,
            backbone.AspectKindC,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
            aspectC,
            sup,
            createOMLProvenanceAnnotations(uuid)))
        ),
        "addCardinalityRestrictedAspect Error")
    } yield result
  } {
    case t: OWLAPIOMF#CardinalityRestrictedAspect =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityCardinalityRestrictedAspect, aspectIRI, t)
      ).left
  }

  /**
    * Constructs an OMF Concept as part of resolving its representation as an OWL Class
    * @param c An OWL Class representing an OMF Concept
    * @param store
    * @return The OMF Concept corresponding to its OWL Class `c` representation
    */
  def createModelEntityConcept
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   c: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Concept
  = for {
    n <- getFragment(c.getIRI)
    u = api.taggedTypes.conceptUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createModelEntityConcept(c, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, c.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelEntityConcept
  (c: OWLClass, name: LocalName, uuid: api.taggedTypes.ConceptUUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Concept
  = iri2typeTerm
    .get(c.getIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#Concept]{
    val _c = terms.Concept(c, c.getIRI, name, uuid)
    sig.concepts += _c
    iri2typeTerm += c.getIRI -> _c
    \/-(_c)
  } {
    case t: Concept =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityConcept, c.getIRI, t)
      ).left
  }

  def createCardinalityRestrictedConcept
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   c: OWLClass,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedConcept
  = for {
    n <- getFragment(c.getIRI)
    u = api.taggedTypes.cardinalityRestrictedConceptUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createCardinalityRestrictedConcept(c, n, u, restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
    aas = getRelevantSubjectAnnotationAssertions(ont, c.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createCardinalityRestrictedConcept
  (c: OWLClass, name: LocalName, uuid: api.taggedTypes.CardinalityRestrictedConceptUUID,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedConcept
  = iri2typeTerm
    .get(c.getIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedConcept] {
    val _c = terms.CardinalityRestrictedConcept(c, c.getIRI, name, uuid, restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
    sig.cardinalityRestrictedConcepts += _c
    iri2typeTerm += c.getIRI -> _c
    \/-(_c)
  } {
    case t: OWLAPIOMF#CardinalityRestrictedConcept =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityCardinalityRestrictedConcept, c.getIRI, t)
      ).left
  }

  def addEntityConcept
  (conceptIRI: IRI, name: LocalName, uuid: api.taggedTypes.ConceptUUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Concept
  = iri2typeTerm
    .get(conceptIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#Concept] {
    val conceptC = owlDataFactory.getOWLClass(conceptIRI)
    for {
      result <- createModelEntityConcept(conceptC, name, uuid)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont,
            owlDataFactory.getOWLDeclarationAxiom(
              conceptC,
              createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont,
            owlDataFactory.getOWLSubClassOfAxiom(
              conceptC,
              backbone.ConceptKindC,
              createOMLProvenanceAnnotations(uuid)))),
        "addConcept Error")
    } yield result
  } {
    case t: OWLAPIOMF#Concept =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityConcept, conceptIRI, t)
      ).left
  }

  def addCardinalityRestrictedConcept
  (conceptIRI: IRI, name: LocalName, uuid: api.taggedTypes.CardinalityRestrictedConceptUUID,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedConcept
  = iri2typeTerm
    .get(conceptIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedConcept]{
    val conceptC = owlDataFactory.getOWLClass(conceptIRI)
    for {
      result <- createCardinalityRestrictedConcept(conceptC, name, uuid, restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
      sup = makeOWLCardinalityRestriction(restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            conceptC,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
            conceptC,
            backbone.ConceptKindC,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
            conceptC,
            sup,
            createOMLProvenanceAnnotations(uuid)))
        ),
        "addCardinalityRestrictedConcept Error")
    } yield result
  } {
    case t: OWLAPIOMF#CardinalityRestrictedConcept =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityCardinalityRestrictedConcept, conceptIRI, t)
      ).left
  }

  def createReifiedRelationshipRestriction
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   e: OWLClass,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipRestriction
  = for {
    name <- getFragment(e.getIRI)
    uuid = api.taggedTypes.reifiedRelationshipRestrictionUUID(generateUUIDFromString(tboxUUID, "name" -> name))
    term <- createReifiedRelationshipRestriction(e, uuid, e.getIRI, name, source, target)
    aas = getRelevantSubjectAnnotationAssertions(ont, e.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createReifiedRelationshipRestriction
  (e: OWLClass,
   uuid: api.taggedTypes.ReifiedRelationshipRestrictionUUID,
   iri: IRI,
   name: LocalName,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipRestriction
  = iri2typeTerm
    .get(iri)
    .fold[OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipRestriction] {
    val _pr = ReifiedRelationshipRestriction(e, iri, name, uuid,source, target)
    sig.reifiedRelationshipRestrictions += _pr
    iri2typeTerm += _pr.iri -> _pr
    _pr.right
  } {
    case srr: OWLAPIOMF#ReifiedRelationshipRestriction =>
      srr.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.ReifiedRelationshipRestriction, iri, t)
      ).left
  }

  def makeReifiedRelationshipRestriction
  (uuid: api.taggedTypes.ReifiedRelationshipRestrictionUUID,
   iri: IRI,
   name: LocalName,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipRestriction
  = sig.axioms
    .find {
      case axiom: ReifiedRelationshipRestriction =>
        axiom.iri == iri && axiom.source == source && axiom.target == target
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ ReifiedRelationshipRestriction] {
    val e = owlDataFactory.getOWLClass(iri)
    for {
      result <- createReifiedRelationshipRestriction(e, uuid, iri, name, source, target)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            e,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
            e,
            backbone.ConceptualRelationshipKindC,
            createOMLProvenanceAnnotations(uuid)))
        ),
        "makeReifiedRelationshipRestriction error")
    } yield result
  } {
    case term: OWLAPIOMF#ReifiedRelationshipRestriction =>
      term.right

    case other =>
      Set(
        duplicateModelTermAxiomException(AxiomExceptionKind.ReifiedRelationshipRestrictionException, other)
      ).left
  }

  def addReifiedRelationshipRestriction
  (uuid: api.taggedTypes.ReifiedRelationshipRestrictionUUID,
   iri: IRI,
   name: LocalName,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipRestriction
  = (isTypeTermDefinedRecursively(source), isTypeTermDefinedRecursively(target)) match {
    case (true, true) =>
      makeReifiedRelationshipRestriction(uuid, iri, name, source, target)

    case (s, t) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ReifiedRelationshipRestrictionException,
          Map.empty[AxiomScopeAccessKind, Resource] ++
            (if (s)
              Map.empty[AxiomScopeAccessKind, Resource]
            else
              Map(AxiomScopeAccessKind.Domain -> source)) ++
            (if (t)
              Map.empty[AxiomScopeAccessKind, Resource]
            else
              Map(AxiomScopeAccessKind.Range -> target))
        )).left
  }

  /**
    * Constructs an OMF ReifiedRelationship as part of resolving its representation as an OWL Class
    * @param r An OWL Class representing an OMF ReifiedRelationship
    * @param u An OWL ObjectProperty representing the unreified property from the RR domain to the RR range
    * @param ui Optionally, the inverse OWL ObjectProperty from the RR range to the RR domain
    * @param source The resolved OMF RR domain
    * @param rSource The OWL ObjectProperty representing the RR domain property
    * @param target The resolved OMF RR range
    * @param rTarget The OWL ObjectProperty representing the RR range property
    * @param characteristics The resolved characteristis of the OWL representation of the RR
    * @param store
    * @return
    */
  def createEntityReifiedRelationship
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   r: OWLClass,
   u: OWLObjectProperty, ui: Option[OWLObjectProperty],
   source: Entity, rSource: OWLObjectProperty,
   target: Entity, rTarget: OWLObjectProperty,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationship
  = for {
    rn <- getFragment(r.getIRI)
    ru = api.taggedTypes.reifiedRelationshipUUID(generateUUIDFromString(tboxUUID, "name" -> rn))
    un <- getFragment(u.getIRI)
    in <- ui.fold[OMFError.Throwables \/ Option[LocalName]](None.right) { i =>
      getFragment(i.getIRI).map(Some(_))
    }
    term <- createEntityReifiedRelationship(
      r, rn, ru,
      un, u, in, ui,
      source, rSource, target, rTarget, characteristics)
    ras = getRelevantSubjectAnnotationAssertions(ont, r.getIRI)
    pas = getRelevantSubjectAnnotationAssertions(ont, u.getIRI)
    ias = ui.map(i => getRelevantSubjectAnnotationAssertions(ont, i.getIRI))
    _ <- store.ops.addReificationAnnotationAssertions(this, term, ras)
    _ <- store.ops.addReifiedPropertyAnnotationAssertions(this, term, pas)
    _ <- ias.fold[OMFError.Throwables \/ Unit] { types.rightUnitNES }{ as =>
      store.ops.addReifiedInverseAnnotationAssertions(this, term, as)
    }
  } yield term

  def createEntityReifiedRelationship
  (r: OWLClass, name: LocalName, uuid: api.taggedTypes.ReifiedRelationshipUUID,
   unreifiedPropertyName: LocalName, u: OWLObjectProperty,
   inversePropertyName: Option[LocalName], ui: Option[OWLObjectProperty],
   source: Entity, rSource: OWLObjectProperty,
   target: Entity, rTarget: OWLObjectProperty,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationship
  = iri2typeTerm
    .get(r.getIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationship] {
    for {
      uName <- getFragment(u.getIRI)
      fwdUUID = resolver.api.taggedTypes.forwardPropertyUUID(
        generateUUIDFromString(
          "ForwardProperty",
          "name" -> uName,
          "reifiedRelationship" -> uuid.toString))
      fwd = ForwardProperty(u, fwdUUID, u.getIRI, uName)

      inv <- ui.fold[OMFError.Throwables \/ Option[InverseProperty]](\/-(None)) { i =>
        for {
          iName <- getFragment(i.getIRI)
          invUUID = resolver.api.taggedTypes.inversePropertyUUID(
            generateUUIDFromString(
              "InverseProperty",
              "name" -> iName,
              "reifiedRelationship" -> uuid.toString))
          inv = InverseProperty(i, invUUID, i.getIRI, iName)
        } yield Some(inv)
      }

      _r = terms.ReifiedRelationship(r, r.getIRI, name, uuid,
        fwd, inv,
        source, rSource,
        target, rTarget,
        characteristics)
      _ = sig.reifiedRelationships += _r
      _ = sig.forwardProperties += fwd
      _ = sig.inverseProperties ++= inv
      _ = iri2typeTerm += r.getIRI -> _r
      _ = reifiedRelation2forwardProperty += _r -> fwd
      _ = inv.foreach { i =>
        reifiedRelation2inverseProperty += _r -> i
      }
    } yield _r
  } {
    case t: OWLAPIOMF#ReifiedRelationship =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityReifiedRelationship, r.getIRI, t)
      ).left
  }

  protected def makeEntityReifiedRelationship
  (rIRI: IRI,
   name: LocalName,
   unreifiedRelationshipName: LocalName,
   unreifiedInverseRelationshipName: Option[LocalName],
   uuid: api.taggedTypes.ReifiedRelationshipUUID,
   rIRISource: IRI, rIRITarget: IRI,
   uIRI: IRI, uiIRI: Option[IRI],
   source: Entity, target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationship
  = {
    val sourceC = owlDataFactory.getOWLClass(source.iri)
    val targetC = owlDataFactory.getOWLClass(target.iri)
    val r = owlDataFactory.getOWLClass(rIRI)
    val rSource = owlDataFactory.getOWLObjectProperty(rIRISource)
    val rTarget = owlDataFactory.getOWLObjectProperty(rIRITarget)
    val u = owlDataFactory.getOWLObjectProperty(uIRI)
    val ui = uiIRI.map { _uiIRI => owlDataFactory.getOWLObjectProperty(_uiIRI) }

    for {
      result <- createEntityReifiedRelationship(r, name, uuid,
        unreifiedRelationshipName, u,
        unreifiedInverseRelationshipName, ui,
        source, rSource,
        target, rTarget,
        characteristics)

      vr = owlDataFactory.getSWRLVariable(IRI.create("urn:swrl#r"))
      vs = owlDataFactory.getSWRLVariable(IRI.create("urn:swrl#s"))
      vt = owlDataFactory.getSWRLVariable(IRI.create("urn:swrl#t"))

      body1 = owlDataFactory.getSWRLObjectPropertyAtom(rSource, vr, vs)
      body2 = owlDataFactory.getSWRLObjectPropertyAtom(rTarget, vr, vt)

      head = owlDataFactory.getSWRLObjectPropertyAtom(u, vs, vt)

      rule = owlDataFactory.getSWRLRule(
        Set(body1, body2),
        Set(head),
        createOMLProvenanceAnnotationsWithLabel(s"$unreifiedRelationshipName derivarion", uuid))

      _ <-
      applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            r,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
            r,
            backbone.ConceptualRelationshipKindC,
            createOMLProvenanceAnnotations(uuid))),

          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            rSource,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
            rSource,
            backbone.topReifiedObjectPropertySourceOP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
            rSource,
            r,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
            rSource,
            sourceC,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom(
            rSource,
            createOMLProvenanceAnnotations(uuid))),

          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            rTarget,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
            rTarget,
            backbone.topReifiedObjectPropertyTargetOP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
            rTarget,
            r,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
            rTarget,
            targetC,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom(
            rTarget,
            createOMLProvenanceAnnotations(uuid))),

          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            u,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
            u,
            backbone.topReifiedObjectPropertyOP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
            u,
            sourceC,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
            u,
            targetC,
            createOMLProvenanceAnnotations(uuid))),

          new AddAxiom(ont, rule)
        ) ++
          ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
                _ui,
                createOMLProvenanceAnnotations(uuid))),
              new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
                isDerivedAP,
                _ui.getIRI,
                owlDataFactory.getOWLLiteral(true),
                createOMLProvenanceAnnotations(uuid))),
              new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                _ui,
                backbone.topReifiedObjectPropertyOP,
                createOMLProvenanceAnnotations(uuid))),
              new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
                _ui,
                targetC,
                createOMLProvenanceAnnotations(uuid))),
              new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
                _ui,
                sourceC,
                createOMLProvenanceAnnotations(uuid))),
              new AddAxiom(ont, owlDataFactory.getOWLInverseObjectPropertiesAxiom(
                u,
                _ui,
                createOMLProvenanceAnnotations(uuid)))
            )
          } ++
          (if (characteristics.contains(RelationshipCharacteristics.isFunctional))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLInverseFunctionalObjectPropertyAxiom(
                rSource,
                createOMLProvenanceAnnotations(uuid))),
              new AddAxiom(ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom(
                u,
                createOMLProvenanceAnnotations(uuid)))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLInverseFunctionalObjectPropertyAxiom(
                  _ui,
                  createOMLProvenanceAnnotations(uuid)))
              )
            }
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isInverseFunctional))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLInverseFunctionalObjectPropertyAxiom(
                rTarget,
                createOMLProvenanceAnnotations(uuid))),
              new AddAxiom(ont, owlDataFactory.getOWLInverseFunctionalObjectPropertyAxiom(
                u,
                createOMLProvenanceAnnotations(uuid)))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom(
                  _ui,
                  createOMLProvenanceAnnotations(uuid)))
              )
            }
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isSymmetric))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLSymmetricObjectPropertyAxiom(
                u,
                createOMLProvenanceAnnotations(uuid)))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSymmetricObjectPropertyAxiom(
                  _ui,
                  createOMLProvenanceAnnotations(uuid)))
              )
            }
            else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isAsymmetric))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLAsymmetricObjectPropertyAxiom(
                u,
                createOMLProvenanceAnnotations(uuid)))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLAsymmetricObjectPropertyAxiom(
                  _ui,
                  createOMLProvenanceAnnotations(uuid)))
              )
            }
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isReflexive))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLReflexiveObjectPropertyAxiom(
                u,
                createOMLProvenanceAnnotations(uuid)))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLReflexiveObjectPropertyAxiom(
                  _ui,
                  createOMLProvenanceAnnotations(uuid)))
              )
            }
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isIrreflexive))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLIrreflexiveObjectPropertyAxiom(
                u,
                createOMLProvenanceAnnotations(uuid)))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLIrreflexiveObjectPropertyAxiom(
                  _ui,
                  createOMLProvenanceAnnotations(uuid)))
              )
            }
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isEssential))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                sourceC,
                owlDataFactory.getOWLObjectExactCardinality(1, u, targetC),
                createOMLProvenanceAnnotations(uuid)))
            )
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isInverseEssential))
            ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  targetC,
                  owlDataFactory.getOWLObjectExactCardinality(1, _ui, sourceC),
                  createOMLProvenanceAnnotations(uuid)))
              )
            }
          else
            Seq.empty),
        "makeEntityReifiedRelationship error")

    } yield result
  }

  def addEntityReifiedRelationship
  (rIRI: IRI, name: LocalName,
   unreifiedRelationshipName: LocalName,
   unreifiedInverseRelationshipName: Option[LocalName],
   uuid: api.taggedTypes.ReifiedRelationshipUUID,
   rIRISource: IRI, rIRITarget: IRI,
   uIRI: IRI, uiIRI: Option[IRI],
   source: Entity, target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationship
  = ( lookupTerm(rIRI, recursively = true),
    lookupTerm(rIRISource, recursively = true),
    lookupTerm(rIRITarget, recursively = true),
    lookupTerm(uIRI, recursively = true),
    lookupTerm(uiIRI, recursively = true) ) match {
    case (None, None, None, None, None) =>
      (isTypeTermDefinedRecursively(source), isTypeTermDefinedRecursively(target)) match {
        case (true, true) =>
          makeEntityReifiedRelationship(
            rIRI, name, unreifiedRelationshipName, unreifiedInverseRelationshipName,
            uuid, rIRISource, rIRITarget, uIRI, uiIRI,
            source, target, characteristics)
        case (false, true) =>
          Set(
            entityScopeException(ElementExceptionKind.EntityReifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Source -> source))
          ).left

        case (true, false) =>
          val closure = terminologyBoxImportClosure(this).to[Seq].sortBy(_.iri.toString)
          java.lang.System.out.println(s"from: ${this.iri} unresolvable target: ${target.iri}\nimport closure ${closure.size} graphs:")
          java.lang.System.out.println(closure.map(_.iri).mkString("\n"))

          Set(
            entityScopeException(ElementExceptionKind.EntityReifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Target -> target))
          ).left

        case (false, false) =>
          Set(
            entityScopeException(ElementExceptionKind.EntityReifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
          ).left
      }

    case (Some(t), _, _, _, _) =>
      Set(
        entityConflictException(ElementExceptionKind.EntityReifiedRelationship, rIRI, t)
      ).left

    case (_, Some(t), _, _, _) =>
      Set(
        entityConflictException(ElementExceptionKind.EntityReifiedRelationship, rIRISource, t)
      ).left

    case (_, _, Some(t), _, _) =>
      Set(
        entityConflictException(ElementExceptionKind.EntityReifiedRelationship, rIRITarget, t)
      ).left

    case (_, _, _, Some(t), _) =>
      Set(
        entityConflictException(ElementExceptionKind.EntityReifiedRelationship, uIRI, t)
      ).left

    case (_, _, _, _, Some(t)) =>
      require(uiIRI.isDefined)
      Set(
        entityConflictException(ElementExceptionKind.EntityReifiedRelationship, uiIRI.get, t)
      ).left
  }

  def createCardinalityRestrictedReifiedRelationship
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   r: OWLClass,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedReifiedRelationship
  = for {
    n <- getFragment(r.getIRI)
    u = api.taggedTypes.cardinalityRestrictedReifiedRelationshipUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createCardinalityRestrictedReifiedRelationship(
      r, n, u,
      restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
    aas = getRelevantSubjectAnnotationAssertions(ont, r.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createCardinalityRestrictedReifiedRelationship
  (r: OWLClass, name: LocalName, uuid: api.taggedTypes.CardinalityRestrictedReifiedRelationshipUUID,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedReifiedRelationship
  = iri2typeTerm
    .get(r.getIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedReifiedRelationship] {
    val _r = terms.CardinalityRestrictedReifiedRelationship(
      r, r.getIRI, name, uuid,
      restrictedRelationship.domain(),
      restrictedRelationship.range(),
      restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
    sig.cardinalityRestrictedReifiedRelationships += _r
    iri2typeTerm += r.getIRI -> _r
    \/-(_r)
  } {
    case t: OWLAPIOMF#CardinalityRestrictedReifiedRelationship =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityCardinalityRestrictedReifiedRelationship, r.getIRI, t)
      ).left
  }


  protected def makeCardinalityRestrictedReifiedRelationship
  (rIRI: IRI,
   name: LocalName,
   uuid: api.taggedTypes.CardinalityRestrictedReifiedRelationshipUUID,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedReifiedRelationship
  = {
    val r = owlDataFactory.getOWLClass(rIRI)
    for {
      result <- createCardinalityRestrictedReifiedRelationship(r, name, uuid,
        restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
      sup = makeOWLCardinalityRestriction(restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)
      _ <-
        applyOntologyChangesOrNoOp(ontManager,
          Seq(
            new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
              r,
              createOMLProvenanceAnnotations(uuid))),
            new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
              r,
              backbone.ConceptualRelationshipKindC,
              createOMLProvenanceAnnotations(uuid))),
            new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
              r,
              sup,
              createOMLProvenanceAnnotations(uuid)))
          ),
          "makeCardinalityRestrictedReifiedRelationship error")

    } yield result
  }

  def addCardinalityRestrictedReifiedRelationship
  (rIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.CardinalityRestrictedReifiedRelationshipUUID,
   restrictionKind: CardinalityRestrictionKind,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: Option[OWLAPIOMF#Entity],
   restrictedCardinality: tables.taggedTypes.PositiveIntegerLiteral)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#CardinalityRestrictedReifiedRelationship
  = lookupTerm(rIRI, recursively = true) match {
    case None =>
      makeCardinalityRestrictedReifiedRelationship(
            rIRI, name, uuid, restrictionKind, restrictedRelationship, restrictedRange, restrictedCardinality)

    case Some(t) =>
      Set(
        entityConflictException(ElementExceptionKind.EntityCardinalityRestrictedReifiedRelationship, rIRI, t)
      ).left
  }

  /**
    * Constructs an OMF UnreifiedRelationship as part of resolving its representation as an OWL ObjectProperty
    * @param r
    * @param source
    * @param target
    * @param characteristics
    * @param store
    * @return
    */
  def createEntityUnreifiedRelationship
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   r: OWLObjectProperty,
   source: Entity,
   target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#UnreifiedRelationship
  = for {
    rn <- getFragment(r.getIRI)
    ru = api.taggedTypes.unreifiedRelationshipUUID(generateUUIDFromString(tboxUUID, "name" -> rn))
    term <- createEntityUnreifiedRelationship(
      r, rn, ru, source, target, characteristics)
    aas = getRelevantSubjectAnnotationAssertions(ont, r.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createEntityUnreifiedRelationship
  (r: OWLObjectProperty, name: LocalName, uuid: api.taggedTypes.UnreifiedRelationshipUUID,
   source: Entity,
   target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#UnreifiedRelationship
  = iri2typeTerm
    .get(r.getIRI)
    .fold[OMFError.Throwables \/ UnreifiedRelationship] {
    val _r = terms.UnreifiedRelationship(r, r.getIRI, name, uuid, source, target, characteristics)
    sig.unreifiedRelationships += _r
    iri2typeTerm += r.getIRI -> _r
    \/-(_r)
  } {
    case t: OWLAPIOMF#UnreifiedRelationship =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityUnreifiedRelationship, r.getIRI, t)
      ).left
  }

  protected def makeUnreifiedRelationship
  (rIRI: IRI, name: LocalName, uuid: api.taggedTypes.UnreifiedRelationshipUUID,
   source: Entity, target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#UnreifiedRelationship
  = {
    val sourceC = owlDataFactory.getOWLClass(source.iri)
    val targetC = owlDataFactory.getOWLClass(target.iri)
    val r = owlDataFactory.getOWLObjectProperty(rIRI)

    for {
      result <- createEntityUnreifiedRelationship(r, name, uuid,
        source,
        target,
        characteristics)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            r,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
            r,
            backbone.topUnreifiedObjectPropertyOP,
            createOMLProvenanceAnnotations(uuid))),

          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
            r,
            sourceC,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
            r,
            targetC,
            createOMLProvenanceAnnotations(uuid)))
        ) ++
          (if (characteristics.contains(RelationshipCharacteristics.isFunctional))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom(
                r,
                createOMLProvenanceAnnotations(uuid)))
            )
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isInverseFunctional))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLInverseFunctionalObjectPropertyAxiom(
                r,
                createOMLProvenanceAnnotations(uuid)))
            )
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isSymmetric))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLSymmetricObjectPropertyAxiom(
                r,
                createOMLProvenanceAnnotations(uuid)))
            )
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isAsymmetric))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLAsymmetricObjectPropertyAxiom(
                r,
                createOMLProvenanceAnnotations(uuid)))
            )
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isReflexive))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLReflexiveObjectPropertyAxiom(
                r,
                createOMLProvenanceAnnotations(uuid)))
            )
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isIrreflexive))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLIrreflexiveObjectPropertyAxiom(
                r,
                createOMLProvenanceAnnotations(uuid)))
            )
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isEssential))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                sourceC,
                owlDataFactory.getOWLObjectExactCardinality(1, r, targetC),
                createOMLProvenanceAnnotations(uuid)))
            )
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isInverseEssential))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                targetC,
                owlDataFactory.getOWLObjectExactCardinality(1, owlDataFactory.getOWLObjectInverseOf(r), sourceC),
                createOMLProvenanceAnnotations(uuid)))
            )
          else
            Seq.empty),
        "makeUnreifiedRelationship")
    } yield result
  }

  def addEntityUnreifiedRelationship
  (rIRI: IRI, name: LocalName, uuid: api.taggedTypes.UnreifiedRelationshipUUID,
   source: Entity, target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#UnreifiedRelationship
  = lookupTerm(rIRI, recursively = true) match {
    case None =>
      (isTypeTermDefinedRecursively(source), isTypeTermDefinedRecursively(target)) match {
        case (true, true) =>
          makeUnreifiedRelationship(rIRI, name, uuid, source, target, characteristics)
        case (false, true) =>
          Set(
            entityScopeException(ElementExceptionKind.EntityUnreifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Source -> source))
          ).left

        case (true, false) =>
          Set(
            entityScopeException(ElementExceptionKind.EntityUnreifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Target -> target))
          ).left

        case (false, false) =>
          Set(
            entityScopeException(ElementExceptionKind.EntityUnreifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
          ).left
      }

    case Some(t) =>
      Set(
        entityConflictException(ElementExceptionKind.EntityUnreifiedRelationship, rIRI, t)
      ).left
  }

  override def getEntityDefinitionMap
  : Map[OWLClass, OWLAPIOMF#Entity]
  = ( sig.aspects.map(a => a.e -> a) ++
      sig.concepts.map(c => c.e -> c) ++
      sig.reifiedRelationships.map(r => r.e -> r)).toMap

  override def getRestrictableRelationshipMap
  : Map[OWLObjectProperty, OWLAPIOMF#RestrictableRelationship] =
    sig.forwardProperties.map(p => p.e -> p).toMap ++
      sig.inverseProperties.map(p => p.e -> p).toMap ++
      sig.unreifiedRelationships.map(p => p.e -> p).toMap

  override def getScalarDatatypeDefinitionMap
  : Map[OWLDatatype, OWLAPIOMF#Scalar]
  = sig.scalarDataTypes
    .map(t => t.e -> t)
    .toMap

  /**
    * Constructs an OMF Scalar as part of resolving its representation as an OWL Datatype
    * @param dt
    * @param store
    * @return
    */
  def createModelScalarDataType
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   dt: OWLDatatype)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Scalar
  = for {
    n <- getFragment(dt.getIRI)
    u = api.taggedTypes.scalarUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createModelScalarDataType(dt, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, dt.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelScalarDataType
  (dt: OWLDatatype, name: LocalName, uuid: api.taggedTypes.ScalarUUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Scalar
  = iri2typeTerm
    .get(dt.getIRI)
    .fold[OMFError.Throwables \/ Scalar] {
    val _dt = terms.Scalar(dt, dt.getIRI, name, uuid)
    sig.scalarDataTypes += _dt
    iri2typeTerm += dt.getIRI -> _dt
    _dt.right
  } {
    case t: Scalar =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.ScalarDataType, dt.getIRI, t)
      ).left
  }

  def addScalarDataType
  (scalarIRI: IRI, name: LocalName, uuid: api.taggedTypes.ScalarUUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Scalar
  = iri2typeTerm
    .get(scalarIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#Scalar] {
    val scalarDT = owlDataFactory.getOWLDatatype(scalarIRI)
    for {
      result <- createModelScalarDataType(scalarDT, name, uuid)
      _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont,
            owlDataFactory.getOWLDeclarationAxiom(
              scalarDT,
              createOMLProvenanceAnnotations(uuid)))
        ),
        s"addScalarDataType error: $scalarIRI, name=$name")
    } yield result
  } {
    case t: OWLAPIOMF#Scalar =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.ScalarDataType, scalarIRI, t)
      ).left
  }

  /**
    * Constructs an OMF Structure as part of resolving its representation as an OWL Class
    * @param dt
    * @param store
    * @return
    */
  def createModelStructuredDataType
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   dt: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Structure
  = for {
    n <- getFragment(dt.getIRI)
    u = api.taggedTypes.structureUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createModelStructuredDataType(dt, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, dt.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelStructuredDataType
  (dt: OWLClass, name: LocalName, uuid: api.taggedTypes.StructureUUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Structure
  = iri2typeTerm
    .get(dt.getIRI)
    .fold[OMFError.Throwables \/ Structure]{
    val _st = terms.Structure(dt, dt.getIRI, name, uuid)
    sig.structuredDataTypes += _st
    iri2typeTerm += dt.getIRI -> _st
    \/-(_st)
  } {
    case t: Structure =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.StructuredDataType, dt.getIRI, t)
      ).left
  }

  def addStructuredDataType
  (structuredDataTypeIRI: IRI, name: LocalName, uuid: api.taggedTypes.StructureUUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Structure
  = iri2typeTerm
    .get(structuredDataTypeIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#Structure]{
    val structuredDataTypeC = owlDataFactory
      .getOWLClass(structuredDataTypeIRI)
    for {
      result <- createModelStructuredDataType(structuredDataTypeC, name, uuid)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont,
            owlDataFactory.getOWLDeclarationAxiom(
              structuredDataTypeC,
              createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont,
            owlDataFactory.getOWLSubClassOfAxiom(
              structuredDataTypeC,
              backbone.StructuredDatatypeC,
              createOMLProvenanceAnnotations(uuid)))
        ),
        "addStructuredDataType error")
    } yield result
  } {
    case t: OWLAPIOMF#Structure =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.StructuredDataType, structuredDataTypeIRI, t)
      ).left
  }

  /**
    * Constructs an OMF ScalarOneOfRestriction as part of resolving its representation as an OWL Datatype
    * @param restrictionDT
    * @param restrictedRange
    * @param store
    * @return
    */
  def createScalarOneOfRestriction
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = api.taggedTypes.scalarOneOfRestrictionUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createScalarOneOfRestriction(
      restrictionDT, n, u, restrictedRange)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createScalarOneOfRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: api.taggedTypes.ScalarOneOfRestrictionUUID,
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfRestriction
  = iri2typeTerm
    .get(restrictionDT.getIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfRestriction] {
    val _rdt = terms.ScalarOneOfRestriction(
      restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange)
    sig.scalarOneOfRestrictions += _rdt
    iri2typeTerm += restrictionDT.getIRI -> _rdt
    _rdt.right
  } {
    case t: OWLAPIOMF#ScalarOneOfRestriction =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.ScalarOneOfRestriction, restrictionDT.getIRI, t)
      ).left
  }

  def addScalarOneOfRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: api.taggedTypes.ScalarOneOfRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfRestriction
  = iri2typeTerm
    .get(dataTypeIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfRestriction]{
    val restrictionDT = owlDataFactory
      .getOWLDatatype(dataTypeIRI)
    for {
      result <- createScalarOneOfRestriction(restrictionDT, name, uuid, restrictedRange)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            restrictionDT,
            createOMLProvenanceAnnotations(uuid)))
        ),
        "addScalarOneOfRestriction error")
    } yield {
      iri2typeTerm += restrictionDT.getIRI -> result
      result
    }
  } {
    case t: OWLAPIOMF#ScalarOneOfRestriction =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.ScalarOneOfRestriction, dataTypeIRI, t)
      ).left
  }

  /**
    * Constructs an OMF ScalarOneOfLiteralAxiom as part of resolving its representation as an OWL DataOneOf Axiom
    * @param scalarOneOfRestriction
    * @param value
    * @param store
    * @return
    */
  def createScalarOneOfLiteralAxiom
  (scalarOneOfRestriction: OWLAPIOMF#ScalarOneOfRestriction,
   value: tables.LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfLiteralAxiom
  = for {
    axiomUUID <- scalarOneOfLiteralAxiomUUID(this, scalarOneOfRestriction, value)
    ax <- createScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value, valueType)
  } yield ax

  def createScalarOneOfLiteralAxiom
  (axiomUUID: api.taggedTypes.ScalarOneOfLiteralAxiomUUID,
   scalarOneOfRestriction: OWLAPIOMF#ScalarOneOfRestriction,
   value: tables.LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfLiteralAxiom
  = {
    val ax = types.termAxioms.ScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value, valueType)
    sig.scalarOneOfLiterals += ax
    ax.right
  }

  def addScalarOneOfLiteralAxiom
  (axiomUUID: api.taggedTypes.ScalarOneOfLiteralAxiomUUID,
   scalarOneOfRestriction: OWLAPIOMF#ScalarOneOfRestriction,
   value: tables.LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfLiteralAxiom
  = for {
    axiom <- createScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value, valueType)
    restrictionDT = scalarOneOfRestriction.e
    restrictedDT = scalarOneOfRestriction.restrictedDataRange.e
    literalValue = LiteralConversions.toOWLLiteral(value, owlDataFactory,
      valueType.map(_.e).orElse(Option.apply(restrictedDT)))
    result <- ont
      .datatypeDefinitions(restrictionDT)
      .toScala[Set]
      .headOption
      .fold[OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfLiteralAxiom]{
      val ax = new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
        restrictionDT,
        owlDataFactory.getOWLDataOneOf(literalValue),
        createOMLProvenanceAnnotations(uuid)))
      for {
        _ <- applyOntologyChanges(ontManager,
          Seq(ax),
          "addScalarOneOfLiteralAxiom error")
      } yield axiom
    } { restrictionDF =>
      restrictionDF.getDataRange match {
        case dof: OWLDataOneOf =>
          val values = java.util.stream.Stream.concat(
            dof.values,
            java.util.stream.Stream.of[OWLLiteral](literalValue))
          val ax = new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDataOneOf(values),
            createOMLProvenanceAnnotations(uuid)))
          for {
            _ <- applyOntologyChanges(ontManager,
              Seq(
                new RemoveAxiom(ont, restrictionDF),
                ax
              ),
              "addScalarOneOfLiteralAxiom error")
          } yield axiom
        case _ =>
          Set[java.lang.Throwable](
            OMFError.omfError(s"addScalarOneOfLiteralAxiom error: $restrictionDF")
          ).left
      }
    }
  } yield result

  //

  /**
    * Constructs an OMF BinaryScalarRestiction as part of resolving its representation as an OWL Datatype restriction
    * per OWL2 section 4.5
    * @see https://www.w3.org/TR/owl2-syntax/#Binary_Data
    * @param restrictionDT
    * @param restrictedRange
    * @param length
    * @param minLength
    * @param maxLength
    * @param store
    * @return
    */
  def createBinaryScalarRestriction
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#BinaryScalarRestriction
  = for {
      n <- getFragment(restrictionDT.getIRI)
      u = api.taggedTypes.binaryScalarRestrictionUUID(generateUUIDFromString(tboxUUID, "name" -> n))
      term <- createBinaryScalarRestriction(
        restrictionDT, n, u, restrictedRange,
        length, minLength, maxLength)
      aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
      _ <- store.ops.addAnnotationAssertions(this, term, aas)
    } yield term

  def createBinaryScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: api.taggedTypes.BinaryScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#BinaryScalarRestriction
  = {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#BinaryScalarRestriction] {
      if (store.isBuiltInDatatypeMapConstructed || store.isBinaryKind(restrictedRange)) {
        val _rdt = terms.BinaryScalarRestriction(
          restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
          length, minLength, maxLength)
        sig.binaryScalarRestrictions += _rdt
        iri2typeTerm += restrictionDT.getIRI -> _rdt
        _rdt.right
      } else
        Set[java.lang.Throwable](OMFError.omfError(
          s"createBinaryScalarRestriction: restricted data range must be binary per OWL2 section 4.5: $restrictedRange")
        ).left
    } {
      case t: OWLAPIOMF#BinaryScalarRestriction =>
        t.right
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.BinaryScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  }

  def addBinaryScalarRestriction
  (dataTypeIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.BinaryScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#BinaryScalarRestriction
  = {
    val restrictionDT = owlDataFactory.getOWLDatatype(dataTypeIRI)
    for {
      rdr <- createBinaryScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        length, minLength, maxLength)
      result <- addBinaryScalarRestriction(restrictionDT, rdr)
    } yield result
  }

  def addBinaryScalarRestriction
  (restrictionDT: OWLDatatype,
   rdr: OWLAPIOMF#BinaryScalarRestriction)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#BinaryScalarRestriction
  = for {
    _ <- applyOntologyChangesOrNoOp(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
          restrictionDT,
          createOMLProvenanceAnnotations(uuid)))
      ) ++ rdr.length.map { l =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(OWLFacet.LENGTH, Integer.parseInt(l))),
          createOMLProvenanceAnnotations(uuid)))
      } ++ rdr.minLength.map { minL =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(OWLFacet.MIN_LENGTH, Integer.parseInt(minL))),
          createOMLProvenanceAnnotations(uuid)))
      } ++ rdr.maxLength.map { maxL =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, Integer.parseInt(maxL))),
          createOMLProvenanceAnnotations(uuid)))
      },
      s"addBinaryScalarRestriction error: ${restrictionDT.getIRI}")
    _ = iri2typeTerm += restrictionDT.getIRI -> rdr
    _ = sig.binaryScalarRestrictions += rdr
  } yield rdr

  // IRIScalarRestriction

  /**
    * Constructs an OMF IRIScalarRestriction as part of resolving its representation as an OWL Datatype restriction
    * per OWL2 section 4.6
    * @see https://www.w3.org/TR/owl2-syntax/#IRIs_2
    * @param restrictionDT
    * @param restrictedRange
    * @param length
    * @param minLength
    * @param maxLength
    * @param pattern
    * @param store
    * @return
    */
  def createIRIScalarRestriction
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#IRIScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = api.taggedTypes.iriScalarRestrictionUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createIRIScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      length, minLength, maxLength, pattern)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createIRIScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName,
   uuid: api.taggedTypes.IRIScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#IRIScalarRestriction
  = {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#IRIScalarRestriction] {
      if (store.isBuiltInDatatypeMapConstructed || store.isIRIKind(restrictedRange)) {
        val _rdt = terms.IRIScalarRestriction(
          restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
          length, minLength, maxLength, pattern)
        sig.iriScalarRestrictions += _rdt
        iri2typeTerm += restrictionDT.getIRI -> _rdt
        _rdt.right
      } else
        Set[java.lang.Throwable](OMFError.omfError(
          s"createBinaryScalarRestriction: restricted data range must be IRI per OWL2 section 4.6: $restrictedRange")
        ).left
    } {
      case t: OWLAPIOMF#IRIScalarRestriction =>
        t.right
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.IRIScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  }

  def addIRIScalarRestriction
  (dataTypeIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.IRIScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#IRIScalarRestriction
  = {
    val restrictionDT = owlDataFactory.getOWLDatatype(dataTypeIRI)
    for {
      rdr <- createIRIScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        length, minLength, maxLength, pattern)
      result <- addIRIScalarRestriction(restrictionDT, rdr)
    } yield result
  }

  def addIRIScalarRestriction
  (restrictionDT: OWLDatatype,
   rdr: OWLAPIOMF#IRIScalarRestriction)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#IRIScalarRestriction
  = for {
    _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            restrictionDT,
            createOMLProvenanceAnnotations(uuid)))
        ) ++ rdr.length.map { l =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(OWLFacet.LENGTH, Integer.parseInt(l))),
            createOMLProvenanceAnnotations(uuid)))
        } ++ rdr.minLength.map { minL =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(OWLFacet.MIN_LENGTH, Integer.parseInt(minL))),
            createOMLProvenanceAnnotations(uuid)))
        } ++ rdr.maxLength.map { maxL =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, Integer.parseInt(maxL))),
            createOMLProvenanceAnnotations(uuid)))
        } ++ rdr.pattern.map { patt =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.PATTERN,
                owlDataFactory.getOWLLiteral(patt, owlDataFactory.getStringOWLDatatype))),
            createOMLProvenanceAnnotations(uuid)))
        },
        s"addIRIScalarRestriction error: ${restrictionDT.getIRI}")
    _ = iri2typeTerm += restrictionDT.getIRI -> rdr
    _ = sig.iriScalarRestrictions += rdr
  } yield rdr

  // NumericScalarRestriction

  /**
    * Constructs an OMF NumericScalarRestriction as part of resolving its representation as an OWL Datatype restriction
    * per OWL2 sections 4.1, 4.2
    * @see https://www.w3.org/TR/owl2-syntax/#Real_Numbers.2C_Decimal_Numbers.2C_and_Integers
    * @see https://www.w3.org/TR/owl2-syntax/#Floating-Point_Numbers
    * @param restrictionDT
    * @param restrictedRange
    * @param minInclusive
    * @param maxInclusive
    * @param minExclusive
    * @param maxExclusive
    * @param store
    * @return
    */
  def createNumericScalarRestriction
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[tables.LiteralNumber],
   maxInclusive: Option[tables.LiteralNumber],
   minExclusive: Option[tables.LiteralNumber],
   maxExclusive: Option[tables.LiteralNumber])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#NumericScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = api.taggedTypes.numericScalarRestrictionUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createNumericScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      minInclusive, maxInclusive, minExclusive, maxExclusive)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createNumericScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName,
   uuid: api.taggedTypes.NumericScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[tables.LiteralNumber],
   maxInclusive: Option[tables.LiteralNumber],
   minExclusive: Option[tables.LiteralNumber],
   maxExclusive: Option[tables.LiteralNumber])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#NumericScalarRestriction
  = {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#NumericScalarRestriction] {
      if (store.isBuiltInDatatypeMapConstructed || store.isNumericKind(restrictedRange)) {
        val _rdt = terms.NumericScalarRestriction(
          restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
          minInclusive, maxInclusive, minExclusive, maxExclusive)
        sig.numericScalarRestrictions += _rdt
        iri2typeTerm += restrictionDT.getIRI -> _rdt
        _rdt.right
      } else
        Set[java.lang.Throwable](OMFError.omfError(
          s"createBinaryScalarRestriction: restricted data range must be numeric per OWL2 sections 4.1, 4.2: $restrictedRange")
        ).left
    } {
      case t: OWLAPIOMF#NumericScalarRestriction =>
        t.right
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.NumericScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  }

  def addNumericScalarRestriction
  (dataTypeIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.NumericScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[tables.LiteralNumber],
   maxInclusive: Option[tables.LiteralNumber],
   minExclusive: Option[tables.LiteralNumber],
   maxExclusive: Option[tables.LiteralNumber])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#NumericScalarRestriction
  = {
    val restrictionDT = owlDataFactory.getOWLDatatype(dataTypeIRI)
    for {
      rdr <- createNumericScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        minInclusive, maxInclusive, minExclusive, maxExclusive)
      result <- addNumericScalarRestriction(restrictionDT, rdr)
    } yield result
  }

  def addNumericScalarRestriction
  (restrictionDT: OWLDatatype,
   rdr: OWLAPIOMF#NumericScalarRestriction)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#NumericScalarRestriction
  = for {
    _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            restrictionDT,
            createOMLProvenanceAnnotations(uuid)))
        ) ++ rdr.minInclusive.map { minI =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.MIN_INCLUSIVE,
                LiteralConversions.toOWLLiteral(minI, owlDataFactory, Option.apply(rdr.restrictedDataRange.e)))),
            createOMLProvenanceAnnotations(uuid)))
        } ++ rdr.maxInclusive.map { maxI =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.MAX_INCLUSIVE,
                LiteralConversions.toOWLLiteral(maxI, owlDataFactory, Option.apply(rdr.restrictedDataRange.e)))),
            createOMLProvenanceAnnotations(uuid)))
        } ++ rdr.minExclusive.map { minE =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.MIN_EXCLUSIVE,
                LiteralConversions.toOWLLiteral(minE, owlDataFactory, Option.apply(rdr.restrictedDataRange.e)))),
            createOMLProvenanceAnnotations(uuid)))
        } ++ rdr.maxExclusive.map { maxE =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.MAX_EXCLUSIVE,
                LiteralConversions.toOWLLiteral(maxE, owlDataFactory, Option.apply(rdr.restrictedDataRange.e)))),
            createOMLProvenanceAnnotations(uuid)))
        },
        s"addNumericScalarRestriction error: ${restrictionDT.getIRI}")
    _ = iri2typeTerm += restrictionDT.getIRI -> rdr
    _ = sig.numericScalarRestrictions += rdr
  } yield rdr

  // PlainLiteralScalarRestriction

  /**
    * Constructs an OMF PlainLiteral restriction as part of resolving its representation as an OWL Datatype restriction
    * per OWL2 section 4.3
    * @see https://www.w3.org/TR/owl2-syntax/#Strings
    * @param restrictionDT
    * @param restrictedRange
    * @param length
    * @param minLength
    * @param maxLength
    * @param pattern
    * @param language
    * @param store
    * @return
    */
  def createPlainLiteralScalarRestriction
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern],
   language: Option[tables.taggedTypes.LanguageTagDataType])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#PlainLiteralScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = api.taggedTypes.plainLiteralScalarRestrictionUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createPlainLiteralScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      length, minLength, maxLength, pattern, language)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createPlainLiteralScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName,
   uuid: api.taggedTypes.PlainLiteralScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern],
   language: Option[tables.taggedTypes.LanguageTagDataType])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#PlainLiteralScalarRestriction
  = {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#PlainLiteralScalarRestriction] {
      if (store.isBuiltInDatatypeMapConstructed || store.isPlainLiteralKind(restrictedRange)) {
        val _rdt = terms.PlainLiteralScalarRestriction(
          restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
          length, minLength, maxLength, pattern, language)
        sig.plainLiteralScalarRestrictions += _rdt
        iri2typeTerm += restrictionDT.getIRI -> _rdt
        _rdt.right
      } else
        Set[java.lang.Throwable](OMFError.omfError(
          s"createBinaryScalarRestriction: restricted data range must be plain literal per OWL2 section 4.3: $restrictedRange")
        ).left
    } {
      case t: OWLAPIOMF#PlainLiteralScalarRestriction =>
        t.right
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.PlainLiteralScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  }

  def addPlainLiteralScalarRestriction
  (dataTypeIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.PlainLiteralScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern],
   language: Option[tables.taggedTypes.LanguageTagDataType])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#PlainLiteralScalarRestriction
  = {
    val restrictionDT = owlDataFactory.getOWLDatatype(dataTypeIRI)
    for {
      rdr <- createPlainLiteralScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        length, minLength, maxLength, pattern, language)
      result <- addPlainLiteralScalarRestriction(restrictionDT, rdr)
    } yield result
  }

  def addPlainLiteralScalarRestriction
  (restrictionDT: OWLDatatype,
   rdr: OWLAPIOMF#PlainLiteralScalarRestriction)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#PlainLiteralScalarRestriction
  = for {
  _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            restrictionDT, createOMLProvenanceAnnotations(uuid)))
        ) ++ rdr.length.map { l =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.LENGTH,
                Integer.parseInt(l))),
            createOMLProvenanceAnnotations(uuid)))
        } ++ rdr.minLength.map { minL =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.MIN_LENGTH,
                Integer.parseInt(minL))),
            createOMLProvenanceAnnotations(uuid)))
        } ++ rdr.maxLength.map { maxL =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.MAX_LENGTH,
                Integer.parseInt(maxL))),
            createOMLProvenanceAnnotations(uuid)))
        } ++ rdr.pattern.map { patt =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.PATTERN,
                owlDataFactory.getOWLLiteral(patt, owlDataFactory.getStringOWLDatatype))),
            createOMLProvenanceAnnotations(uuid)))
        } ++ rdr.language.map { lang =>
          new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.LANG_RANGE,
                owlDataFactory.getOWLLiteral(lang))),
            createOMLProvenanceAnnotations(uuid)))
        },
        s"addPlainLiteralScalarRestriction error: ${restrictionDT.getIRI}")
    _ = iri2typeTerm += restrictionDT.getIRI -> rdr
    _ = sig.plainLiteralScalarRestrictions += rdr
  } yield rdr

  // StringScalarRestriction

  /**
    * Constructs an OMF String restriction as part of resolving its representation as an OWL Datatype restriction
    * per OWL2 section 4.3
    * @see https://www.w3.org/TR/owl2-syntax/#Strings
    * @param restrictionDT
    * @param restrictedRange
    * @param length
    * @param minLength
    * @param maxLength
    * @param pattern
    * @param store
    * @return
    */
  def createStringScalarRestriction
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StringScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = api.taggedTypes.stringScalarRestrictionUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createStringScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      length, minLength, maxLength, pattern)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createStringScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName,
   uuid: api.taggedTypes.StringScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StringScalarRestriction
  = {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#StringScalarRestriction] {
      if (store.isBuiltInDatatypeMapConstructed || store.isStringKind(restrictedRange)) {
        val _rdt = terms.StringScalarRestriction(
          restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
          length, minLength, maxLength, pattern)
        sig.stringScalarRestrictions += _rdt
        iri2typeTerm += restrictionDT.getIRI -> _rdt
        _rdt.right
      } else
        Set[java.lang.Throwable](OMFError.omfError(
          s"createBinaryScalarRestriction: ${restrictionDT.getIRI} restricted data range must be string per OWL2 section 4.3: $restrictedRange")
        ).left
    } {
      case t: OWLAPIOMF#StringScalarRestriction =>
        t.right
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.StringScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  }

  def addStringScalarRestriction
  (dataTypeIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.StringScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.taggedTypes.PositiveIntegerLiteral],
   minLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   maxLength: Option[tables.taggedTypes.PositiveIntegerLiteral],
   pattern: Option[tables.taggedTypes.LiteralPattern])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StringScalarRestriction
  = {
    val restrictionDT = owlDataFactory.getOWLDatatype(dataTypeIRI)
    for {
      rdr <- createStringScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        length, minLength, maxLength, pattern)
      result <- addStringScalarRestriction(restrictionDT, rdr)
    } yield result
  }

  def addStringScalarRestriction
  (restrictionDT: OWLDatatype,
   rdr: OWLAPIOMF#StringScalarRestriction)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StringScalarRestriction
  = for {
    _ <- applyOntologyChangesOrNoOp(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
          restrictionDT, createOMLProvenanceAnnotations(uuid)))
      ) ++ rdr.length.map { l =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(OWLFacet.LENGTH, Integer.parseInt(l))),
          createOMLProvenanceAnnotations(uuid)))
      } ++ rdr.minLength.map { minL =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(OWLFacet.MIN_LENGTH, Integer.parseInt(minL))),
          createOMLProvenanceAnnotations(uuid)))
      } ++ rdr.maxLength.map { maxL =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, Integer.parseInt(maxL))),
          createOMLProvenanceAnnotations(uuid)))
      } ++ rdr.pattern.map { patt =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(
              OWLFacet.PATTERN,
              owlDataFactory.getOWLLiteral(patt))),
          createOMLProvenanceAnnotations(uuid)))
      },
      s"addStringScalarRestriction error: ${restrictionDT.getIRI}")
    _ = iri2typeTerm += restrictionDT.getIRI -> rdr
    _ = sig.stringScalarRestrictions += rdr
  } yield rdr

  // SynonymScalarRestriction

  /**
    * Constructs an OMF Synonym restriction as part of resolving its representation as an OWL Datatype synonym
    * per OWL2 section 9.4
    * @see https://www.w3.org/TR/owl2-syntax/#Strings
    * @param restrictionDT
    * @param restrictedRange
    * @param store
    * @return
    */
  def createSynonymScalarRestriction
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#SynonymScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = api.taggedTypes.synonymScalarRestrictionUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createSynonymScalarRestriction(
      restrictionDT, n, u, restrictedRange)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createSynonymScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName,
   uuid: api.taggedTypes.SynonymScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#SynonymScalarRestriction
  = iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#SynonymScalarRestriction] {
    val _rdt = terms.SynonymScalarRestriction(
      restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange)
    sig.synonymScalarRestrictions += _rdt
    iri2typeTerm += restrictionDT.getIRI -> _rdt
    _rdt.right
  } {
    case t: OWLAPIOMF#SynonymScalarRestriction =>
      t.right
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.SynonymScalarRestriction, restrictionDT.getIRI, t)
      ).left
  }

  def addSynonymScalarRestriction
  (dataTypeIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.SynonymScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#SynonymScalarRestriction
  = {
    val restrictionDT = owlDataFactory.getOWLDatatype(dataTypeIRI)
    for {
      rdr <- createSynonymScalarRestriction(
        restrictionDT, name, uuid, restrictedRange)
      result <- addSynonymScalarRestriction(restrictionDT, rdr)
    } yield result
  }

  def addSynonymScalarRestriction
  (restrictionDT: OWLDatatype,
   rdr: OWLAPIOMF#SynonymScalarRestriction)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#SynonymScalarRestriction
  = for {
    _ <- applyOntologyChangesOrNoOp(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
          restrictionDT,
          createOMLProvenanceAnnotations(uuid))),
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          rdr.restrictedDataRange.e,
          createOMLProvenanceAnnotations(uuid)))
      ),
      s"addSynonymScalarRestriction error: ${restrictionDT.getIRI}")
    _ = iri2typeTerm += restrictionDT.getIRI -> rdr
    _ = sig.synonymScalarRestrictions += rdr
  } yield rdr

  // TimeScalarRestriction

  /**
    * Constructs an OMF Time restriction as part of resolving its representation as an OWL Datatype restriction
    * per OWL2 section 4.7
    * @see https://www.w3.org/TR/owl2-syntax/#Time_Instants
    * @param restrictionDT
    * @param restrictedRange
    * @param minInclusive
    * @param maxInclusive
    * @param minExclusive
    * @param maxExclusive
    * @param store
    * @return
    */
  def createTimeScalarRestriction
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[tables.LiteralDateTime],
   maxInclusive: Option[tables.LiteralDateTime],
   minExclusive: Option[tables.LiteralDateTime],
   maxExclusive: Option[tables.LiteralDateTime])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#TimeScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = api.taggedTypes.timeScalarRestrictionUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createTimeScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      minInclusive, maxInclusive, minExclusive, maxExclusive)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createTimeScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName,
   uuid: api.taggedTypes.TimeScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[tables.LiteralDateTime],
   maxInclusive: Option[tables.LiteralDateTime],
   minExclusive: Option[tables.LiteralDateTime],
   maxExclusive: Option[tables.LiteralDateTime])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#TimeScalarRestriction
  = {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#TimeScalarRestriction] {
      if (store.isBuiltInDatatypeMapConstructed || store.isTimeKind(restrictedRange)) {
        val _rdt = terms.TimeScalarRestriction(
          restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
          minInclusive, maxInclusive, minExclusive, maxExclusive)
        sig.timeScalarRestrictions += _rdt
        iri2typeTerm += restrictionDT.getIRI -> _rdt
        _rdt.right
      } else
        Set[java.lang.Throwable](OMFError.omfError(
          s"createTimeScalarRestriction: restricted data range must be time per OWL2 section 4.7: $restrictedRange")
        ).left
    } {
      case t: OWLAPIOMF#TimeScalarRestriction =>
        t.right
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.TimeScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  }

  def addTimeScalarRestriction
  (dataTypeIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.TimeScalarRestrictionUUID,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[tables.LiteralDateTime],
   maxInclusive: Option[tables.LiteralDateTime],
   minExclusive: Option[tables.LiteralDateTime],
   maxExclusive: Option[tables.LiteralDateTime])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#TimeScalarRestriction
  = {
    val restrictionDT = owlDataFactory.getOWLDatatype(dataTypeIRI)
    for {
      rdr <- createTimeScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        minInclusive, maxInclusive, minExclusive, maxExclusive)
      result <- addTimeScalarRestriction(restrictionDT, rdr)
    } yield result
  }

  def addTimeScalarRestriction
  (restrictionDT: OWLDatatype,
   rdr: OWLAPIOMF#TimeScalarRestriction)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#TimeScalarRestriction
  = for {
    _ <- applyOntologyChangesOrNoOp(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
          restrictionDT,
          createOMLProvenanceAnnotations(uuid)))
      ) ++ rdr.minInclusive.map { minI =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(
              OWLFacet.MIN_INCLUSIVE,
              LiteralConversions.toOWLLiteral(minI, owlDataFactory, Option.apply(rdr.restrictedDataRange.e)))),
          createOMLProvenanceAnnotations(uuid)))
      } ++ rdr.maxInclusive.map { maxI =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(
              OWLFacet.MAX_INCLUSIVE,
              LiteralConversions.toOWLLiteral(maxI, owlDataFactory, Option.apply(rdr.restrictedDataRange.e)))),
          createOMLProvenanceAnnotations(uuid)))
      } ++ rdr.minExclusive.map { minE =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(
              OWLFacet.MIN_EXCLUSIVE,
              LiteralConversions.toOWLLiteral(minE, owlDataFactory, Option.apply(rdr.restrictedDataRange.e)))),
          createOMLProvenanceAnnotations(uuid)))
      } ++ rdr.maxExclusive.map { maxE =>
        new AddAxiom(ont, owlDataFactory.getOWLDatatypeDefinitionAxiom(
          restrictionDT,
          owlDataFactory.getOWLDatatypeRestriction(
            rdr.restrictedDataRange.e,
            owlDataFactory.getOWLFacetRestriction(
              OWLFacet.MAX_EXCLUSIVE,
              LiteralConversions.toOWLLiteral(maxE, owlDataFactory, Option.apply(rdr.restrictedDataRange.e)))),
          createOMLProvenanceAnnotations(uuid)))
      },
      s"addTimeScalarRestriction error: ${restrictionDT.getIRI}")
    _ = iri2typeTerm += restrictionDT.getIRI -> rdr
    _ = sig.timeScalarRestrictions += rdr
  } yield rdr

  def createDataRelationshipFromEntityToScalar
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   esc: OWLDataProperty,
   isIdentityCriteria: Boolean,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataProperty
  = for {
    n <- getFragment(esc.getIRI)
    u = api.taggedTypes.entityScalarDataPropertyUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createDataRelationshipFromEntityToScalar(esc, n, isIdentityCriteria, u, source, target)
    aas = getRelevantSubjectAnnotationAssertions(ont, esc.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createDataRelationshipFromEntityToScalar
  (esc: OWLDataProperty, name: LocalName, isIdentityCriteria: Boolean,
   uuid: api.taggedTypes.EntityScalarDataPropertyUUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataProperty
  = {
    val escIRI: IRI = esc.getIRI
    iri2typeTerm
      .get(escIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataProperty] {
      val _esc = terms.EntityScalarDataProperty(esc, esc.getIRI, name, isIdentityCriteria, uuid, source, target)
      sig.entityScalarDataProperties += _esc
      iri2typeTerm += escIRI -> _esc
      _esc.right
    } {
      case t: OWLAPIOMF#EntityScalarDataProperty =>
        t.right
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.DataRelationshipFromEntityToScalar, escIRI, t)
        ).left
    }
  }

  protected def makeDataRelationshipFromEntityToScalar
  (dIRI: IRI, name: LocalName, isIdentityCriteria: Boolean,
   uuid: api.taggedTypes.EntityScalarDataPropertyUUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataProperty
  = {
    val escDP = owlDataFactory.getOWLDataProperty(dIRI)
    for {
      term <- createDataRelationshipFromEntityToScalar(escDP, name, isIdentityCriteria, uuid, source, target)
      func = if (isIdentityCriteria)
        Option(new AddAxiom(ont, owlDataFactory.getOWLFunctionalDataPropertyAxiom(
          escDP,
          createOMLProvenanceAnnotations(uuid))))
      else
        Option.empty
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            escDP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubDataPropertyOfAxiom(
            escDP, backbone.topDataPropertyDP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLDataPropertyDomainAxiom(
            escDP, source.e,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLDataPropertyRangeAxiom(
            escDP, owlDataFactory.getOWLDatatype(target.iri),
            createOMLProvenanceAnnotations(uuid)))
        ) ++ func,
        "makeDataRelationshipFromEntityToScalar")
    } yield term
  }

  def addDataRelationshipFromEntityToScalar
  (dIRI: IRI, name: LocalName, isIdentityCriteria: Boolean,
   uuid: api.taggedTypes.EntityScalarDataPropertyUUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ EntityScalarDataProperty
  = iri2typeTerm
    .get(dIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataProperty]({
    (isTypeTermDefinedRecursively(source),
      isTypeTermDefinedRecursively(target)) match {
      case (true, true) =>
        makeDataRelationshipFromEntityToScalar(dIRI, name, isIdentityCriteria, uuid, source, target)
      case (false, true) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromEntityToScalar, dIRI,
            Map(RelationshipScopeAccessKind.Source -> source))
        ).left
      case (true, false) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromEntityToScalar, dIRI,
            Map(RelationshipScopeAccessKind.Target -> target))
        ).left
      case (false, false) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromEntityToScalar, dIRI,
            Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
        ).left
    }
  }) { term =>
    Set(
      entityConflictException(ElementExceptionKind.DataRelationshipFromEntityToScalar, dIRI, term)
    ).left
  }

  def createDataRelationshipFromEntityToStructure
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   esc: OWLObjectProperty,
   isIdentityCriteria: Boolean,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataProperty
  = for {
    n <- getFragment(esc.getIRI)
    u = api.taggedTypes.entityStructuredDataPropertyUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createDataRelationshipFromEntityToStructure(esc, n, isIdentityCriteria, u, source, target)
  } yield term

  def createDataRelationshipFromEntityToStructure
  (esc: OWLObjectProperty, name: LocalName, isIdentityCriteria: Boolean,
   uuid: api.taggedTypes.EntityStructuredDataPropertyUUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataProperty
  = {
    val escIRI: IRI = esc.getIRI
    iri2typeTerm
      .get(escIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataProperty] {
      val _esc = terms.EntityStructuredDataProperty(esc, esc.getIRI, name, isIdentityCriteria, uuid, source, target)
      sig.entityStructuredDataProperties += _esc
      iri2typeTerm += escIRI -> _esc
      _esc.right
    } {
      case t: OWLAPIOMF#EntityStructuredDataProperty =>
        t.right
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.DataRelationshipFromEntityToStructure, escIRI, t)
        ).left
    }
  }

  protected def makeDataRelationshipFromEntityToStructure
  (dIRI: IRI, name: LocalName, isIdentityCriteria: Boolean,
   uuid: api.taggedTypes.EntityStructuredDataPropertyUUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataProperty
  = {
    val escDP = owlDataFactory.getOWLObjectProperty(dIRI)
    for {
      term <- createDataRelationshipFromEntityToStructure(escDP, name, isIdentityCriteria, uuid, source, target)
      func = if (isIdentityCriteria)
        Option(new AddAxiom(ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom(
          escDP,
          createOMLProvenanceAnnotations(uuid))))
      else
        Option.empty
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            escDP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
            escDP, backbone.topReifiedStructuredDataPropertyOP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
            escDP, source.e,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
            escDP, target.e,
            createOMLProvenanceAnnotations(uuid)))
        ) ++ func,
        "makeDataRelationshipFromEntityToStructure")
    } yield term
  }

  def addDataRelationshipFromEntityToStructure
  (dIRI: IRI, name: LocalName, isIdentityCriteria: Boolean,
   uuid: api.taggedTypes.EntityStructuredDataPropertyUUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataProperty
  = iri2typeTerm
    .get(dIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataProperty]({
    (isTypeTermDefinedRecursively(source),
      isTypeTermDefinedRecursively(target)) match {
      case (true, true) =>
        makeDataRelationshipFromEntityToStructure(dIRI, name, isIdentityCriteria, uuid, source, target)
      case (false, true) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromEntityToStructure, dIRI,
            Map(RelationshipScopeAccessKind.Source -> source))
        ).left
      case (true, false) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromEntityToStructure, dIRI,
            Map(RelationshipScopeAccessKind.Target -> target))
        ).left
      case (false, false) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromEntityToStructure, dIRI,
            Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
        ).left
    }
  }) { term =>
    Set(
      entityConflictException(ElementExceptionKind.DataRelationshipFromEntityToStructure, dIRI, term)
    ).left
  }

  def createDataRelationshipFromStructureToScalar
  (tboxUUID: api.taggedTypes.ScalarDataPropertyUUID,
   esc: OWLDataProperty,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarDataProperty
  = for {
    n <- getFragment(esc.getIRI)
    u = api.taggedTypes.scalarDataPropertyUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createDataRelationshipFromStructureToScalar(esc, n, u, source, target)
  } yield term

  def createDataRelationshipFromStructureToScalar
  (esc: OWLDataProperty, name: LocalName,
   uuid: api.taggedTypes.ScalarDataPropertyUUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarDataProperty
  = {
    val escIRI: IRI = esc.getIRI
    iri2typeTerm
      .get(escIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#ScalarDataProperty] {
      val _esc = terms.ScalarDataProperty(esc, esc.getIRI, name, uuid, source, target)
      sig.scalarDataProperties += _esc
      iri2typeTerm += escIRI -> _esc
      _esc.right
    } {
      case t: OWLAPIOMF#ScalarDataProperty =>
        t.right
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.DataRelationshipFromStructureToScalar, escIRI, t)
        ).left
    }
  }

  protected def makeDataRelationshipFromStructureToScalar
  (dIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.ScalarDataPropertyUUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarDataProperty
  = {
    val escDP = owlDataFactory.getOWLDataProperty(dIRI)
    for {
      term <- createDataRelationshipFromStructureToScalar(escDP, name, uuid, source, target)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            escDP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubDataPropertyOfAxiom(
            escDP, backbone.topDataPropertyDP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLDataPropertyDomainAxiom(
            escDP, source.e,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLDataPropertyRangeAxiom(
            escDP, target.e,
            createOMLProvenanceAnnotations(uuid)))
        ),
        "makeDataRelationshipFromStructureToScalar")
    } yield term
  }

  def addDataRelationshipFromStructureToScalar
  (dIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.ScalarDataPropertyUUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarDataProperty
  = iri2typeTerm
    .get(dIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#ScalarDataProperty]({
    (isTypeTermDefinedRecursively(source),
      isTypeTermDefinedRecursively(target)) match {
      case (true, true) =>
        makeDataRelationshipFromStructureToScalar(dIRI, name, uuid, source, target)
      case (false, true) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromStructureToScalar, dIRI,
            Map(RelationshipScopeAccessKind.Source -> source))
        ).left
      case (true, false) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromStructureToScalar, dIRI,
            Map(RelationshipScopeAccessKind.Target -> target))
        ).left
      case (false, false) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromStructureToScalar, dIRI,
            Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
        ).left
    }
  }) { term =>
    Set(
      entityConflictException(ElementExceptionKind.DataRelationshipFromStructureToScalar, dIRI, term)
    ).left
  }

  def createDataRelationshipFromStructureToStructure
  (tboxUUID: api.taggedTypes.TerminologyBoxUUID,
   esc: OWLObjectProperty,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StructuredDataProperty
  = for {
    n <- getFragment(esc.getIRI)
    u = api.taggedTypes.structuredDataPropertyUUID(generateUUIDFromString(tboxUUID, "name" -> n))
    term <- createDataRelationshipFromStructureToStructure(esc, n, u, source, target)
  } yield term

  def createDataRelationshipFromStructureToStructure
  (esc: OWLObjectProperty, name: LocalName,
   uuid: api.taggedTypes.StructuredDataPropertyUUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StructuredDataProperty
  = {
    val escIRI: IRI = esc.getIRI
    iri2typeTerm
      .get(escIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#StructuredDataProperty] {
      val _esc = terms.StructuredDataProperty(esc, esc.getIRI, name, uuid, source, target)
      sig.structuredDataProperties += _esc
      iri2typeTerm += escIRI -> _esc
      _esc.right
    } {
      case t: OWLAPIOMF#StructuredDataProperty =>
        t.right
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.DataRelationshipFromStructureToStructure, escIRI, t)
        ).left
    }
  }

  protected def makeDataRelationshipFromStructureToStructure
  (dIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.StructuredDataPropertyUUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StructuredDataProperty
  = {
    val escDP = owlDataFactory.getOWLObjectProperty(dIRI)
    for {
      term <- createDataRelationshipFromStructureToStructure(escDP, name, uuid, source, target)
      anns = createOMLProvenanceAnnotations(uuid)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            escDP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
            escDP, backbone.topReifiedStructuredDataPropertyOP,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
            escDP, source.e,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
            escDP, target.e,
            createOMLProvenanceAnnotations(uuid)))
        ),
        s"makeDataRelationshipFromStructureToStucture")
    } yield term
  }

  def addDataRelationshipFromStructureToStructure
  (dIRI: IRI, name: LocalName,
   uuid: api.taggedTypes.StructuredDataPropertyUUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StructuredDataProperty
  = iri2typeTerm
    .get(dIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#StructuredDataProperty]({
    (isTypeTermDefinedRecursively(source),
      isTypeTermDefinedRecursively(target)) match {
      case (true, true) =>
        makeDataRelationshipFromStructureToStructure(dIRI, name, uuid, source, target)
      case (false, true) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromStructureToStructure, dIRI,
            Map(RelationshipScopeAccessKind.Source -> source))
        ).left
      case (true, false) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromStructureToStructure, dIRI,
            Map(RelationshipScopeAccessKind.Target -> target))
        ).left
      case (false, false) =>
        Set(
          entityScopeException(ElementExceptionKind.DataRelationshipFromStructureToStructure, dIRI,
            Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
        ).left
    }
  }) { term =>
    Set(
      entityConflictException(ElementExceptionKind.DataRelationshipFromStructureToStructure, dIRI, term)
    ).left
  }

  protected def findSegmentPredicate
  (seg: OWLAPIOMF#RuleBodySegment)
  : OMFError.Throwables \/ OWLAPIOMF#SegmentPredicate
  = sig.segmentPredicates.find(_.bodySegment == seg) match {
    case Some(p) =>
      p.right
    case None =>
      Set(
        OMFError.omfError(s"There should be a SegmentPredicate for: $seg")
      ).left
  }

  @scala.annotation.tailrec
  protected final def collectSegmentPredicates
  (seg: OWLAPIOMF#RuleBodySegment,
   predicates: Seq[OWLAPIOMF#SegmentPredicate])
  : OMFError.Throwables \/ Seq[OWLAPIOMF#SegmentPredicate]
  = findSegmentPredicate(seg) match {
    case \/-(pred) =>
      val nextPredicates = predicates :+ pred
      sig.ruleBodySegments.find(_.previousSegment.contains(seg)) match {
        case Some(next) =>
          collectSegmentPredicates(next, nextPredicates)
        case None =>
          nextPredicates.right
      }
    case -\/(errors) =>
      -\/(errors)
  }

  protected def collectRuleBodyPredicates
  (rule: OWLAPIOMF#ChainRule)
  : OMFError.Throwables \/ Seq[SegmentPredicate]
  = sig.ruleBodySegments.find(_.chainRule.contains(rule)) match {
    case Some(seg) =>
      collectSegmentPredicates(seg, Seq.empty)
    case None =>
      Set(
        OMFError.omfError(s"There should be a RuleBodySegment for rule: $rule")
      ).left
  }

  @scala.annotation.tailrec
  protected final def convertBodyPredicates2Atoms
  (vIndex: Int,
   prevV: SWRLIArgument,
   predicates: Seq[SegmentPredicate],
   nextV: SWRLIArgument,
   atoms: Seq[SWRLAtom])
  : OMFError.Throwables \/ Seq[SWRLAtom]
  = if (predicates.isEmpty)
    atoms.right
  else {
    val (phead, ptail) = (predicates.head, predicates.tail)

    val range
    : OMFError.Throwables \/ (SWRLIArgument, Int)
    = if (phead.isUnary)
        (prevV -> vIndex).right
    else if (ptail.exists(_.isBinary))
       makeVariable(vIndex).map {
         _ -> (1 + vIndex)
       }
    else
      (nextV -> vIndex).right

    range match {
      case \/-((rangeV, vNext)) =>
        (phead.predicate,
         phead.reifiedRelationshipSource,
         phead.reifiedRelationshipInverseSource,
         phead.reifiedRelationshipTarget,
         phead.reifiedRelationshipInverseTarget,
         phead.unreifiedRelationshipInverse) match {

          case (Some(e: Entity), _, _, _, _, _) =>
            val p1 = owlDataFactory.getSWRLClassAtom(e.e, prevV)
            convertBodyPredicates2Atoms(vNext, rangeV, ptail, nextV, atoms :+ p1)

          case (Some(f: ForwardProperty), _, _, _, _, _) =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(f.e, prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case (Some(i: InverseProperty), _, _, _, _, _) =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(i.e, prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case (Some(ur: UnreifiedRelationship), _, _, _, _, _) =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(ur.e, prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case (None, Some(rr), _, _, _, _) =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(rr.rSource, prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case (None, _, Some(rr), _, _, _) =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(
              owlDataFactory.getOWLObjectInverseOf(rr.rSource), prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case (None, _, _, Some(rr), _, _) =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(rr.rTarget, prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case (None, _, _, _, Some(rr), _) =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(
              owlDataFactory.getOWLObjectInverseOf(rr.rTarget), prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case (None, _, _, _, _, Some(ur)) =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(
              owlDataFactory.getOWLObjectInverseOf(ur.e), prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case (pred, rS, rIS, rT, rIT, ur) =>
            Set(
              OMFError.omfError(
                s"convertBodyPredicates2Atoms failed:\n" +
                  pred.fold[String]("- no predicate\n") { p =>
                    s"- predicate: ${p.abbrevIRI}\n"
                  } +
                  rS.fold[String]("- no reifiedRelationshipSource\n") { x =>
                    s"- reifiedRelationshipSource: ${x.abbrevIRI}\n"
                  } +
                  rIS.fold[String]("- no reifiedRelationshipInverseSource\n") { x =>
                    s"- reifiedRelationshipInverseSource: ${x.abbrevIRI}\n"
                  } +
                  rT.fold[String]("- no reifiedRelationshipTarget\n") { x =>
                    s"- reifiedRelationshipTarget: ${x.abbrevIRI}\n"
                  } +
                  rIS.fold[String]("- no reifiedRelationshipInverseTarget\n") { x =>
                    s"- reifiedRelationshipInverseTarget: ${x.abbrevIRI}\n"
                  } +
                  ur.fold[String]("- no unreifiedRelationshipInverse\n") { x =>
                    s"- unreifiedRelationshipInverse: ${x.abbrevIRI}\n"
                  })
            ).left
        }
      case -\/(errors) =>
        -\/(errors)
    }
  }

  def makeChainRule
  (rule: OWLAPIOMF#ChainRule)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Unit
  = for {
    v0 <- makeVariable(0)
    v1 <- makeVariable(1)
    predicates <- collectRuleBodyPredicates(rule)
    headAtom = owlDataFactory.getSWRLObjectPropertyAtom(rule.head.e, v0, v1)
    bodyAtoms <- convertBodyPredicates2Atoms(2, v0, predicates, v1, Seq.empty)
    r = owlDataFactory.getSWRLRule(
      bodyAtoms,
      Collections.singleton(headAtom),
      createOMLProvenanceAnnotationsWithLabel(rule.name, rule.uuid))
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont, r)
      ),
      "addChainRule Error")
  } yield ()

  def createChainRule
  (e: SWRLRule,
   name: LocalName,
   head: OWLAPIOMF#UnreifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ChainRule
  = for {
    iri <- store.ops.withFragment(this.iri, name)
    u = api.taggedTypes.chainRuleUUID(generateUUIDFromString(this.uuid, "name" -> name))
    r <- iri2typeTerm.get(iri).fold[OMFError.Throwables \/ OWLAPIOMF#ChainRule] {
      val cr = types.terms.ChainRule(iri, name, u, head)
      sig.chainRules.add(cr)
      iri2typeTerm += iri -> cr
      \/-(cr)
    } {
      case cr: OWLAPIOMF#ChainRule =>
        if (cr.uuid == u)
          \/-(cr)
        else
          Set(
            entityAlreadyDefinedException(ElementExceptionKind.ChainRule, iri, cr)
          ).left
      case t =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.ChainRule, iri, t)
        ).left

    }
  } yield r

  def addChainRule
  (iri: IRI,
   uuid: api.taggedTypes.ChainRuleUUID,
   head: OWLAPIOMF#UnreifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ChainRule
  = for {
    n <- getFragment(iri)
    rule = ChainRule(iri, n, uuid, head)
    _ = sig.chainRules.add(rule)
  } yield rule

  def createRuleBodySegment
  (chainRule: Option[OWLAPIOMF#ChainRule],
   previousSegment: Option[OWLAPIOMF#RuleBodySegment])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#RuleBodySegment
  = for {
    chainRuleUUID <- chainRule match {
      case Some(cr) =>
        \/-(cr.uuid.toString)
      case None =>
        previousSegment match {
          case Some(ps) =>
            store.ops.getChainRule(ps).map(_.uuid.toString)
          case None =>
            Set(
              OMFError.omfError(
                s"createRuleBodySegment: either chainRule or previousSegment must be defined; both cannot be underfined")
            ).left
        }
    }
    positionUUID = previousSegment match {
      case None =>
        "1"
      case Some(ps) =>
        (1 + ps.position).toString
    }
    u = api.taggedTypes.ruleBodySegmentUUID(generateUUIDFromString(
      "RuleBodySegment",
      "chainRule" -> chainRuleUUID,
      "position" -> positionUUID))
    ruleBodySegment = types.terms.RuleBodySegment(u, chainRule, previousSegment)
    _ = sig.ruleBodySegments.add(ruleBodySegment)
  } yield ruleBodySegment

  def addRuleBodySegment
  (uuid: api.taggedTypes.RuleBodySegmentUUID,
   chainRule: Option[OWLAPIOMF#ChainRule],
   previousSegment: Option[OWLAPIOMF#RuleBodySegment])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#RuleBodySegment
  = {
    val rbs = RuleBodySegment(uuid, chainRule, previousSegment)
    sig.ruleBodySegments.add(rbs)
    rbs.right
  }

  def createSegmentPredicate
  (bodySegment: OWLAPIOMF#RuleBodySegment,
   predicate: Option[OWLAPIOMF#Predicate] = None,
   reifiedRelationshipSource: Option[OWLAPIOMF#ReifiedRelationship] = None,
   reifiedRelationshipInverseSource: Option[OWLAPIOMF#ReifiedRelationship] = None,
   reifiedRelationshipTarget: Option[OWLAPIOMF#ReifiedRelationship] = None,
   reifiedRelationshipInverseTarget: Option[OWLAPIOMF#ReifiedRelationship] = None,
   unreifiedRelationshipInverse: Option[OWLAPIOMF#UnreifiedRelationship] = None)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#SegmentPredicate
  = {
    val u = api.taggedTypes.segmentPredicateUUID(generateUUIDFromUUID(
      "SegmentPredicate",
      Seq.empty[(String, UUID)] ++
        Seq("bodySegment" -> bodySegment.uuid) ++
        predicate.map { vt => "predicate" -> vt.uuid } ++
        reifiedRelationshipSource.map { vt => "reifiedRelationshipSource" -> vt.uuid } ++
        reifiedRelationshipInverseSource.map { vt => "reifiedRelationshipInverseSource" -> vt.uuid } ++
        reifiedRelationshipTarget.map { vt => "reifiedRelationshipTarget" -> vt.uuid } ++
        reifiedRelationshipInverseTarget.map { vt => "reifiedRelationshipInverseTarget" -> vt.uuid } ++
        unreifiedRelationshipInverse.map { vt => "unreifiedRelationshipInverse" -> vt.uuid } : _*))
    addSegmentPredicate(
      u, bodySegment,
      predicate,
      reifiedRelationshipSource,
      reifiedRelationshipInverseSource,
      reifiedRelationshipTarget,
      reifiedRelationshipInverseTarget,
      unreifiedRelationshipInverse)
  }

  def addSegmentPredicate
  (uuid: api.taggedTypes.SegmentPredicateUUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   predicate: Option[OWLAPIOMF#Predicate],
   reifiedRelationshipSource: Option[OWLAPIOMF#ReifiedRelationship],
   reifiedRelationshipInverseSource: Option[OWLAPIOMF#ReifiedRelationship],
   reifiedRelationshipTarget: Option[OWLAPIOMF#ReifiedRelationship],
   reifiedRelationshipInverseTarget: Option[OWLAPIOMF#ReifiedRelationship],
   unreifiedRelationshipInverse: Option[OWLAPIOMF#UnreifiedRelationship])
  : OMFError.Throwables \/ OWLAPIOMF#SegmentPredicate
  = {
    val p = types.terms.SegmentPredicate(
      uuid, bodySegment,
      predicate,
      reifiedRelationshipSource,
      reifiedRelationshipInverseSource,
      reifiedRelationshipTarget,
      reifiedRelationshipInverseTarget,
      unreifiedRelationshipInverse)
    sig.segmentPredicates.add(p)
    p.right
  }

  def createEntityConceptSubClassAxiom
  (uuid: api.taggedTypes.ConceptSpecializationAxiomUUID,
   sub: OWLAPIOMF#ConceptKind,
   sup: OWLAPIOMF#ConceptKind)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ConceptSpecializationAxiom
  = sig.axioms
    .find {
      case axiom: ConceptSpecializationAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ OWLAPIOMF#ConceptSpecializationAxiom] {
      val axiom = ConceptSpecializationAxiom(uuid, sub, sup)
      sig.axioms += axiom
      axiom.right
    } {
    case ax: OWLAPIOMF#ConceptSpecializationAxiom =>
      ax.right

    case other =>
      Set(
        duplicateModelTermAxiomException(AxiomExceptionKind.EntityConceptSubClassAxiomException, other)
      ).left
  }

  def addEntityConceptSubClassAxiom
  (uuid: api.taggedTypes.ConceptSpecializationAxiomUUID,
   sub: OWLAPIOMF#ConceptKind,
   sup: OWLAPIOMF#ConceptKind)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ConceptSpecializationAxiom
  = (isTypeTermDefinedRecursively(sub),
    isTypeTermDefinedRecursively(sup)) match {
    case (true, true) =>
      for {
        axiom <- createEntityConceptSubClassAxiom(uuid, sub, sup)
        subC = owlDataFactory.getOWLClass(sub.iri)
        supC = owlDataFactory.getOWLClass(sup.iri)
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(ont, owlDataFactory
              .getOWLSubClassOfAxiom(
                subC,
                supC,
                createOMLProvenanceAnnotations(uuid)))
          ),
          s"addEntityConceptSubClassAxiom")
      } yield axiom

    case (false, true) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ConceptSubclassAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub))
      ).left

    case (true, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ConceptSubclassAxiomException,
          Map(AxiomScopeAccessKind.Sup -> sup))
      ).left

    case (false, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ConceptSubclassAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub, AxiomScopeAccessKind.Sup -> sup))
      ).left
  }

  def createReifiedRelationshipSpecializationAxiom
  (uuid: api.taggedTypes.ReifiedRelationshipSpecializationAxiomUUID,
   sub: OWLAPIOMF#ConceptualRelationship,
   sup: OWLAPIOMF#ConceptualRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipSpecializationAxiom
  = sig.axioms
    .find {
      case axiom: ReifiedRelationshipSpecializationAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ ReifiedRelationshipSpecializationAxiom] {
    val axiom = ReifiedRelationshipSpecializationAxiom(uuid, sub, sup)
    sig.axioms += axiom
    axiom.right
  } {
    case ax: OWLAPIOMF#ReifiedRelationshipSpecializationAxiom =>
      ax.right

    case other =>
      Set(
        duplicateModelTermAxiomException(AxiomExceptionKind.EntityReifiedRelationshipSubClassAxiomException, other)
      ).left
  }

  def addReifiedRelationshipSpecializationAxiom
  (uuid: api.taggedTypes.ReifiedRelationshipSpecializationAxiomUUID,
   sub: OWLAPIOMF#ConceptualRelationship,
   sup: OWLAPIOMF#ConceptualRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipSpecializationAxiom
  = (isTypeTermDefinedRecursively(sub), isTypeTermDefinedRecursively(sup)) match {
    case (true, true) =>
      (sub, sup) match {
        case (subRR: OWLAPIOMF#ReifiedRelationship, supRR: OWLAPIOMF#ReifiedRelationship) =>
          for {
            axiom <- createReifiedRelationshipSpecializationAxiom(uuid, subRR, supRR)
            _ <- applyOntologyChanges(
              ontManager,
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subRR.e,
                  supRR.e,
                  createOMLProvenanceAnnotations(uuid))),
                new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                  subRR.rSource,
                  supRR.rSource,
                  createOMLProvenanceAnnotations(uuid))),
                new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                  subRR.rTarget,
                  supRR.rTarget,
                  createOMLProvenanceAnnotations(uuid))),
                new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                  subRR.forwardProperty.e,
                  supRR.forwardProperty.e
                ))
              ) ++ subRR.inverseProperty.fold[Seq[OWLOntologyChange]](Seq.empty) { subi =>
                supRR.inverseProperty.fold[Seq[OWLOntologyChange]](Seq.empty) { supi =>
                  Seq(
                    new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                      subi.e, supi.e
                    ))
                  )
                }
              },
              s"addReifiedRelationshipSpecializationAxiom")
          } yield axiom

        case (subRR: OWLAPIOMF#ReifiedRelationship, supPR: OWLAPIOMF#CardinalityRestrictedReifiedRelationship) =>
          for {
            axiom <- createReifiedRelationshipSpecializationAxiom(uuid, subRR, supPR)
            rootRRs = supPR.rootCharacterizedEntityRelationships().selectByKindOf { case rr: ReifiedRelationship => rr }
            _ <- applyOntologyChanges(
              ontManager,
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subRR.e,
                  supPR.e,
                  createOMLProvenanceAnnotations(uuid)))
              ) ++ rootRRs.foldLeft[Seq[OWLOntologyChange]](Seq.empty) { case (acc, rootRR) =>
                acc ++
                  Seq(
                    new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                      subRR.rSource,
                      rootRR.rSource,
                      createOMLProvenanceAnnotations(uuid))),
                    new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                      subRR.rTarget,
                      rootRR.rTarget,
                      createOMLProvenanceAnnotations(uuid))),
                    new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                      subRR.forwardProperty.e,
                      rootRR.forwardProperty.e
                    ))
                  )
              } ++ subRR.inverseProperty.fold[Seq[OWLOntologyChange]](Seq.empty) { subi =>
                rootRRs.foldLeft[Seq[OWLOntologyChange]](Seq.empty) { case (acc, rootRR) =>
                  rootRR.inverseProperty.fold[Seq[OWLOntologyChange]](acc) { supi =>
                    acc ++
                      Seq(
                        new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                          subi.e, supi.e
                        ))
                      )
                  }
                }
              },
              s"addReifiedRelationshipSpecializationAxiom")
          } yield axiom

        case (subRR: OWLAPIOMF#ReifiedRelationship, supPR: OWLAPIOMF#ReifiedRelationshipRestriction) =>
          for {
            axiom <- createReifiedRelationshipSpecializationAxiom(uuid, subRR, supPR)
            rootRRs = supPR.rootCharacterizedEntityRelationships().selectByKindOf { case rr: ReifiedRelationship => rr }
            _ <- applyOntologyChanges(
              ontManager,
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subRR.e,
                  supPR.e,
                  createOMLProvenanceAnnotations(uuid)))
              ) ++ rootRRs.foldLeft[Seq[OWLOntologyChange]](Seq.empty) { case (acc, rootRR) =>
                acc ++
                Seq(
                  new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                    subRR.rSource,
                    rootRR.rSource,
                    createOMLProvenanceAnnotations(uuid))),
                  new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                    subRR.rTarget,
                    rootRR.rTarget,
                    createOMLProvenanceAnnotations(uuid))),
                  new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                    subRR.forwardProperty.e,
                    rootRR.forwardProperty.e
                  ))
                )
              } ++ subRR.inverseProperty.fold[Seq[OWLOntologyChange]](Seq.empty) { subi =>
                rootRRs.foldLeft[Seq[OWLOntologyChange]](Seq.empty) { case (acc, rootRR) =>
                  rootRR.inverseProperty.fold[Seq[OWLOntologyChange]](acc) { supi =>
                    acc ++
                    Seq(
                      new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                        subi.e, supi.e
                      ))
                    )
                  }
                }
              },
              s"addReifiedRelationshipSpecializationAxiom")
          } yield axiom

        case (subPR: OWLAPIOMF#CardinalityRestrictedReifiedRelationship, supRR: OWLAPIOMF#ReifiedRelationship) =>
          for {
            axiom <- createReifiedRelationshipSpecializationAxiom(uuid, subPR, supRR)
            _ <- applyOntologyChanges(
              ontManager,
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subPR.e,
                  supRR.e,
                  createOMLProvenanceAnnotations(uuid))),
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subPR.e,
                  owlDataFactory.getOWLObjectSomeValuesFrom(supRR.rSource, subPR.source.e),
                  createOMLProvenanceAnnotations(uuid))),
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subPR.e,
                  owlDataFactory.getOWLObjectSomeValuesFrom(supRR.rTarget, subPR.target.e),
                  createOMLProvenanceAnnotations(uuid)))
              ),
              s"addReifiedRelationshipSpecializationAxiom")
          } yield axiom

        case (subPR: OWLAPIOMF#CardinalityRestrictedReifiedRelationship, supPR: OWLAPIOMF#CardinalityRestrictedReifiedRelationship) =>
          for {
            axiom <- createReifiedRelationshipSpecializationAxiom(uuid, subPR, supPR)
            rootRRs = supPR.rootCharacterizedEntityRelationships().selectByKindOf { case rr: ReifiedRelationship => rr }
            _ <- applyOntologyChanges(
              ontManager,
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subPR.e,
                  supPR.e,
                  createOMLProvenanceAnnotations(uuid)))
              ) ++ rootRRs.foldLeft[Seq[OWLOntologyChange]](Seq.empty) { case (acc, rootRR) =>
                acc ++
                  Seq(
                    new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                      subPR.e,
                      owlDataFactory.getOWLObjectSomeValuesFrom(rootRR.rSource, subPR.source.e),
                      createOMLProvenanceAnnotations(uuid))),
                    new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                      subPR.e,
                      owlDataFactory.getOWLObjectSomeValuesFrom(rootRR.rTarget, subPR.target.e),
                      createOMLProvenanceAnnotations(uuid)))
                  )
              },
              s"addReifiedRelationshipSpecializationAxiom")
          } yield axiom

        case (subPR: OWLAPIOMF#CardinalityRestrictedReifiedRelationship, supPR: OWLAPIOMF#ReifiedRelationshipRestriction) =>
          for {
            axiom <- createReifiedRelationshipSpecializationAxiom(uuid, subPR, supPR)
            rootRRs = supPR.rootCharacterizedEntityRelationships().selectByKindOf { case rr: ReifiedRelationship => rr }
            _ <- applyOntologyChanges(
              ontManager,
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subPR.e,
                  supPR.e,
                  createOMLProvenanceAnnotations(uuid)))
              ) ++ rootRRs.foldLeft[Seq[OWLOntologyChange]](Seq.empty) { case (acc, rootRR) =>
                acc ++
                  Seq(
                    new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                      subPR.e,
                      owlDataFactory.getOWLObjectSomeValuesFrom(rootRR.rSource, subPR.source.e),
                      createOMLProvenanceAnnotations(uuid))),
                    new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                      subPR.e,
                      owlDataFactory.getOWLObjectSomeValuesFrom(rootRR.rTarget, subPR.target.e),
                      createOMLProvenanceAnnotations(uuid)))
                  )
              },
              s"addReifiedRelationshipSpecializationAxiom")
          } yield axiom

        case (subPR: OWLAPIOMF#ReifiedRelationshipRestriction, supRR: OWLAPIOMF#ReifiedRelationship) =>
          for {
            axiom <- createReifiedRelationshipSpecializationAxiom(uuid, subPR, supRR)
            _ <- applyOntologyChanges(
              ontManager,
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subPR.e,
                  supRR.e,
                  createOMLProvenanceAnnotations(uuid))),
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subPR.e,
                  owlDataFactory.getOWLObjectSomeValuesFrom(supRR.rSource, subPR.source.e),
                  createOMLProvenanceAnnotations(uuid))),
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subPR.e,
                  owlDataFactory.getOWLObjectSomeValuesFrom(supRR.rTarget, subPR.target.e),
                  createOMLProvenanceAnnotations(uuid)))
              ),
              s"addReifiedRelationshipSpecializationAxiom")
          } yield axiom

        case (subPR: OWLAPIOMF#ReifiedRelationshipRestriction, supPR: OWLAPIOMF#CardinalityRestrictedReifiedRelationship) =>
          for {
            axiom <- createReifiedRelationshipSpecializationAxiom(uuid, subPR, supPR)
            rootRRs = supPR.rootCharacterizedEntityRelationships().selectByKindOf { case rr: ReifiedRelationship => rr }
            _ <- applyOntologyChanges(
              ontManager,
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subPR.e,
                  supPR.e,
                  createOMLProvenanceAnnotations(uuid)))
              ) ++ rootRRs.foldLeft[Seq[OWLOntologyChange]](Seq.empty) { case (acc, rootRR) =>
                acc ++
                  Seq(
                    new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                      subPR.e,
                      owlDataFactory.getOWLObjectSomeValuesFrom(rootRR.rSource, subPR.source.e),
                      createOMLProvenanceAnnotations(uuid))),
                    new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                      subPR.e,
                      owlDataFactory.getOWLObjectSomeValuesFrom(rootRR.rTarget, subPR.target.e),
                      createOMLProvenanceAnnotations(uuid)))
                  )
              },
              s"addReifiedRelationshipSpecializationAxiom")
          } yield axiom

        case (subPR: OWLAPIOMF#ReifiedRelationshipRestriction, supPR: OWLAPIOMF#ReifiedRelationshipRestriction) =>
          for {
            axiom <- createReifiedRelationshipSpecializationAxiom(uuid, subPR, supPR)
            rootRRs = supPR.rootCharacterizedEntityRelationships().selectByKindOf { case rr: ReifiedRelationship => rr }
            _ <- applyOntologyChanges(
              ontManager,
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  subPR.e,
                  supPR.e,
                  createOMLProvenanceAnnotations(uuid)))
              ) ++ rootRRs.foldLeft[Seq[OWLOntologyChange]](Seq.empty) { case (acc, rootRR) =>
                acc ++
                  Seq(
                    new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                      subPR.e,
                      owlDataFactory.getOWLObjectSomeValuesFrom(rootRR.rSource, subPR.source.e),
                      createOMLProvenanceAnnotations(uuid))),
                    new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                      subPR.e,
                      owlDataFactory.getOWLObjectSomeValuesFrom(rootRR.rTarget, subPR.target.e),
                      createOMLProvenanceAnnotations(uuid)))
                  )
              },
              s"addReifiedRelationshipSpecializationAxiom")
          } yield axiom

      }

    case (false, true) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ReifiedRelationshipSpecializationAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub))
      ).left

    case (true, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ReifiedRelationshipSpecializationAxiomException,
          Map(AxiomScopeAccessKind.Sup -> sup))
      ).left

    case (false, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ReifiedRelationshipSpecializationAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub, AxiomScopeAccessKind.Sup -> sup))
      ).left
  }

  def addEntityDefinitionUniversalRestrictionAxiom
  (sub: OWLAPIOMF#Entity,
   rel: OWLAPIOMF#RestrictableRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityUniversalRestrictionAxiom
  = for {
    uuid <- entityUniversalRestrictionAxiomUUID(this, sub, rel, range)
    ax <- addEntityDefinitionUniversalRestrictionAxiom(uuid, sub, rel, range)
  } yield ax

  def createEntityDefinitionUniversalRestrictionAxiom
  (uuid: api.taggedTypes.EntityUniversalRestrictionAxiomUUID,
   restrictedDomain: OWLAPIOMF#Entity,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: OWLAPIOMF#Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityUniversalRestrictionAxiom
  = sig.axioms
    .find {
      case axiom: EntityUniversalRestrictionAxiom =>
        axiom.restrictedDomain == restrictedDomain &&
        axiom.restrictedRelationship == restrictedRelationship &&
        axiom.restrictedDomain == restrictedDomain
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ EntityUniversalRestrictionAxiom] {
    val axiom = EntityUniversalRestrictionAxiom(uuid, restrictedDomain, restrictedRelationship, restrictedDomain)
    sig.axioms += axiom
    axiom.right
  } {
    case ax: OWLAPIOMF#EntityUniversalRestrictionAxiom =>
      ax.right

    case other =>
      Set(
        duplicateModelTermAxiomException(AxiomExceptionKind.EntityUniversalEntityRestrictionAxiomException, other)
      ).left
  }

  def addEntityDefinitionUniversalRestrictionAxiom
  (uuid: api.taggedTypes.EntityUniversalRestrictionAxiomUUID,
   sub: OWLAPIOMF#Entity,
   rel: OWLAPIOMF#RestrictableRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityUniversalRestrictionAxiom
  = (isTypeTermDefinedRecursively(sub),
    isRestrictableRelationshipDefinedRecursively(rel),
    isTypeTermDefinedRecursively(range)) match {

    case (true, true, true) =>

      val subC = owlDataFactory.getOWLClass(sub.iri)
      val rangeC = owlDataFactory.getOWLClass(range.iri)
      val op = rel match {
        case fwd: OWLAPIOMF#ForwardProperty =>
          fwd.e
        case inv: OWLAPIOMF#InverseProperty =>
          inv.e
        case ur: OWLAPIOMF#UnreifiedRelationship =>
          ur.e
      }
      for {
        axiom <- createEntityDefinitionUniversalRestrictionAxiom(uuid, sub, rel, range)
        _ <- applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont,
            owlDataFactory.getOWLSubClassOfAxiom(
              subC,
              owlDataFactory.getOWLObjectAllValuesFrom(op, rangeC),
              createOMLProvenanceAnnotations(uuid))),
          ifError = {
            "addEntityDefinitionUniversalRestrictionAxiom Error"
          })
      } yield {
        sig.axioms += axiom
        axiom
      }

    case (false, _, _) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ConceptRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub))
      ).left

    case (_, false, _) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ConceptRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Rel -> rel))
      ).left

    case (_, _, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ConceptRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Range -> range))
      ).left
  }

  def addEntityDefinitionExistentialRestrictionAxiom
  (sub: OWLAPIOMF#Entity,
   rel: OWLAPIOMF#RestrictableRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityExistentialRestrictionAxiom
  = for {
    uuid <- entityExistentialRestrictionAxiomUUID(this, sub, rel, range)
    ax <- addEntityDefinitionExistentialRestrictionAxiom(uuid, sub, rel, range)
  } yield ax

  def createEntityDefinitionExistentialRestrictionAxiom
  (uuid: api.taggedTypes.EntityExistentialRestrictionAxiomUUID,
   restrictedDomain: OWLAPIOMF#Entity,
   restrictedRelationship: OWLAPIOMF#RestrictableRelationship,
   restrictedRange: OWLAPIOMF#Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityExistentialRestrictionAxiom
  = sig.axioms
    .find {
      case axiom: EntityExistentialRestrictionAxiom =>
        axiom.restrictedDomain == restrictedDomain &&
          axiom.restrictedRelationship == restrictedRelationship &&
          axiom.restrictedDomain == restrictedDomain
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ EntityExistentialRestrictionAxiom] {
    val axiom = EntityExistentialRestrictionAxiom(uuid, restrictedDomain, restrictedRelationship, restrictedDomain)
    sig.axioms += axiom
    axiom.right
  } {
    case ax: OWLAPIOMF#EntityExistentialRestrictionAxiom =>
      ax.right

    case other =>
      Set(
        duplicateModelTermAxiomException(AxiomExceptionKind.EntityExistentialEntityRestrictionAxiomException, other)
      ).left
  }
  
  def addEntityDefinitionExistentialRestrictionAxiom
  (uuid: api.taggedTypes.EntityExistentialRestrictionAxiomUUID,
   sub: OWLAPIOMF#Entity,
   rel: OWLAPIOMF#RestrictableRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityExistentialRestrictionAxiom
  = (isTypeTermDefinedRecursively(sub),
     isRestrictableRelationshipDefinedRecursively(rel),
     isTypeTermDefinedRecursively(range)) match {
    case (true, true, true) =>
      val subC = owlDataFactory.getOWLClass(sub.iri)
      val rangeC = owlDataFactory.getOWLClass(range.iri)
      val op = rel match {
        case fwd: OWLAPIOMF#ForwardProperty =>
          fwd.e
        case inv: OWLAPIOMF#InverseProperty =>
          inv.e
        case ur: OWLAPIOMF#UnreifiedRelationship =>
          ur.e
      }
      for {
        axiom <- createEntityDefinitionExistentialRestrictionAxiom(uuid, sub, rel, range)
        _ <- store.applyModelTermAxiomChanges(
          axiom,
          "addEntityDefinitionExistentialRestrictionAxiom",
          Seq(
            new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
              subC,
              owlDataFactory.getOWLObjectSomeValuesFrom(op, rangeC),
              createOMLProvenanceAnnotations(uuid)))
          ))
      } yield {
        sig.axioms += axiom
        axiom
      }

    case (false, _, _) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ConceptRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub))
      ).left

    case (_, false, _) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ConceptRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Rel -> rel))
      ).left

    case (_, _, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ConceptRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Range -> range))
      ).left
  }

  def createEntityScalarDataPropertyExistentialRestrictionAxiom
  (uuid: api.taggedTypes.EntityScalarDataPropertyExistentialRestrictionAxiomUUID,
   restrictedEntity: OWLAPIOMF#Entity,
   scalarProperty: OWLAPIOMF#EntityScalarDataProperty,
   scalarRestriction: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataPropertyExistentialRestrictionAxiom
  = sig.axioms
    .find {
      case axiom: EntityScalarDataPropertyUniversalRestrictionAxiom =>
        axiom.restrictedEntity == restrictedEntity &&
          axiom.scalarProperty == scalarProperty &&
          axiom.scalarRestriction == scalarRestriction
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ EntityScalarDataPropertyExistentialRestrictionAxiom] {
    val axiom = EntityScalarDataPropertyExistentialRestrictionAxiom(uuid, restrictedEntity, scalarProperty, scalarRestriction)
    sig.axioms += axiom
    axiom.right
  } {
    case ax: OWLAPIOMF#EntityScalarDataPropertyExistentialRestrictionAxiom =>
      ax.right

    case other =>
      Set(
        duplicateModelTermAxiomException(AxiomExceptionKind.EntityScalarDataPropertyExistentialRestrictionAxiomException, other)
      ).left
  }

  def addEntityScalarDataPropertyExistentialRestrictionAxiom
  (uuid: api.taggedTypes.EntityScalarDataPropertyExistentialRestrictionAxiomUUID,
   restrictedEntity: OWLAPIOMF#Entity,
   scalarProperty: OWLAPIOMF#EntityScalarDataProperty,
   scalarRestriction: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataPropertyExistentialRestrictionAxiom
  = (isTypeTermDefinedRecursively(restrictedEntity),
    isTypeTermDefinedRecursively(scalarProperty),
    isTypeTermDefinedRecursively(scalarRestriction)) match {
    case (true, true, true) =>
      val subC = owlDataFactory.getOWLClass(restrictedEntity.iri)
      for {
        axiom <- createEntityScalarDataPropertyExistentialRestrictionAxiom(uuid, restrictedEntity, scalarProperty, scalarRestriction)
        _ <- applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
            subC,
            owlDataFactory.getOWLDataSomeValuesFrom(scalarProperty.e, scalarRestriction.e),
            createOMLProvenanceAnnotations(uuid))),
          ifError = {
            "addEntityScalarDataPropertyExistentialRestrictionAxiom Error"
          })
      } yield axiom

    case (false, _, _) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityScalarDataPropertyExistentialRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Domain -> restrictedEntity))
      ).left

    case (_, false, _) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityScalarDataPropertyExistentialRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Rel -> scalarProperty))
      ).left

    case (_, _, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityScalarDataPropertyExistentialRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Range -> scalarRestriction))
      ).left

  }

  def createEntityScalarDataPropertyUniversalRestrictionAxiom
  (uuid: api.taggedTypes.EntityScalarDataPropertyUniversalRestrictionAxiomUUID,
   restrictedEntity: OWLAPIOMF#Entity,
   scalarProperty: OWLAPIOMF#EntityScalarDataProperty,
   scalarRestriction: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataPropertyUniversalRestrictionAxiom
  = sig.axioms
    .find {
      case axiom: EntityScalarDataPropertyUniversalRestrictionAxiom =>
        axiom.restrictedEntity == restrictedEntity &&
          axiom.scalarProperty == scalarProperty &&
          axiom.scalarRestriction == scalarRestriction
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ EntityScalarDataPropertyUniversalRestrictionAxiom] {
    val axiom = EntityScalarDataPropertyUniversalRestrictionAxiom(uuid, restrictedEntity, scalarProperty, scalarRestriction)
    sig.axioms += axiom
    axiom.right
  } {
    case ax: OWLAPIOMF#EntityScalarDataPropertyUniversalRestrictionAxiom =>
      ax.right

    case other =>
      Set(
        duplicateModelTermAxiomException(AxiomExceptionKind.EntityScalarDataPropertyUniversalRestrictionAxiomException, other)
      ).left
  }

  def addEntityScalarDataPropertyUniversalRestrictionAxiom
  (uuid: api.taggedTypes.EntityScalarDataPropertyUniversalRestrictionAxiomUUID,
   restrictedEntity: OWLAPIOMF#Entity,
   scalarProperty: OWLAPIOMF#EntityScalarDataProperty,
   scalarRestriction: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataPropertyUniversalRestrictionAxiom
  = (isTypeTermDefinedRecursively(restrictedEntity),
    isTypeTermDefinedRecursively(scalarProperty),
    isTypeTermDefinedRecursively(scalarRestriction)) match {
    case (true, true, true) =>
      val subC = owlDataFactory.getOWLClass(restrictedEntity.iri)
      for {
        axiom <- createEntityScalarDataPropertyUniversalRestrictionAxiom(uuid, restrictedEntity, scalarProperty, scalarRestriction)
        _ <- applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont,
            owlDataFactory.getOWLSubClassOfAxiom(
              subC,
              owlDataFactory.getOWLDataAllValuesFrom(scalarProperty.e, scalarRestriction.e),
              createOMLProvenanceAnnotations(uuid))),
          ifError = {
            "addEntityScalarDataPropertyUniversalRestrictionAxiom Error"
          })
      } yield axiom

    case (false, _, _) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityScalarDataPropertyUniversalRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Domain -> restrictedEntity))
      ).left

    case (_, false, _) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityScalarDataPropertyUniversalRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Rel -> scalarProperty))
      ).left

    case (_, _, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityScalarDataPropertyUniversalRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Range -> scalarRestriction))
      ).left

  }

  def createEntityScalarDataPropertyParticularRestrictionAxiom
  (uuid: api.taggedTypes.EntityScalarDataPropertyParticularRestrictionAxiomUUID,
   restrictedEntity: OWLAPIOMF#Entity,
   scalarProperty: OWLAPIOMF#EntityScalarDataProperty,
   literalValue: tables.LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataPropertyParticularRestrictionAxiom
  = sig.axioms
    .find {
      case axiom: EntityScalarDataPropertyParticularRestrictionAxiom =>
        axiom.restrictedEntity == restrictedEntity &&
          axiom.scalarProperty == scalarProperty
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ EntityScalarDataPropertyParticularRestrictionAxiom] {
    val axiom = EntityScalarDataPropertyParticularRestrictionAxiom(uuid, restrictedEntity, scalarProperty, literalValue, valueType)
    sig.axioms += axiom
    axiom.right
  } {
    case ax: OWLAPIOMF#EntityScalarDataPropertyParticularRestrictionAxiom =>
      ax.right

    case other =>
      Set(
        duplicateModelTermAxiomException(AxiomExceptionKind.EntityScalarDataPropertyParticularRestrictionAxiomException, other)
      ).left
  }

  def addEntityScalarDataPropertyParticularRestrictionAxiom
  (uuid: api.taggedTypes.EntityScalarDataPropertyParticularRestrictionAxiomUUID,
   restrictedEntity: OWLAPIOMF#Entity,
   scalarProperty: OWLAPIOMF#EntityScalarDataProperty,
   literalValue: tables.LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataPropertyParticularRestrictionAxiom
  = (isTypeTermDefinedRecursively(restrictedEntity),
    isTypeTermDefinedRecursively(scalarProperty)) match {
    case (true, true) =>
      // TODO: Key axiom should have been defined on the scalar data property, not the restriction!
      val key = if (sig.kind == TerminologyKind.isClosedWorld)
        Option(new AddAxiom(ont, owlDataFactory.getOWLHasKeyAxiom(
          restrictedEntity.e,
          Collections.singleton(scalarProperty.e),
          createOMLProvenanceAnnotations(uuid)))
        )
      else
        Option.empty
      for {
        ax <- createEntityScalarDataPropertyParticularRestrictionAxiom(uuid, restrictedEntity, scalarProperty, literalValue, valueType)
        _ <- applyOntologyChangesOrNoOp(
          ontManager,
          Seq(
            new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
              restrictedEntity.e,
              owlDataFactory.getOWLDataHasValue(scalarProperty.e,
                LiteralConversions.toOWLLiteral(literalValue, owlDataFactory,
                  valueType.map(_.e).orElse(Option.apply(scalarProperty.range.e)))),
              createOMLProvenanceAnnotations(uuid)))
          ) ++ key,
          ifError = {
            "addEntityScalarDataPropertyParticularRestrictionAxiom Error"
          })
      } yield ax

    case (false, _) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityScalarDataPropertyParticularRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Domain -> restrictedEntity))
      ).left

    case (_, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityScalarDataPropertyParticularRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Rel -> scalarProperty))
      ).left

  }

  def addEntityStructuredDataPropertyParticularRestrictionAxiom
  (uuid: api.taggedTypes.EntityStructuredDataPropertyParticularRestrictionAxiomUUID,
   restrictedEntity: OWLAPIOMF#Entity,
   structuredProperty: OWLAPIOMF#EntityStructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataPropertyParticularRestrictionAxiom
  = (isTypeTermDefinedRecursively(restrictedEntity),
    isTypeTermDefinedRecursively(structuredProperty)) match {
    case (true, true) =>
      for {
        e_iri <- withFragment(iri, localName(uuid.toString))
        e_ni = owlDataFactory.getOWLNamedIndividual(e_iri)
        axiom = EntityStructuredDataPropertyParticularRestrictionAxiom(uuid, restrictedEntity, structuredProperty, e_ni)
        // TODO: Key axiom should have been defined on the scalar data property, not the restriction!
        key = if (sig.kind == TerminologyKind.isClosedWorld)
          Option(new AddAxiom(ont, owlDataFactory.getOWLHasKeyAxiom(
            restrictedEntity.e,
            Collections.singleton(structuredProperty.e),
            createOMLProvenanceAnnotations(uuid)))
          )
        else
          Option.empty
        _ <- applyOntologyChangesOrNoOp(
          ontManager,
          Seq(
            new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
              e_ni,
              createOMLProvenanceAnnotations(uuid))),
            new AddAxiom(ont, owlDataFactory.getOWLClassAssertionAxiom(
              structuredProperty.range.e,
              e_ni,
              createOMLProvenanceAnnotations(uuid))),
            new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
              restrictedEntity.e,
              owlDataFactory.getOWLObjectHasValue(structuredProperty.e, e_ni),
              createOMLProvenanceAnnotations(uuid)))
          ) ++ key,
          ifError = {
            "addEntityStructuredDataPropertyParticularRestrictionAxiom Error"
          })
      } yield {
        sig.axioms += axiom
        axiom
      }

    case (false, _) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityStructuredDataPropertyParticularRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Domain -> restrictedEntity))
      ).left

    case (_, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityStructuredDataPropertyParticularRestrictionAxiomException,
          Map(AxiomScopeAccessKind.Rel -> structuredProperty))
      ).left

  }

  def addRestrictionStructuredDataPropertyTuple
  (uuid: api.taggedTypes.RestrictionStructuredDataPropertyTupleUUID,
   structuredDataPropertyContext: OWLAPIOMF#RestrictionStructuredDataPropertyContext,
   structuredProperty: OWLAPIOMF#DataRelationshipToStructure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#RestrictionStructuredDataPropertyTuple
  = if (isTypeTermDefinedRecursively(structuredProperty)) {
    for {
      e_iri <- withFragment(iri, localName(uuid.toString))
      e_ni = owlDataFactory.getOWLNamedIndividual(e_iri)
      tuple = types.RestrictionStructuredDataPropertyTuple(uuid, structuredDataPropertyContext, structuredProperty, e_ni)
      _ <- applyOntologyChangesOrNoOp(
        ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            e_ni,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLClassAssertionAxiom(
            structuredProperty.range.e, e_ni,
            createOMLProvenanceAnnotations(uuid))),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyAssertionAxiom(
            structuredProperty.e, structuredDataPropertyContext.e, e_ni,
            createOMLProvenanceAnnotations(uuid)))
        ),
        ifError = {
          "addRestrictionStructuredDataPropertyTuple Error"
        })
    } yield {
      sig.restrictionStructuredDataPropertyTuples += tuple
      tuple
    }
  } else
      Set(
        axiomScopeException(
          AxiomExceptionKind.RestrictionStructuredDataPropertyTupleException,
          Map(AxiomScopeAccessKind.Rel -> structuredProperty))
      ).left

  def addRestrictionScalarDataPropertyValue
  (uuid: api.taggedTypes.RestrictionScalarDataPropertyValueUUID,
   structuredDataPropertyContext: OWLAPIOMF#RestrictionStructuredDataPropertyContext,
   scalarProperty: OWLAPIOMF#DataRelationshipToScalar,
   literalValue: tables.LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#RestrictionScalarDataPropertyValue
  = if (isTypeTermDefinedRecursively(scalarProperty)) {
    val value = types.RestrictionScalarDataPropertyValue(uuid, structuredDataPropertyContext, scalarProperty, literalValue, valueType)
    for {
      _ <- applyOntologyChangesOrNoOp(
        ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDataPropertyAssertionAxiom(
            scalarProperty.e,
            structuredDataPropertyContext.e,
            LiteralConversions.toOWLLiteral(literalValue, owlDataFactory,
              valueType.map(_.e).orElse(Option.apply(scalarProperty.range.e))),
            createOMLProvenanceAnnotations(uuid)))
        ),
        ifError = {
          "addRestrictionScalarDataPropertyValue Error"
        })
    } yield {
      sig.restrictionScalarDataPropertyValues += value
      value
    }
  } else
    Set(
      axiomScopeException(
        AxiomExceptionKind.RestrictionScalarDataPropertyValueException,
        Map(AxiomScopeAccessKind.Rel -> scalarProperty))
    ).left

  def createEntityDefinitionAspectSubClassAxiom
  (uuid: api.taggedTypes.AspectSpecializationAxiomUUID,
   sub: OWLAPIOMF#Entity,
   sup: OWLAPIOMF#AspectKind)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#AspectSpecializationAxiom
  = sig.axioms
    .find {
      case axiom: AspectSpecializationAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ OWLAPIOMF#AspectSpecializationAxiom] {
    val axiom = AspectSpecializationAxiom(uuid, sub, sup)
    sig.axioms += axiom
    axiom.right
  } { other =>
    Set(
      duplicateModelTermAxiomException(AxiomExceptionKind.EntityDefinitionAspectSubClassAxiomException, other)
    ).left
  }

  def addEntityDefinitionAspectSubClassAxiom
  (uuid: api.taggedTypes.AspectSpecializationAxiomUUID,
   sub: OWLAPIOMF#Entity,
   sup: OWLAPIOMF#AspectKind)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#AspectSpecializationAxiom
  = (isTypeTermDefinedRecursively(sub),
    isTypeTermDefinedRecursively(sup)) match {
    case (true, true) =>
      for {
        axiom <- createEntityDefinitionAspectSubClassAxiom(uuid, sub, sup)
        _ <- applyOntologyChanges(
          ontManager,
          Seq(
            new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
              sub.e,
              sup.e,
              createOMLProvenanceAnnotations(uuid)))
          ),
          "addEntityDefinitionAspectSubClassAxiom")
      } yield axiom

    case (false, true) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityDefinitionAspectSubClassAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub))
      ).left

    case (true, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityDefinitionAspectSubClassAxiomException,
          Map(AxiomScopeAccessKind.Sup -> sub))
      ).left

    case (false, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.EntityDefinitionAspectSubClassAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub, AxiomScopeAccessKind.Sup -> sub))
      ).left
  }

  def createOMFEntityConceptDesignationTerminologyGraphAxiom
  (uuid: api.taggedTypes.ConceptDesignationTerminologyAxiomUUID,
   graph: OWLAPIOMF#MutableTerminologyBox,
   entityConceptDesignation: OWLAPIOMF#Concept,
   designationTerminologyGraph: OWLAPIOMF#TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ConceptDesignationTerminologyAxiom
  = sig.conceptDesignation
    .find {
      case axiom: ConceptDesignationTerminologyAxiom =>
        axiom.designatedConcept == entityConceptDesignation &&
          axiom.targetModuleIRI == designationTerminologyGraph.iri
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ ConceptDesignationTerminologyAxiom] {
    val axiom = ConceptDesignationTerminologyAxiom(
      uuid, graph.uuid, entityConceptDesignation, designationTerminologyGraph.iri)
    sig.conceptDesignation += axiom
    axiom.right
  } { other =>
    Set(
      duplicateTerminologyGraphAxiomException(AxiomExceptionKind.EntityConceptDesignationTerminologyGraphAxiomException, other)
    ).left
  }

  def addEntityConceptDesignationTerminologyGraphAxiom
  (uuid: api.taggedTypes.ConceptDesignationTerminologyAxiomUUID,
   graph: OWLAPIOMF#MutableTerminologyBox,
   entityConceptDesignation: OWLAPIOMF#Concept,
   designationTerminologyGraph: OWLAPIOMF#TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ConceptDesignationTerminologyAxiom
  = for {
    axiom <- createOMFEntityConceptDesignationTerminologyGraphAxiom(
      uuid, graph, entityConceptDesignation, designationTerminologyGraph)
  } yield {
    sig.conceptDesignation += axiom
    axiom
  }

  def createSubDataPropertyOfAxiom
  (uuid: api.taggedTypes.SubDataPropertyOfAxiomUUID,
   sub: OWLAPIOMF#EntityScalarDataProperty,
   sup: OWLAPIOMF#EntityScalarDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#SubDataPropertyOfAxiom
  = sig.axioms
    .find {
      case axiom: OWLAPIOMF#SubDataPropertyOfAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ OWLAPIOMF#SubDataPropertyOfAxiom] {
    val axiom = SubDataPropertyOfAxiom(uuid, sub, sup)
    sig.axioms += axiom
    axiom.right
  } { other =>
    Set(
      duplicateModelTermAxiomException(AxiomExceptionKind.SubDataPropertyOfAxiomException, other)
    ).left
  }

  def addSubDataPropertyOfAxiom
  (uuid: api.taggedTypes.SubDataPropertyOfAxiomUUID,
   sub: OWLAPIOMF#EntityScalarDataProperty,
   sup: OWLAPIOMF#EntityScalarDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#SubDataPropertyOfAxiom
  = (isTypeTermDefinedRecursively(sub), isTypeTermDefinedRecursively(sup)) match {
    case (true, true) =>
      for {
        axiom <- createSubDataPropertyOfAxiom(uuid, sub, sup)
        _ <- applyOntologyChanges(
          ontManager,
          Seq(
            new AddAxiom(ont, owlDataFactory.getOWLSubDataPropertyOfAxiom(
              sub.e,
              sup.e,
              createOMLProvenanceAnnotations(uuid)))
          ),
          s"addSubDataPropertyOfAxiom")
      } yield axiom

    case (false, true) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.SubDataPropertyOfAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub))
      ).left

    case (true, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.SubDataPropertyOfAxiomException,
          Map(AxiomScopeAccessKind.Sup -> sup))
      ).left

    case (false, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.SubDataPropertyOfAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub, AxiomScopeAccessKind.Sup -> sup))
      ).left
  }

  def createSubObjectPropertyOfAxiom
  (uuid: api.taggedTypes.SubObjectPropertyOfAxiomUUID,
   sub: OWLAPIOMF#UnreifiedRelationship,
   sup: OWLAPIOMF#UnreifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#SubObjectPropertyOfAxiom
  = sig.axioms
    .find {
      case axiom: OWLAPIOMF#SubObjectPropertyOfAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ OWLAPIOMF#SubObjectPropertyOfAxiom] {
    val axiom = SubObjectPropertyOfAxiom(uuid, sub, sup)
    sig.axioms += axiom
    axiom.right
  } { other =>
    Set(
      duplicateModelTermAxiomException(AxiomExceptionKind.SubObjectPropertyOfAxiomException, other)
    ).left
  }

  def addSubObjectPropertyOfAxiom
  (uuid: api.taggedTypes.SubObjectPropertyOfAxiomUUID,
   sub: OWLAPIOMF#UnreifiedRelationship,
   sup: OWLAPIOMF#UnreifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#SubObjectPropertyOfAxiom
  = (isTypeTermDefinedRecursively(sub), isTypeTermDefinedRecursively(sup)) match {
    case (true, true) =>
      for {
        axiom <- createSubObjectPropertyOfAxiom(uuid, sub, sup)
        _ <- applyOntologyChanges(
          ontManager,
          Seq(
            new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
              sub.e,
              sup.e,
              createOMLProvenanceAnnotations(uuid)))
          ),
          s"addSubObjectPropertyOfAxiom")
      } yield axiom

    case (false, true) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.SubObjectPropertyOfAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub))
      ).left

    case (true, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.SubObjectPropertyOfAxiomException,
          Map(AxiomScopeAccessKind.Sup -> sup))
      ).left

    case (false, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.SubObjectPropertyOfAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub, AxiomScopeAccessKind.Sup -> sup))
      ).left
  }


}
