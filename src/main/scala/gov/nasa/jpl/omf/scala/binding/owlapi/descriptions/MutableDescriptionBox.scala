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

package gov.nasa.jpl.omf.scala.binding.owlapi.descriptions

import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.resolver.Extent2Tables.toUUIDString
import gov.nasa.jpl.imce.oml.resolver.Filterable.filterable
import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue, LiteralValue}
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{MutableModule, Resource}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.TerminologyBox
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.{DescriptionBoxSignature, DescriptionKind, MutableDescriptionBoxSignature, OMFError, uuidG}
import org.semanticweb.owlapi.model._

import scala.collection.immutable._
import scala.collection.mutable.{HashMap, HashSet}
import scala.compat.java8.StreamConverters._
import scala.{Any, Boolean, Int, Option, None, Some, StringContext}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

object MutableDescriptionBox {

  def initialize
  (uuid: api.taggedTypes.DescriptionBoxUUID,
   name: tables.taggedTypes.LocalName,
   iri: IRI,
   kind: DescriptionKind,
   ont: OWLOntology,
   backbone: OMFBackbone)
  (implicit store: OWLAPIOMFGraphStore, ops: OWLAPIOMFOps)
  : OMFError.Throwables \/ MutableDescriptionBox
  = MutableDescriptionBox(
    sig = DescriptionBoxSignature[OWLAPIOMF, HashSet](
      uuid, name, iri, kind,
      descriptionBoxRefinements = HashSet.empty[DescriptionBoxRefinement],
      closedWorldDefinitions = HashSet.empty[DescriptionBoxExtendsClosedWorldDefinitions],
      conceptInstances = HashSet.empty[ConceptInstance],
      reifiedRelationshipInstances = HashSet.empty[ReifiedRelationshipInstance],
      reifiedRelationshipInstanceDomains = HashSet.empty[ReifiedRelationshipInstanceDomain],
      reifiedRelationshipInstanceRanges = HashSet.empty[ReifiedRelationshipInstanceRange],
      unreifiedRelationshipInstanceTuples = HashSet.empty[UnreifiedRelationshipInstanceTuple],
      instanceRelationshipEnumerationRestrictions = HashSet.empty[InstanceRelationshipEnumerationRestriction],
      instanceRelationshipValueRestrictions = HashSet.empty[InstanceRelationshipValueRestriction],
      instanceRelationshipExistentialRangeRestrictions = HashSet.empty[InstanceRelationshipExistentialRangeRestriction],
      instanceRelationshipUniversalRangeRestrictions = HashSet.empty[InstanceRelationshipUniversalRangeRestriction],
      singletonScalarDataPropertyValues = HashSet.empty[SingletonInstanceScalarDataPropertyValue],
      singletonStructuredDataPropertyValues = HashSet.empty[SingletonInstanceStructuredDataPropertyValue],
      scalarDataPropertyValues = HashSet.empty[ScalarDataPropertyValue],
      structuredDataPropertyTuples = HashSet.empty[StructuredDataPropertyTuple],
      annotationProperties =
        HashSet.empty[AnnotationProperty],
      annotationPropertyValues =
        HashSet.empty[AnnotationPropertyValue]),
    ont = ont,
    backbone = backbone)(ops).right[OMFError.Throwables]
}

case class MutableDescriptionBox
(override val sig: MutableDescriptionBoxSignature[OWLAPIOMF],
 override val ont: OWLOntology,
 override val backbone: OMFBackbone)
(override implicit val ops: OWLAPIOMFOps)
  extends DescriptionBox
    with MutableModule {

  override type MS = MutableDescriptionBoxSignature[OWLAPIOMF]

  protected val iri2namedIndividual = HashMap[IRI, ConceptualEntitySingletonInstance]()

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: MutableDescriptionBox => true
    case _ => false
  }

  override val hashCode: Int = (sig, ont).##

  override def equals(other: Any): Boolean = other match {
    case that: MutableDescriptionBox =>
      (that canEqual this) &&
        (this.sig.uuid == that.sig.uuid) &&
        (this.sig == that.sig) &&
        (this.ont == that.ont)
    case _ =>
      false
  }

  def addAnnotationProperty
  (ap: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ AnnotationProperty
  = for {
    _ <- (sig.annotationProperties += ap).right[OMFError.Throwables]
    ont_ap = owlDataFactory.getOWLAnnotationProperty(ap.iri)
    _ <- applyOntologyChangeOrNoOp(ontManager,
      new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
        ont_ap,
        createOMLProvenanceAnnotations(api.taggedTypes.fromUUIDString(ap.uuid)))),
      "addAnnotationProperty error")
  } yield ap

  def addAnnotation
  (subject: OWLAPIOMF#LogicalElement,
   property: AnnotationProperty,
   value: tables.taggedTypes.StringDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ AnnotationPropertyValue
  = for {
    a <- new AnnotationPropertyValue(
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
      case r: Resource =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(ont_ap, r.iri, ont_lit)),
          "addAnnotation error")
      case _ =>
        Set[java.lang.Throwable](
          OMFError.omfError(s"addAnnotation is not supported for a non-resource subject: $subject")
        ).left
    }
  } yield a

  def removeAnnotations
  (subject: OWLAPIOMF#LogicalElement,
   property: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Set[AnnotationPropertyValue]
  = for {
    sUUID <- subject.uuid.toString.right[OMFError.Throwables]
    ae <- sig.annotationPropertyValues.find { a => a.subjectUUID == sUUID && a.propertyUUID == property.uuid } match {
      case Some(a0) =>
        sig.annotationPropertyValues -= a0
        Set(a0).right
      case None =>
        Set.empty[AnnotationPropertyValue].right
    }
    ont_ap = owlDataFactory.getOWLAnnotationProperty(property.iri)
    _ <- subject match {
      case r: Resource =>
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

  def addDescriptionBoxExtendsClosedWorldDefinitions
  (uuid: api.taggedTypes.DescriptionBoxExtendsClosedWorldDefinitionsUUID,
   closedWorldDefinitions: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.DescriptionBoxExtendsClosedWorldDefinitions
  = for {
    _ <- applyOntologyChangeOrNoOp(ontManager,
      new AddImport(ont, owlDataFactory.getOWLImportsDeclaration(closedWorldDefinitions.iri)),
      "addDescriptionBoxExtendsClosedWorldDefinitions error")
    ax = descriptions.DescriptionBoxExtendsClosedWorldDefinitions(
      uuid,
      sourceModule = this.uuid,
      targetModuleIRI = closedWorldDefinitions.iri)
    _ = sig.closedWorldDefinitions += ax
  } yield ax

  def addDescriptionBoxRefinement
  (uuid: api.taggedTypes.DescriptionBoxRefinementUUID,
   refinedDescriptionBox: descriptions.DescriptionBox)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.DescriptionBoxRefinement
  = for {
    _ <- applyOntologyChangeOrNoOp(ontManager,
      new AddImport(ont, owlDataFactory.getOWLImportsDeclaration(refinedDescriptionBox.iri)),
      "addDescriptionBoxRefinement error")
    ax = descriptions.DescriptionBoxRefinement(
      uuid,
      sourceModule = this.uuid,
      targetModuleIRI = refinedDescriptionBox.iri)
    _ = sig.descriptionBoxRefinements += ax
  } yield ax

  def createConceptInstance
  (uuid: api.taggedTypes.ConceptInstanceUUID,
   iri: IRI,
   ni: OWLNamedIndividual,
   conceptType: ConceptKind,
   fragment: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ConceptInstance
  = iri2namedIndividual
    .get(iri)
    .fold[OMFError.Throwables \/ descriptions.ConceptInstance] {
    val i = descriptions.ConceptInstance(
      iri = iri,
      uuid = uuid,
      name = fragment,
      ni = ni,
      conceptType = conceptType)
    sig.conceptInstances += i
    iri2namedIndividual += iri -> i
    i.right
  } {
    case ci: descriptions.ConceptInstance =>
      Set[java.lang.Throwable](
        ConceptInstanceAlreadyDefinedException(ElementExceptionKind.ConceptInstance, iri, ci)
      ).left
    case i =>
      Set[java.lang.Throwable](
        InstanceConflictException(ElementExceptionKind.ConceptInstance, iri, i)
      ).left
  }

  def addConceptInstance
  (uuid: api.taggedTypes.ConceptInstanceUUID,
   iri: IRI,
   conceptType: ConceptKind,
   fragment: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ConceptInstance
  = for {
    ni <- owlDataFactory.getOWLNamedIndividual(iri).right[OMFError.Throwables]
    i <- createConceptInstance(uuid, iri, ni, conceptType, fragment)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
          ni,
          createOMLProvenanceAnnotations(uuid))),
        new AddAxiom(ont, owlDataFactory.getOWLClassAssertionAxiom(
          conceptType.e,
          ni,
          createOMLProvenanceAnnotations(uuid)))),
      "addConceptInstance Error")
  } yield i

  def createReifiedRelationshipInstance
  (uuid: api.taggedTypes.ReifiedRelationshipInstanceUUID,
   iri: IRI,
   ni: OWLNamedIndividual,
   relationshipType: ConceptualRelationship,
   fragment: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ReifiedRelationshipInstance
  = iri2namedIndividual
    .get(iri)
    .fold[OMFError.Throwables \/ descriptions.ReifiedRelationshipInstance] {
    val i = descriptions.ReifiedRelationshipInstance(
      iri = iri,
      uuid = uuid,
      name = fragment,
      ni = ni,
      relationshipType = relationshipType)
    sig.reifiedRelationshipInstances += i
    iri2namedIndividual += iri -> i
    i.right
  } {
    case rri: descriptions.ReifiedRelationshipInstance =>
      Set[java.lang.Throwable](
        ReifiedRelationshipInstanceAlreadyDefinedException(ElementExceptionKind.ReifiedRelationshipInstance, iri, rri)
      ).left
    case i =>
      Set[java.lang.Throwable](
        InstanceConflictException(ElementExceptionKind.ConceptInstance, iri, i)
      ).left
  }

  def addReifiedRelationshipInstance
  (uuid: api.taggedTypes.ReifiedRelationshipInstanceUUID,
   iri: IRI,
   relationshipType: ConceptualRelationship,
   fragment: tables.taggedTypes.LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ReifiedRelationshipInstance
  = for {
    ni <- owlDataFactory.getOWLNamedIndividual(iri).right[OMFError.Throwables]
    i <- createReifiedRelationshipInstance(uuid, iri, ni, relationshipType, fragment)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
          ni,
          createOMLProvenanceAnnotations(uuid))),
        new AddAxiom(ont, owlDataFactory.getOWLClassAssertionAxiom(
          relationshipType.e,
          ni,
          createOMLProvenanceAnnotations(uuid)))),
      "addReifiedRelationshipInstance Error")
  } yield i

  def createReifiedRelationshipInstanceDomain
  (uuid: api.taggedTypes.ReifiedRelationshipInstanceDomainUUID,
   rri: descriptions.ReifiedRelationshipInstance,
   source: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ReifiedRelationshipInstanceDomain
  = sig
    .reifiedRelationshipInstanceDomains
    .find {
      case axiom: descriptions.ReifiedRelationshipInstanceDomain =>
        axiom.relationshipInstance == rri &&
        axiom.domain == source
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.ReifiedRelationshipInstanceDomain] {
    val i = descriptions.ReifiedRelationshipInstanceDomain(uuid, rri, source)
    sig.reifiedRelationshipInstanceDomains += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      ReifiedRelationshipInstanceDomainAlreadyDefinedException(AxiomExceptionKind.ReifiedRelationshipInstanceDomain, other)
    ).left
  }

  def addReifiedRelationshipInstanceDomain
  (uuid: api.taggedTypes.ReifiedRelationshipInstanceDomainUUID,
   rri: descriptions.ReifiedRelationshipInstance,
   source: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ReifiedRelationshipInstanceDomain
  = for {
    i <- createReifiedRelationshipInstanceDomain(uuid, rri, source)
    rootRRs = rri.relationshipType.rootCharacterizedEntityRelationships().selectByKindOf { case rr: ReifiedRelationship => rr }
    _ <- applyOntologyChanges(ontManager,
      rootRRs.foldLeft[Seq[AddAxiom]] {
        Seq.empty
      } { case (acc, rr) =>
        acc :+ new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyAssertionAxiom(
          rr.rSource,
          rri.ni,
          source.ni,
          createOMLProvenanceAnnotations(uuid)))
      },
      "addReifiedRelationshipInstanceDomain Error")
  } yield i

  def createReifiedRelationshipInstanceRange
  (uuid: api.taggedTypes.ReifiedRelationshipInstanceRangeUUID,
   rri: descriptions.ReifiedRelationshipInstance,
   target: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ReifiedRelationshipInstanceRange
  = sig
    .reifiedRelationshipInstanceRanges
    .find {
      case axiom: descriptions.ReifiedRelationshipInstanceRange =>
        axiom.relationshipInstance == rri &&
          axiom.range == target
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.ReifiedRelationshipInstanceRange] {
    val i = descriptions.ReifiedRelationshipInstanceRange(uuid, rri, target)
    sig.reifiedRelationshipInstanceRanges += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      ReifiedRelationshipInstanceRangeAlreadyDefinedException(AxiomExceptionKind.ReifiedRelationshipInstanceRange, other)
    ).left
  }

  def addReifiedRelationshipInstanceRange
  (uuid: api.taggedTypes.ReifiedRelationshipInstanceRangeUUID,
   rri: descriptions.ReifiedRelationshipInstance,
   target: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ReifiedRelationshipInstanceRange
  = for {
    i <- createReifiedRelationshipInstanceRange(uuid, rri, target)
    rootRRs = rri.relationshipType.rootCharacterizedEntityRelationships().selectByKindOf { case rr: ReifiedRelationship => rr }
    _ <- applyOntologyChanges(ontManager,
      rootRRs.foldLeft[Seq[AddAxiom]] {
        Seq.empty
      } { case (acc, rr) =>
        acc :+ new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyAssertionAxiom(
          rr.rTarget,
          rri.ni,
          target.ni,
          createOMLProvenanceAnnotations(uuid)))
      },
      "addReifiedRelationshipInstanceRange Error")
  } yield i

  def createUnreifiedRelationshipInstanceTuple
  (uuid: api.taggedTypes.UnreifiedRelationshipInstanceTupleUUID,
   unreifiedRelationship: UnreifiedRelationship,
   source: descriptions.ConceptualEntitySingletonInstance,
   target: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.UnreifiedRelationshipInstanceTuple
  = sig
    .unreifiedRelationshipInstanceTuples
    .find {
      case axiom: descriptions.UnreifiedRelationshipInstanceTuple =>
        axiom.unreifiedRelationship == unreifiedRelationship &&
        axiom.domain == source &&
        axiom.range == target
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.UnreifiedRelationshipInstanceTuple] {
    val i = descriptions.UnreifiedRelationshipInstanceTuple(uuid, unreifiedRelationship, source, target)
    sig.unreifiedRelationshipInstanceTuples += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      UnreifiedRelationshipInstanceTupleAlreadyDefinedException(AxiomExceptionKind.UnreifiedRelationshipInstanceTuple, other)
    ).left
  }

  def addUnreifiedRelationshipInstanceTuple
  (uuid: api.taggedTypes.UnreifiedRelationshipInstanceTupleUUID,
   unreifiedRelationship: UnreifiedRelationship,
   source: descriptions.ConceptualEntitySingletonInstance,
   target: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.UnreifiedRelationshipInstanceTuple
  = for {
    i <- createUnreifiedRelationshipInstanceTuple(uuid, unreifiedRelationship, source, target)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyAssertionAxiom(
          unreifiedRelationship.e,
          source.ni,
          target.ni,
          createOMLProvenanceAnnotations(uuid)))),
      "addUnreifiedRelationshipInstanceTuple Error")
  } yield i

  ///

  def createInstanceRelationshipEnumerationRestriction
  (uuid: api.taggedTypes.InstanceRelationshipEnumerationRestrictionUUID,
   restrictedRelationship: RestrictableRelationship,
   domain: descriptions.ConceptualEntitySingletonInstance,
   references: Vector[descriptions.ConceptualEntitySingletonInstance])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.InstanceRelationshipEnumerationRestriction
  = sig
    .instanceRelationshipEnumerationRestrictions
    .find {
      case axiom: descriptions.InstanceRelationshipEnumerationRestriction =>
        axiom.restrictedRelationship == restrictedRelationship &&
          axiom.domain == domain &&
          axiom.references == references
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.InstanceRelationshipEnumerationRestriction] {
    val i = descriptions.InstanceRelationshipEnumerationRestriction(uuid, restrictedRelationship, domain, references)
    sig.instanceRelationshipEnumerationRestrictions += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      InstanceRelationshipEnumerationRestrictionAlreadyDefinedException(AxiomExceptionKind.InstanceRelationshipEnumerationRestriction, other)
    ).left
  }

  def addInstanceRelationshipEnumerationRestriction
  (uuid: api.taggedTypes.InstanceRelationshipEnumerationRestrictionUUID,
   restrictedRelationship: RestrictableRelationship,
   domain: descriptions.ConceptualEntitySingletonInstance,
   references: Vector[descriptions.ConceptualEntitySingletonInstance])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.InstanceRelationshipEnumerationRestriction
  = for {
    i <- createInstanceRelationshipEnumerationRestriction(uuid, restrictedRelationship, domain, references)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory.getOWLClassAssertionAxiom(
            owlDataFactory.getOWLObjectAllValuesFrom(
              restrictedRelationship.e,
               owlDataFactory.getOWLObjectOneOf(references.map(_.ni): _*)),
            domain.ni,
            createOMLProvenanceAnnotations(uuid)))),
      "addInstanceRelationshipEnumerationRestriction Error")
  } yield i

  ///

  def createInstanceRelationshipValueRestriction
  (uuid: api.taggedTypes.InstanceRelationshipValueRestrictionUUID,
   restrictedRelationship: RestrictableRelationship,
   domain: descriptions.ConceptualEntitySingletonInstance,
   range: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.InstanceRelationshipValueRestriction
  = sig
    .instanceRelationshipValueRestrictions
    .find {
      case axiom: descriptions.InstanceRelationshipValueRestriction =>
        axiom.restrictedRelationship == restrictedRelationship &&
          axiom.domain == domain &&
          axiom.range == range
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.InstanceRelationshipValueRestriction] {
    val i = descriptions.InstanceRelationshipValueRestriction(uuid, restrictedRelationship, domain, range)
    sig.instanceRelationshipValueRestrictions += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      InstanceRelationshipValueRestrictionAlreadyDefinedException(AxiomExceptionKind.InstanceRelationshipValueRestriction, other)
    ).left
  }

  def addInstanceRelationshipValueRestriction
  (uuid: api.taggedTypes.InstanceRelationshipValueRestrictionUUID,
   restrictedRelationship: RestrictableRelationship,
   domain: descriptions.ConceptualEntitySingletonInstance,
   range: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.InstanceRelationshipValueRestriction
  = for {
    i <- createInstanceRelationshipValueRestriction(uuid, restrictedRelationship, domain, range)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory.getOWLClassAssertionAxiom(
            owlDataFactory.getOWLObjectHasValue(
              restrictedRelationship.e,
              range.ni),
            domain.ni,
            createOMLProvenanceAnnotations(uuid)))),
      "addInstanceRelationshipValueRestriction Error")
  } yield i

  ///

  def createInstanceRelationshipExistentialRangeRestriction
  (uuid: api.taggedTypes.InstanceRelationshipExistentialRangeRestrictionUUID,
   restrictedRelationship: RestrictableRelationship,
   domain: descriptions.ConceptualEntitySingletonInstance,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.InstanceRelationshipExistentialRangeRestriction
  = sig
    .instanceRelationshipExistentialRangeRestrictions
    .find {
      case axiom: descriptions.InstanceRelationshipExistentialRangeRestriction =>
        axiom.restrictedRelationship == restrictedRelationship &&
          axiom.domain == domain &&
          axiom.range == range
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.InstanceRelationshipExistentialRangeRestriction] {
    val i = descriptions.InstanceRelationshipExistentialRangeRestriction(uuid, restrictedRelationship, domain, range)
    sig.instanceRelationshipExistentialRangeRestrictions += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      InstanceRelationshipExistentialRangeRestrictionAlreadyDefinedException(AxiomExceptionKind.InstanceRelationshipExistentialRangeRestriction, other)
    ).left
  }

  def addInstanceRelationshipExistentialRangeRestriction
  (uuid: api.taggedTypes.InstanceRelationshipExistentialRangeRestrictionUUID,
   restrictedRelationship: RestrictableRelationship,
   domain: descriptions.ConceptualEntitySingletonInstance,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.InstanceRelationshipExistentialRangeRestriction
  = for {
    i <- createInstanceRelationshipExistentialRangeRestriction(uuid, restrictedRelationship, domain, range)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory.getOWLClassAssertionAxiom(
            owlDataFactory.getOWLObjectSomeValuesFrom(
              restrictedRelationship.e,
              range.e),
            domain.ni,
            createOMLProvenanceAnnotations(uuid)))),
      "addInstanceRelationshipExistentialRangeRestriction Error")
  } yield i

  ///

  def createInstanceRelationshipUniversalRangeRestriction
  (uuid: api.taggedTypes.InstanceRelationshipUniversalRangeRestrictionUUID,
   restrictedRelationship: RestrictableRelationship,
   domain: descriptions.ConceptualEntitySingletonInstance,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.InstanceRelationshipUniversalRangeRestriction
  = sig
    .instanceRelationshipUniversalRangeRestrictions
    .find {
      case axiom: descriptions.InstanceRelationshipUniversalRangeRestriction =>
        axiom.restrictedRelationship == restrictedRelationship &&
          axiom.domain == domain &&
          axiom.range == range
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.InstanceRelationshipUniversalRangeRestriction] {
    val i = descriptions.InstanceRelationshipUniversalRangeRestriction(uuid, restrictedRelationship, domain, range)
    sig.instanceRelationshipUniversalRangeRestrictions += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      InstanceRelationshipUniversalRangeRestrictionAlreadyDefinedException(AxiomExceptionKind.InstanceRelationshipUniversalRangeRestriction, other)
    ).left
  }

  def addInstanceRelationshipUniversalRangeRestriction
  (uuid: api.taggedTypes.InstanceRelationshipUniversalRangeRestrictionUUID,
   restrictedRelationship: RestrictableRelationship,
   domain: descriptions.ConceptualEntitySingletonInstance,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.InstanceRelationshipUniversalRangeRestriction
  = for {
    i <- createInstanceRelationshipUniversalRangeRestriction(uuid, restrictedRelationship, domain, range)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory.getOWLClassAssertionAxiom(
            owlDataFactory.getOWLObjectSomeValuesFrom(
              restrictedRelationship.e,
              range.e),
            domain.ni,
            createOMLProvenanceAnnotations(uuid)))),
      "addInstanceRelationshipUniversalRangeRestriction Error")
  } yield i

  ///

  def createSingletonInstanceScalarDataPropertyValue
  (uuid: api.taggedTypes.SingletonInstanceScalarDataPropertyValueUUID,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2sc: EntityScalarDataProperty,
   value: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.SingletonInstanceScalarDataPropertyValue
  = sig
    .singletonScalarDataPropertyValues
    .find {
      case axiom: descriptions.SingletonInstanceScalarDataPropertyValue =>
        axiom.ei == ei &&
          axiom.dataRelationship == e2sc
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.SingletonInstanceScalarDataPropertyValue] {
    val i = descriptions.SingletonInstanceScalarDataPropertyValue(uuid, ei, e2sc, value, valueType)
    sig.singletonScalarDataPropertyValues += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      SingletonInstanceScalarDataPropertyValueAlreadyDefinedException(
        AxiomExceptionKind.SingletonInstanceScalarDataPropertyValue, other)
    ).left
  }

  def addSingletonInstanceScalarDataPropertyValue
  (uuid: api.taggedTypes.SingletonInstanceScalarDataPropertyValueUUID,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2sc: EntityScalarDataProperty,
   value: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.SingletonInstanceScalarDataPropertyValue
  = for {
    i <- createSingletonInstanceScalarDataPropertyValue(uuid, ei, e2sc, value, valueType)
    lit = LiteralConversions.toOWLLiteral(value, owlDataFactory, valueType.map(_.e).orElse(Option.apply(e2sc.range.e)))
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory
            .getOWLDataPropertyAssertionAxiom(e2sc.e, ei.ni, lit, createOMLProvenanceAnnotations(uuid)))),
      "addSingletonInstanceScalarDataPropertyValue Error")
  } yield i

  def createSingletonInstanceStructuredDataPropertyValue
  (uuid: api.taggedTypes.SingletonInstanceStructuredDataPropertyValueUUID,
   ni: OWLNamedIndividual,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2st: EntityStructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.SingletonInstanceStructuredDataPropertyValue
  = sig
    .singletonStructuredDataPropertyValues
    .find {
      case axiom: descriptions.SingletonInstanceStructuredDataPropertyValue =>
        axiom.ni == ni &&
          axiom.ei == ei &&
          axiom.structuredDataProperty == e2st
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.SingletonInstanceStructuredDataPropertyValue] {
    val i = descriptions.SingletonInstanceStructuredDataPropertyValue(uuid, ni, ei, e2st)
    sig.singletonStructuredDataPropertyValues += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      SingletonInstanceStructuredDataPropertyValueAlreadyDefinedException(
        AxiomExceptionKind.SingletonInstanceStructuredDataPropertyValue, other)
    ).left
  }

  def addSingletonInstanceStructuredDataPropertyValue
  (uuid: api.taggedTypes.SingletonInstanceStructuredDataPropertyValueUUID,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2st: EntityStructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.SingletonInstanceStructuredDataPropertyValue
  = for {
    ni_iri <- ops.withFragment(iri, tables.taggedTypes.localName(uuid.toString))
    ni = owlDataFactory.getOWLNamedIndividual(ni_iri)
    i <- createSingletonInstanceStructuredDataPropertyValue(uuid, ni, ei, e2st)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory.getOWLClassAssertionAxiom(
          e2st.range.e,
          ni,
          createOMLProvenanceAnnotations(uuid))),
        new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyAssertionAxiom(
          e2st.e,
          ei.ni,
          ni,
          createOMLProvenanceAnnotations(uuid)))),
      "addSingletonInstanceStructuredDataPropertyValue Error")
  } yield i

  def createScalarDataPropertyValue
  (uuid: api.taggedTypes.ScalarDataPropertyValueUUID,
   context: SingletonInstanceStructuredDataPropertyContext,
   s2sc: ScalarDataProperty,
   value: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ScalarDataPropertyValue
  = sig
    .scalarDataPropertyValues
    .find {
      case axiom: descriptions.ScalarDataPropertyValue =>
        axiom.context == context &&
          axiom.dataRelationship == s2sc
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.ScalarDataPropertyValue] {
    val i = descriptions.ScalarDataPropertyValue(uuid, context, s2sc, value, valueType)
    sig.scalarDataPropertyValues += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      ScalarDataPropertyValueAlreadyDefinedException(
        AxiomExceptionKind.ScalarDataPropertyValue, other)
    ).left
  }

  def makeScalarDataPropertyValue
  (uuid: api.taggedTypes.ScalarDataPropertyValueUUID,
   context: SingletonInstanceStructuredDataPropertyContext,
   s2sc: ScalarDataProperty,
   value: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ScalarDataPropertyValue
  = for {
    i <- createScalarDataPropertyValue(uuid, context, s2sc, value, valueType)
    lit = LiteralConversions.toOWLLiteral(value, owlDataFactory, valueType.map(_.e).orElse(Option.apply(s2sc.range.e)))
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory.getOWLDataPropertyAssertionAxiom(
          s2sc.e, context.ni,
          lit,
          createOMLProvenanceAnnotations(uuid)))),
      "makeScalarDataPropertyValue Error")
  } yield i

  def createStructuredDataPropertyTuple
  (uuid: api.taggedTypes.StructuredDataPropertyTupleUUID,
   ni: OWLNamedIndividual,
   context: SingletonInstanceStructuredDataPropertyContext,
   s2st: StructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.StructuredDataPropertyTuple
  = sig
    .structuredDataPropertyTuples
    .find {
      case axiom: descriptions.StructuredDataPropertyTuple =>
        axiom.context == context &&
          axiom.structuredDataProperty == s2st
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ descriptions.StructuredDataPropertyTuple] {
    val i = descriptions.StructuredDataPropertyTuple(uuid, ni, context, s2st)
    sig.structuredDataPropertyTuples += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      StructuredDataPropertyTupleAlreadyDefinedException(
        AxiomExceptionKind.StructuredDataPropertyTuple, other)
    ).left
  }

  def makeStructuredDataPropertyTuple
  (uuid: api.taggedTypes.StructuredDataPropertyTupleUUID,
   context: SingletonInstanceStructuredDataPropertyContext,
   s2st: StructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.StructuredDataPropertyTuple
  = for {
    ni_iri <- ops.withFragment(iri, tables.taggedTypes.localName(uuid.toString))
    ni = owlDataFactory.getOWLNamedIndividual(ni_iri)
    i <- createStructuredDataPropertyTuple(uuid, ni, context, s2st)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory.getOWLClassAssertionAxiom(
          s2st.range.e,
          ni,
          createOMLProvenanceAnnotations(uuid))),
        new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyAssertionAxiom(
          s2st.e,
          context.ni,
          ni,
          createOMLProvenanceAnnotations(uuid)))),
      "makeStructuredDataPropertyTuple Error")
  } yield i

}