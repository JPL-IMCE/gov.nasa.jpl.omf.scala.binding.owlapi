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

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.{AnnotationEntry, AnnotationProperty}
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{MutableModule, Resource}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.TerminologyBox
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.OMFError
import gov.nasa.jpl.omf.scala.core.{DescriptionBoxSignature, DescriptionKind, MutableDescriptionBoxSignature}
import gov.nasa.jpl.omf.scala.core.OMLString.{LexicalValue, LocalName}
import org.semanticweb.owlapi.model._

import scala.collection.immutable._
import scala.collection.mutable.{HashMap, HashSet}
import scala.compat.java8.StreamConverters._
import scala.{Any, Boolean, None, Some, StringContext}
import scala.Predef.{ArrowAssoc, String}
import scalaz._
import Scalaz._

object MutableDescriptionBox {

  def initialize
  (uuid: UUID,
   name: LocalName,
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
      singletonScalarDataPropertyValues = HashSet.empty[SingletonInstanceScalarDataPropertyValue],
      singletonStructuredDataPropertyValues = HashSet.empty[SingletonInstanceStructuredDataPropertyValue],
      scalarDataPropertyValues = HashSet.empty[ScalarDataPropertyValue],
      structuredDataPropertyTuples = HashSet.empty[StructuredDataPropertyTuple],
      annotationProperties =
        HashSet.empty[AnnotationProperty],
      annotations =
        HashSet.empty[(AnnotationProperty, scala.collection.immutable.Set[AnnotationEntry])]),
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

  def addAnnotationProperty
  (ap: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ AnnotationProperty
  = for {
    _ <- (sig.annotationProperties += ap).right[OMFError.Throwables]
    ont_ap = owlDataFactory.getOWLAnnotationProperty(ap.iri)
    _ <- applyOntologyChangeOrNoOp(ontManager,
      new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(ont_ap)),
      "addAnnotationProperty error")
  } yield ap

  def addAnnotation
  (subject: OWLAPIOMF#Element,
   property: AnnotationProperty,
   value: String)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ AnnotationEntry
  = for {
    a <- AnnotationEntry(
      moduleUUID = uuid.toString,
      subjectUUID = subject.uuid.toString,
      value).right[OMFError.Throwables]
    _ = sig.annotations.find { case (ap, _) => ap == property } match {
      case Some((ap, aes)) =>
        sig.annotations -= property -> aes
        sig.annotations += property -> (aes + a)
      case None =>
        sig.annotations += property -> (Set.empty[AnnotationEntry] + a)
    }
    ont_ap = owlDataFactory.getOWLAnnotationProperty(property.iri)
    ont_lit = owlDataFactory.getOWLLiteral(value)
    _ <- subject match {
      case m: MutableDescriptionBox =>
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
  (subject: OWLAPIOMF#Element,
   property: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ Set[AnnotationEntry]
  = for {
    sUUID <- subject.uuid.toString.right[OMFError.Throwables]
    ae <- sig.annotations.find { case (ap, _) => ap == property } match {
      case Some((ap, aes)) =>
        sig.annotations -= property -> aes
        val removed = aes.filter(_.subjectUUID == sUUID)
        sig.annotations += property -> (aes -- removed)
        removed.right
      case None =>
        Set.empty[AnnotationEntry].right
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
  (uuid: UUID,
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
      targetModule = closedWorldDefinitions)
    _ = sig.closedWorldDefinitions += ax
  } yield ax

  def addDescriptionBoxRefinement
  (uuid: UUID,
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
      targetModule = refinedDescriptionBox)
    _ = sig.descriptionBoxRefinements += ax
  } yield ax

  def createConceptInstance
  (uuid: UUID,
   iri: IRI,
   ni: OWLNamedIndividual,
   conceptType: Concept,
   fragment: LocalName)
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
  (uuid: UUID,
   iri: IRI,
   conceptType: Concept,
   fragment: LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ConceptInstance
  = for {
    ni <- owlDataFactory.getOWLNamedIndividual(iri).right[OMFError.Throwables]
    i <- createConceptInstance(uuid, iri, ni, conceptType, fragment)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory
            .getOWLDeclarationAxiom(ni)),
        new AddAxiom(ont,
          owlDataFactory
            .getOWLClassAssertionAxiom(conceptType.e, ni))),
      "addConceptInstance Error")
  } yield i

  def createReifiedRelationshipInstance
  (uuid: UUID,
   iri: IRI,
   ni: OWLNamedIndividual,
   relationshipType: ReifiedRelationship,
   fragment: LocalName)
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
  (uuid: UUID,
   iri: IRI,
   relationshipType: ReifiedRelationship,
   fragment: LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ReifiedRelationshipInstance
  = for {
    ni <- owlDataFactory.getOWLNamedIndividual(iri).right[OMFError.Throwables]
    i <- createReifiedRelationshipInstance(uuid, iri, ni, relationshipType, fragment)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory
            .getOWLDeclarationAxiom(ni)),
        new AddAxiom(ont,
          owlDataFactory
            .getOWLClassAssertionAxiom(relationshipType.e, ni))),
      "addReifiedRelationshipInstance Error")
  } yield i

  def createReifiedRelationshipInstanceDomain
  (uuid: UUID,
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
  (uuid: UUID,
   rri: descriptions.ReifiedRelationshipInstance,
   source: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ReifiedRelationshipInstanceDomain
  = for {
    i <- createReifiedRelationshipInstanceDomain(uuid, rri, source)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(rri.relationshipType.rSource, rri.ni, source.ni))),
      "addReifiedRelationshipInstanceDomain Error")
  } yield i

  def createReifiedRelationshipInstanceRange
  (uuid: UUID,
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
  (uuid: UUID,
   rri: descriptions.ReifiedRelationshipInstance,
   target: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ReifiedRelationshipInstanceRange
  = for {
    i <- createReifiedRelationshipInstanceRange(uuid, rri, target)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(rri.relationshipType.rTarget, rri.ni, target.ni))),
      "addReifiedRelationshipInstanceRange Error")
  } yield i

  def createUnreifiedRelationshipInstanceTuple
  (uuid: UUID,
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
  (uuid: UUID,
   unreifiedRelationship: UnreifiedRelationship,
   source: descriptions.ConceptualEntitySingletonInstance,
   target: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.UnreifiedRelationshipInstanceTuple
  = for {
    i <- createUnreifiedRelationshipInstanceTuple(uuid, unreifiedRelationship, source, target)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(unreifiedRelationship.e, source.ni, target.ni))),
      "addUnreifiedRelationshipInstanceTuple Error")
  } yield i

  def createSingletonInstanceScalarDataPropertyValue
  (uuid: UUID,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2sc: EntityScalarDataProperty,
   value: LexicalValue)
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
    val i = descriptions.SingletonInstanceScalarDataPropertyValue(uuid, ei, e2sc, value)
    sig.singletonScalarDataPropertyValues += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      SingletonInstanceScalarDataPropertyValueAlreadyDefinedException(
        AxiomExceptionKind.SingletonInstanceScalarDataPropertyValue, other)
    ).left
  }

  def addSingletonInstanceScalarDataPropertyValue
  (uuid: UUID,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2sc: EntityScalarDataProperty,
   value: LexicalValue)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.SingletonInstanceScalarDataPropertyValue
  = for {
    i <- createSingletonInstanceScalarDataPropertyValue(uuid, ei, e2sc, value)
    lit = owlDataFactory.getOWLLiteral(value, e2sc.range.e)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory
            .getOWLDataPropertyAssertionAxiom(e2sc.e, ei.ni, lit))),
      "addSingletonInstanceScalarDataPropertyValue Error")
  } yield i

  def createSingletonInstanceStructuredDataPropertyValue
  (uuid: UUID,
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
  (uuid: UUID,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2st: EntityStructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.SingletonInstanceStructuredDataPropertyValue
  = for {
    ni_iri <- ops.withFragment(iri, LocalName(uuid.toString))
    ni = owlDataFactory.getOWLNamedIndividual(ni_iri)
    i <- createSingletonInstanceStructuredDataPropertyValue(uuid, ni, ei, e2st)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory
            .getOWLClassAssertionAxiom(e2st.range.e, ni)),
        new AddAxiom(ont,
          owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(e2st.e, ei.ni, ni))),
      "addSingletonInstanceStructuredDataPropertyValue Error")
  } yield i

  def createScalarDataPropertyValue
  (uuid: UUID,
   context: SingletonInstanceStructuredDataPropertyContext,
   s2sc: ScalarDataProperty,
   value: LexicalValue)
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
    val i = descriptions.ScalarDataPropertyValue(uuid, context, s2sc, value)
    sig.scalarDataPropertyValues += i
    i.right
  } { other =>
    Set[java.lang.Throwable](
      ScalarDataPropertyValueAlreadyDefinedException(
        AxiomExceptionKind.ScalarDataPropertyValue, other)
    ).left
  }

  def makeScalarDataPropertyValue
  (uuid: UUID,
   context: SingletonInstanceStructuredDataPropertyContext,
   s2sc: ScalarDataProperty,
   value: LexicalValue)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.ScalarDataPropertyValue
  = for {
    i <- createScalarDataPropertyValue(uuid, context, s2sc, value)
    lit = owlDataFactory.getOWLLiteral(value, s2sc.range.e)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory
            .getOWLDataPropertyAssertionAxiom(s2sc.e, context.ni, lit))),
      "makeScalarDataPropertyValue Error")
  } yield i

  def createStructuredDataPropertyTuple
  (uuid: UUID,
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
  (uuid: UUID,
   context: SingletonInstanceStructuredDataPropertyContext,
   s2st: StructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ descriptions.StructuredDataPropertyTuple
  = for {
    ni_iri <- ops.withFragment(iri, LocalName(uuid.toString))
    ni = owlDataFactory.getOWLNamedIndividual(ni_iri)
    i <- createStructuredDataPropertyTuple(uuid, ni, context, s2st)
    _ <- applyOntologyChanges(ontManager,
      Seq(
        new AddAxiom(ont,
          owlDataFactory
            .getOWLClassAssertionAxiom(s2st.range.e, ni)),
        new AddAxiom(ont,
          owlDataFactory
            .getOWLObjectPropertyAssertionAxiom(s2st.e, context.ni, ni))),
      "makeStructuredDataPropertyTuple Error")
  } yield i

}