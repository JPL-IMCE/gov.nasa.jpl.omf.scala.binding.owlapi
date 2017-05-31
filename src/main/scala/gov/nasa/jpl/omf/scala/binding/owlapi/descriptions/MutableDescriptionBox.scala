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
import gov.nasa.jpl.omf.scala.binding.owlapi.common.MutableModule
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.TerminologyBox
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import gov.nasa.jpl.omf.scala.core.{DescriptionBoxSignature, DescriptionKind, MutableDescriptionBoxSignature}
import gov.nasa.jpl.omf.scala.core.OMLString.{LexicalValue, LocalName}
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.IRI

import scala.collection.immutable._
import scala.collection.mutable.{HashMap, HashSet}
import scala.{Any, Boolean, None, Some}
import scala.Predef.{ArrowAssoc,String}
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
  : Throwables \/ MutableDescriptionBox
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
    backbone = backbone)(ops).right[Throwables]
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
  : Throwables \/ AnnotationProperty
  = {
    sig.annotationProperties += ap
    ap.right
  }

  def addAnnotation
  (subject: OWLAPIOMF#Element,
   property: AnnotationProperty,
   value: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ AnnotationEntry
  = {
    val a = AnnotationEntry(
      moduleUUID=uuid.toString,
      subjectUUID=subject.uuid.toString,
      value)
    sig.annotations.find { case (ap, _) => ap == property } match {
      case Some((ap, aes)) =>
        sig.annotations -= property -> aes
        sig.annotations += property -> (aes + a)
      case None =>
        sig.annotations += property -> (Set.empty[AnnotationEntry] + a)
    }
    a.right
  }

  def removeAnnotations
  (subject: OWLAPIOMF#Element,
   property: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ Set[AnnotationEntry]
  = {
    val sUUID = subject.uuid.toString
    sig.annotations.find { case (ap, _) => ap == property } match {
      case Some((ap, aes)) =>
        sig.annotations -= property -> aes
        val removed = aes.filter(_.subjectUUID == sUUID)
        sig.annotations += property -> (aes -- removed)
        removed.right
      case None =>
        Set.empty[AnnotationEntry].right
    }
  }

  def addDescriptionBoxExtendsClosedWorldDefinitions
  (uuid: UUID,
   closedWorldDefinitions: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ descriptions.DescriptionBoxExtendsClosedWorldDefinitions
  = {
    val i = descriptions.DescriptionBoxExtendsClosedWorldDefinitions(
      uuid,
      sourceModule = this.uuid,
      targetModule = closedWorldDefinitions)
    sig.closedWorldDefinitions += i
    // @TODO: create the OWL representation
    i.right
  }

  def addDescriptionBoxRefinement
  (uuid: UUID,
   refinedDescriptionBox: descriptions.DescriptionBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ descriptions.DescriptionBoxRefinement
  = {
    val i = descriptions.DescriptionBoxRefinement(
      uuid,
      sourceModule = this.uuid,
      targetModule = refinedDescriptionBox)
    sig.descriptionBoxRefinements += i
    // @TODO: create the OWL representation
    i.right
  }

  def addConceptInstance
  (uuid: UUID,
   iri: IRI,
   conceptType: Concept,
   fragment: LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ descriptions.ConceptInstance
  = {
    val i = descriptions.ConceptInstance(
      iri = iri,
      uuid = uuid,
      name = fragment,
      conceptType = conceptType)
    sig.conceptInstances += i
    // @TODO: create the OWL representation
    i.right
  }

  def addReifiedRelationshipInstance
  (uuid: UUID,
   iri: IRI,
   relationshipType: ReifiedRelationship,
   fragment: LocalName)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ descriptions.ReifiedRelationshipInstance
  = {
    val i = descriptions.ReifiedRelationshipInstance(
      iri = iri,
      uuid = uuid,
      name = fragment,
      relationshipType = relationshipType)
    sig.reifiedRelationshipInstances += i
    // @TODO: create the OWL representation
    i.right
  }

  def addReifiedRelationshipInstanceDomain
  (uuid: UUID,
   relationshipInstance: descriptions.ReifiedRelationshipInstance,
   source: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ descriptions.ReifiedRelationshipInstanceDomain
  = {
    val i = descriptions.ReifiedRelationshipInstanceDomain(uuid, relationshipInstance, source)
    sig.reifiedRelationshipInstanceDomains += i
    // @TODO: create the OWL representation
    i.right
  }

  def addReifiedRelationshipInstanceRange
  (uuid: UUID,
   relationshipInstance: descriptions.ReifiedRelationshipInstance,
   target: descriptions.ConceptualEntitySingletonInstance)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ descriptions.ReifiedRelationshipInstanceRange
  = {
    val i = descriptions.ReifiedRelationshipInstanceRange(uuid, relationshipInstance, target)
    sig.reifiedRelationshipInstanceRanges += i
    // @TODO: create the OWL representation
    i.right
  }

  def addSingletonInstanceScalarDataPropertyValue
  (uuid: UUID,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2sc: EntityScalarDataProperty,
   value: LexicalValue)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ descriptions.SingletonInstanceScalarDataPropertyValue
  = {
    val i = descriptions.SingletonInstanceScalarDataPropertyValue(uuid, ei, e2sc, value)
    sig.singletonScalarDataPropertyValues += i
    // @TODO: create the OWL representation
    i.right
  }

  def addSingletonInstanceStructuredDataPropertyValue
  (uuid: UUID,
   ei: descriptions.ConceptualEntitySingletonInstance,
   e2st: EntityStructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ descriptions.SingletonInstanceStructuredDataPropertyValue
  = {
    val i = descriptions.SingletonInstanceStructuredDataPropertyValue(uuid, ei, e2st)
    sig.singletonStructuredDataPropertyValues += i
    // @TODO: create the OWL representation
    i.right
  }

  def makeScalarDataPropertyValue
  (uuid: UUID,
   context: SingletonInstanceStructuredDataPropertyContext,
   s2sc: ScalarDataProperty,
   value: LexicalValue)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ descriptions.ScalarDataPropertyValue
  = {
    val i = descriptions.ScalarDataPropertyValue(uuid, context, s2sc, value)
    sig.scalarDataPropertyValues += i
    // @TODO: create the OWL representation
    i.right
  }

  def makeStructuredDataPropertyTuple
  (uuid: UUID,
   context: SingletonInstanceStructuredDataPropertyContext,
   s2st: StructuredDataProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ descriptions.StructuredDataPropertyTuple
  = {
    val i = descriptions.StructuredDataPropertyTuple(uuid, context, s2st)
    sig.structuredDataPropertyTuples += i
    // @TODO: create the OWL representation
    i.right
  }

}