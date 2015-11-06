/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2015, California Institute of Technology ("Caltech").
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
package gov.nasa.jpl.omf.scala.binding.owlapi.types

import java.lang.System

import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.ConstrainingFacet
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.ChangeApplied

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.{Boolean,Enumeration,Option,None,Some,StringContext,Unit}
import scala.Predef.{Set=>_,Map=>_,_}
import scala.language.postfixOps
import scalaz._, Scalaz._

object EntityExceptionKind extends Enumeration {
  type EntityExceptionKind = Value
  val EntityAspect,
  EntityConcept,
  EntityReifiedRelationship,
  DataRelationshipFromEntityToScalar,
  ScalarDataType,
  StructuredDataType = Value
}

import gov.nasa.jpl.omf.scala.binding.owlapi.types.EntityExceptionKind._

object RelationshipScopeAccessKind extends Enumeration {
  type RelationshipScopeAccessKind = Value
  val Source, Target = Value
}

import gov.nasa.jpl.omf.scala.binding.owlapi.types.RelationshipScopeAccessKind._

object AxiomExceptionKind extends Enumeration {
  type AxiomExceptionKind = Value
  val AspectSubclassAxiomException,
  ConceptSubclassAxiomException,
  ConceptRestrictionAxiomException,
  ReifiedRelationshipSubclassAxiomException,
  EntityDefinitionAspectSubClassAxiomException,
  EntityConceptSubClassAxiomException,
  EntityReifiedRelationshipSubClassAxiomException,
  EntityConceptDesignationTerminologyGraphAxiomException,
  ScalarDataTypeFacetRestrictionAxiomException = Value
}

import gov.nasa.jpl.omf.scala.binding.owlapi.types.AxiomExceptionKind._

object AxiomScopeAccessKind extends Enumeration {
  type AxiomScopeAccessKind = Value
  val Sub, Sup, Rel, Range = Value
}

import gov.nasa.jpl.omf.scala.binding.owlapi.types.AxiomScopeAccessKind._

sealed abstract class MutableModelTerminologyGraphException
(override val message: String)
  extends OMFError.OMFException(message) {
  require(null != message)
}

case class EntityAlreadyDefinedException
(kind: EntityExceptionKind,
 iri: IRI,
 term: ModelTypeTerm)
  extends MutableModelTerminologyGraphException(s"Cannot create $kind with IRI='$iri'" +
                                                s" because it is already defined as: $term") {

  require(null != kind)
  require(null != iri)
  require(null != term)
}

case class EntityConflictException
(kind: EntityExceptionKind,
 iri: IRI,
 conflictingTerm: ModelTypeTerm)
  extends MutableModelTerminologyGraphException(s"Cannot create $kind with IRI='$iri' " +
                                                s"because this IRI refers to: $conflictingTerm") {

  require(null != kind)
  require(null != iri)
  require(null != conflictingTerm)
}

case class EntityScopeException
(kind: EntityExceptionKind,
 iri: IRI,
 unaccessibleTerms: Map[RelationshipScopeAccessKind, ModelTypeTerm])
  extends MutableModelTerminologyGraphException(
                                                 s"""Cannot create $kind with IRI='$iri' because
                                                     |there are ${unaccessibleTerms.size} terms out of scope of
                                                     |the graph: """.stripMargin +
                                                 (unaccessibleTerms.map { case (k, t) => s"$k: $t" } mkString ", "))

case class AxiomScopeException
(kind: AxiomExceptionKind,
 unaccessibleTerms: Map[AxiomScopeAccessKind, ModelTypeTerm])
  extends MutableModelTerminologyGraphException(
                                                 s"""Cannot create $kind because
                                                     |there are ${unaccessibleTerms.size} terms out of scope of
                                                     |the graph: """.stripMargin +
                                                 (unaccessibleTerms.map { case (k, t) => s"$k: $t" } mkString ", "))

case class DuplicateModelTermAxiomException
(kind: AxiomExceptionKind,
 axiom: ModelTermAxiom)
  extends MutableModelTerminologyGraphException(
                                                 s"""Cannot create $kind because
                                                     |the axiom is already asserted $axiom""".stripMargin)

case class MutableModelTerminologyGraph
(override val kind: TerminologyKind,
 override val ont: OWLOntology,
 backbone: OMFBackbone)
(override implicit val ops: OWLAPIOMFOps)
  extends ModelTerminologyGraph(kind, ont)(ops) {

  override val isImmutableModelTerminologyGraph = true
  override val isMutableModelTerminologyGraph = false

  override val kindIRI: IRI = makeKindIRI("mutable")

  val LOG: Boolean = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.MutableModelTerminologyGraph")

  def setTerminologyGraphShortName
  (shortName: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    for {
      c1 <-
        getTerminologyGraphShortNameAnnotation
        .fold[NonEmptyList[java.lang.Throwable] \/ Unit](
          \/-(())
        ){ annotation =>
          applyOntologyChange(
            ontManager,
            new RemoveOntologyAnnotation(ont, annotation),
            ifError="Failed to remove the tbox ontology 'rdfs:label' annotation")
        }
      c2 <-
        shortName
        .fold[NonEmptyList[java.lang.Throwable] \/ Unit](
          \/-(())
        ){ label =>
          applyOntologyChange(
            ontManager,
            new AddOntologyAnnotation(
              ont,
              owlDataFactory.getOWLAnnotation(omfStore.RDFS_LABEL, owlDataFactory.getOWLLiteral(label))),
            ifError="Failed to add the tbox ontology 'rdfs:label' annotation",
            ifSuccess=(() => {
              omfStore.setTerminologyGraphShortName(this, label)
              if (LOG)
                System.out.println(s"setTerminologyGraphShortName: $kindIRI name='$label'")
            }).some)
          }
    } yield ()

  def setTerminologyGraphUUID
  (uuid: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    for {
      c1 <-
        getTerminologyGraphUUIDAnnotation
          .fold[NonEmptyList[java.lang.Throwable] \/ Unit](
          \/-(())
        ) { annotation =>
          applyOntologyChange(
            ontManager,
            new RemoveOntologyAnnotation(ont, annotation),
            ifError = "Failed to remove the tbox ontology 'uuid' annotation")
        }
      c2 <-
        uuid
        .fold[NonEmptyList[java.lang.Throwable] \/ Unit](
          \/-(())
        ) { id =>
          applyOntologyChange(
            ontManager,
            new AddOntologyAnnotation(
              ont,
              owlDataFactory.getOWLAnnotation(omfStore.ANNOTATION_HAS_UUID, owlDataFactory.getOWLLiteral(id))),
            ifError = "Failed to add the tbox ontology 'uuid' annotation",
            ifSuccess = (() => {
              omfStore.setTerminologyGraphUUID(this, id)
              if (LOG)
                System.out.println(s"setTerminologyGraphUUID: $kindIRI uuid='$id'")
            }).some)
        }
    } yield ()


  import ops._

  val rdfs_labelAP = owlDataFactory.getOWLAnnotationProperty(rdfs_label)
  val isAbstractAP = owlDataFactory.getOWLAnnotationProperty(AnnotationIsAbstract)
  val isDerivedAP = owlDataFactory.getOWLAnnotationProperty(AnnotationIsDerived)

  override protected val aspects =
    scala.collection.mutable.ListBuffer[ModelEntityAspect]()

  override protected val concepts =
    scala.collection.mutable.ListBuffer[ModelEntityConcept]()

  override protected val reifiedRelationships =
    scala.collection.mutable.ListBuffer[ModelEntityReifiedRelationship]()

  override protected val unreifiedRelationships =
    scala.collection.mutable.ListBuffer[ModelEntityUnreifiedRelationship]()

  override protected val sc =
    scala.collection.mutable.ListBuffer[ModelScalarDataType]()

  override protected val st =
    scala.collection.mutable.ListBuffer[ModelStructuredDataType]()

  override protected val e2sc =
    scala.collection.mutable.ListBuffer[ModelDataRelationshipFromEntityToScalar]()

  override protected val e2st =
    scala.collection.mutable.ListBuffer[ModelDataRelationshipFromEntityToStructure]()

  override protected val s2sc =
    scala.collection.mutable.ListBuffer[ModelDataRelationshipFromStructureToScalar]()

  override protected val s2st =
    scala.collection.mutable.ListBuffer[ModelDataRelationshipFromStructureToStructure]()

  override protected val ax =
    scala.collection.mutable.ListBuffer[ModelTermAxiom]()

  override def getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition] =
    (aspects.map (a => a.e -> a) ++
     concepts.map (c => c.e -> c) ++
     reifiedRelationships.map (r => r.e -> r)).toMap

  override def getScalarDatatypeDefinitionMap: Map[OWLDatatype, ModelScalarDataType] =
    sc
    .map (t => t.sc -> t)
    .toMap

  override protected val iri2typeTerm = scala.collection.mutable.HashMap[IRI, ModelTypeTerm]()


  def addTerminologyGraphExtension
  (extendedG: ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom =
    for {
      axiom <- store.createTerminologyGraphDirectExtensionAxiom(this, extendedG)
    } yield {
      for {
        change <- Seq(
                       new AddImport(ont,
                                     ontManager.getOWLDataFactory.getOWLImportsDeclaration(extendedG.iri))
                     )
      } {
        val result = ontManager.applyChange(change)
        require(
                 result == ChangeApplied.SUCCESSFULLY,
                 s"\naddTerminologyGraphExtension:\n$change")
      }
      axiom
    }

  def setTermShortName
  (term: types.ModelTypeTerm,
   shortName: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    for {
      c1 <-
      	getTermShortNameAnnotationAssertionAxiom(term)
        .fold[NonEmptyList[java.lang.Throwable] \/ Unit](
            \/-(())
        ){ annotationAssertionAxiom =>
          applyOntologyChange(
            ontManager,
            new RemoveAxiom(ont, annotationAssertionAxiom),
            ifError="Failed to remove a tbox term 'rdfs:label' annotation assertion axiom")
          }
      c2 <-
      	shortName
        .fold[NonEmptyList[java.lang.Throwable] \/ Unit](
          \/-(())
        ){ label =>
          applyOntologyChange(
            ontManager,
            new AddAxiom(
              ont,
              owlDataFactory
                .getOWLAnnotationAssertionAxiom(
                  omfStore.RDFS_LABEL,
                  term.iri,
                  owlDataFactory.getOWLLiteral(label))),
            ifError="Failed to add a tbox term 'rdfs:label' annotation assertion axiom",
            ifSuccess=(() => {
              omfStore.setTermShortName(this, term, label)
              if (LOG)
                System.out.println(s"setTermShortName: ${term.iri} name='$label'")
            }).some)
          }
    } yield ()

  def setTermUUID
  (term: types.ModelTypeTerm,
   uuid: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    for {
      c1 <-
        getTermUUIDAnnotationAssertionAxiom(term)
        .fold[NonEmptyList[java.lang.Throwable] \/ Unit](
          \/-(())
        ){ annotationAssertionAxiom =>
          applyOntologyChange(
            ontManager,
            new RemoveAxiom(ont, annotationAssertionAxiom),
            ifError="Failed to remove a tbox term 'uuid' annotation assertion axiom")
        }

      c2 <-
        uuid
        .fold[NonEmptyList[java.lang.Throwable] \/ Unit](
          \/-(())
        ){ id =>
          applyOntologyChange(
            ontManager,
            new AddAxiom(
              ont,
              owlDataFactory
                .getOWLAnnotationAssertionAxiom(omfStore.ANNOTATION_HAS_UUID,
                  term.iri,
                  owlDataFactory.getOWLLiteral(id))),
            ifError="Failed to add a tbox term 'uuid' annotation assertion axiom",
            ifSuccess=(() => {
              omfStore.setTermUUID(this, term, id)
              if (LOG)
                System.out.println(s"setTermUUID: ${term.iri} name='$id'")
            }).some)
        }

    } yield ()

  def createModelEntityAspect
  (a: OWLClass)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelEntityAspect =
    iri2typeTerm
      .get(a.getIRI)
      .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelEntityAspect]({
      val _a = types.ModelEntityAspect(a)
      aspects += _a
      iri2typeTerm += a.getIRI -> _a
      \/-(_a)
    }){
      case t: types.ModelEntityAspect =>
        NonEmptyList(
          entityAlreadyDefinedException(EntityAspect, a.getIRI, t)
        ).left
      case t                          =>
        NonEmptyList(
          entityConflictException(EntityAspect, a.getIRI, t)
        ).left
    }

  def addEntityAspect
  (aspectIRI: IRI)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelEntityAspect =
    iri2typeTerm
    .get(aspectIRI)
    .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelEntityAspect]({
      val aspectC = owlDataFactory.getOWLClass(aspectIRI)
      for {
        result <- createModelEntityAspect(aspectC)
      } yield {
        for {
          change <- Seq(
            new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(aspectC)),
            new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(aspectC, backbone.AspectC)))
        } {
          val result = ontManager.applyChange(change)
          require(
            result == ChangeApplied.SUCCESSFULLY,
            s"\naddEntityAspect:\n$change")
        }
        result
      }
    }){
      case t: types.ModelEntityAspect =>
        NonEmptyList(
          entityAlreadyDefinedException(EntityAspect, aspectIRI, t)
        ).left
      case t                          =>
        NonEmptyList(
          entityConflictException(EntityAspect, aspectIRI, t)
        ).left
    }

  def createModelEntityConcept
  (c: OWLClass,
   isAbstract: Boolean)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelEntityConcept =
    iri2typeTerm
      .get(c.getIRI)
      .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelEntityConcept]({
      val _c = types.ModelEntityConcept(c, isAbstract)
      concepts += _c
      iri2typeTerm += c.getIRI -> _c
      \/-(_c)
    }){
      case t: types.ModelEntityConcept =>
        NonEmptyList(
          entityAlreadyDefinedException(EntityConcept, c.getIRI, t)
        ).left
      case t                          =>
        NonEmptyList(
          entityConflictException(EntityConcept, c.getIRI, t)
        ).left
    }

  def addEntityConcept
  (conceptIRI: IRI,
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelEntityConcept =
    iri2typeTerm
    .get(conceptIRI)
    .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelEntityConcept]({
      val conceptC = owlDataFactory.getOWLClass(conceptIRI)
      for {
        result <- createModelEntityConcept(conceptC, isAbstract)
      } yield {
        for {
          change <-
          Seq(new AddAxiom(ont,
            owlDataFactory
              .getOWLDeclarationAxiom(conceptC)),
            new AddAxiom(ont,
              owlDataFactory
                .getOWLAnnotationAssertionAxiom(isAbstractAP,
                  conceptIRI,
                  owlDataFactory.getOWLLiteral(isAbstract))),
            new AddAxiom(ont,
              owlDataFactory
                .getOWLSubClassOfAxiom(conceptC, backbone.EntityC)))
        } {
          val result = ontManager.applyChange(change)
          require(
            result == ChangeApplied.SUCCESSFULLY,
            s"\naddEntityConcept:\n$change")
        }
        result
      }
    }){
      case t: types.ModelEntityConcept =>
        NonEmptyList(
          entityAlreadyDefinedException(EntityConcept, conceptIRI, t)
        ).left
      case t                           =>
        NonEmptyList(
          entityConflictException(EntityConcept, conceptIRI, t)
        ).left
    }

  def createEntityReifiedRelationship
  (r: OWLClass,
   u: OWLObjectProperty, ui: Option[OWLObjectProperty],
   source: ModelEntityDefinition, rSource: OWLObjectProperty,
   target: ModelEntityDefinition, rTarget: OWLObjectProperty,
   characteristics: Iterable[RelationshipCharacteristics],
   isAbstract: Boolean)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship =
    iri2typeTerm
      .get(r.getIRI)
      .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship]({
      val _r = types.ModelEntityReifiedRelationship(r,
        u, ui,
        source, rSource,
        target, rTarget,
        characteristics, isAbstract)
      reifiedRelationships += _r
      iri2typeTerm += r.getIRI -> _r
      \/-(_r)
    }){
      case t: types.ModelEntityReifiedRelationship =>
        NonEmptyList(
          entityAlreadyDefinedException(EntityReifiedRelationship, r.getIRI, t)
        ).left
      case t                                       =>
        NonEmptyList(
          entityConflictException(EntityReifiedRelationship, r.getIRI, t)
        ).left
    }

  protected def makeEntityReifiedRelationship
  (rIRI: IRI,
   rIRISource: IRI, rIRITarget: IRI,
   uIRI: IRI, uiIRI: Option[IRI],
   source: ModelEntityDefinition, target: ModelEntityDefinition,
   characteristics: Iterable[RelationshipCharacteristics],
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship = {

    val sourceC = owlDataFactory.getOWLClass(source.iri)
    val targetC = owlDataFactory.getOWLClass(target.iri)
    val r = owlDataFactory.getOWLClass(rIRI)
    val rSource = owlDataFactory.getOWLObjectProperty(rIRISource)
    val rTarget = owlDataFactory.getOWLObjectProperty(rIRITarget)
    val u = owlDataFactory.getOWLObjectProperty(uIRI)
    val ui = if (uiIRI.isEmpty) None else Some(owlDataFactory.getOWLObjectProperty(uiIRI.get))

    for {
      result <- createEntityReifiedRelationship(r, u, ui,
                                                source, rSource,
                                                target, rTarget,
                                                characteristics, isAbstract)
    } yield {

      val vr: SWRLVariable = owlDataFactory.getSWRLVariable(IRI.create("urn:swrl#r"))
      val vs: SWRLVariable = owlDataFactory.getSWRLVariable(IRI.create("urn:swrl#s"))
      val vt: SWRLVariable = owlDataFactory.getSWRLVariable(IRI.create("urn:swrl#t"))

      val body1: SWRLObjectPropertyAtom = owlDataFactory.getSWRLObjectPropertyAtom(rSource, vr, vs)
      val body2: SWRLObjectPropertyAtom = owlDataFactory.getSWRLObjectPropertyAtom(rTarget, vr, vt)

      val head: SWRLObjectPropertyAtom = owlDataFactory.getSWRLObjectPropertyAtom(u, vs, vt)

      val rule: SWRLRule = owlDataFactory.getSWRLRule(Set(body1, body2), Set(head))

      for {
        change <-
        Seq(new AddAxiom(ont,
                         owlDataFactory
                         .getOWLDeclarationAxiom(r)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLAnnotationAssertionAxiom(isAbstractAP,
                                                         rIRI,
                                                         owlDataFactory.getOWLLiteral(isAbstract))),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLSubClassOfAxiom(r, backbone.ReifiedObjectPropertyC)),

            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLDeclarationAxiom(rSource)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLSubObjectPropertyOfAxiom(rSource,
                                                         backbone.topReifiedObjectPropertySourceOP)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLObjectPropertyDomainAxiom(rSource, r)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLObjectPropertyRangeAxiom(rSource, sourceC)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLFunctionalObjectPropertyAxiom(rSource)),

            new AddAxiom(ont,
                         owlDataFactory.getOWLDeclarationAxiom(rTarget)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLSubObjectPropertyOfAxiom(rTarget,
                                                         backbone.topReifiedObjectPropertyTargetOP)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLObjectPropertyDomainAxiom(rTarget, r)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLObjectPropertyRangeAxiom(rTarget, targetC)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLFunctionalObjectPropertyAxiom(rTarget)),

            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLDeclarationAxiom(u)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLSubObjectPropertyOfAxiom(u,
                                                         backbone.topReifiedObjectPropertyOP)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLObjectPropertyDomainAxiom(u, sourceC)),
            new AddAxiom(ont,
                         owlDataFactory
                         .getOWLObjectPropertyRangeAxiom(u, targetC)),
            new AddAxiom(ont, rule)
           ) ++
        (if (ui.isDefined)
          Seq(
               new AddAxiom(ont,
                            owlDataFactory
                            .getOWLDeclarationAxiom(ui.get)),
               new AddAxiom(ont,
                            owlDataFactory
                            .getOWLAnnotationAssertionAxiom(isDerivedAP,
                                                            ui.get.getIRI,
                                                            owlDataFactory.getOWLLiteral(true))),
               new AddAxiom(ont,
                            owlDataFactory
                            .getOWLSubObjectPropertyOfAxiom(ui.get,
                                                            backbone.topReifiedObjectPropertyOP)),
               new AddAxiom(ont,
                            owlDataFactory
                            .getOWLObjectPropertyDomainAxiom(ui.get,
                                                             targetC)),
               new AddAxiom(ont,
                            owlDataFactory
                            .getOWLObjectPropertyRangeAxiom(ui.get,
                                                            sourceC))
             )
        else
          Seq())
      } {
        val result = ontManager.applyChange(change)
        require(
                 result == ChangeApplied.SUCCESSFULLY,
                 s"\nmakeEntityReifiedRelationship:\n$change")
      }

      result
    }

  }

  def addEntityReifiedRelationship
  (rIRI: IRI,
   rIRISource: IRI, rIRITarget: IRI,
   uIRI: IRI, uiIRI: Option[IRI],
   source: ModelEntityDefinition, target: ModelEntityDefinition,
   characteristics: Iterable[RelationshipCharacteristics],
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship =
    (lookupTypeTerm(rIRI, recursively = true),
      lookupTypeTerm(rIRISource, recursively = true),
      lookupTypeTerm(rIRITarget, recursively = true),
      lookupTypeTerm(uIRI, recursively = true),
      lookupTypeTerm(uiIRI, recursively = true)) match {
      case (None, None, None, None, None) =>
        (isTypeTermDefinedRecursively(source), isTypeTermDefinedRecursively(target)) match {
          case (true, true)  =>
            makeEntityReifiedRelationship(rIRI, rIRISource, rIRITarget, uIRI, uiIRI,
                                          source, target, characteristics, isAbstract)
          case (false, true) =>
            NonEmptyList(
              entityScopeException(EntityReifiedRelationship, rIRI,
                Map(RelationshipScopeAccessKind.Source -> source))
            ).left

          case (true, false) =>
            NonEmptyList(
              entityScopeException(EntityReifiedRelationship, rIRI,
                Map(RelationshipScopeAccessKind.Target -> target))
            ).left

          case (false, false) =>
            NonEmptyList(
              entityScopeException(EntityReifiedRelationship, rIRI,
                Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
            ).left
        }

      case (Some(t), _, _, _, _) =>
        NonEmptyList(
          entityConflictException(EntityReifiedRelationship, rIRI, t)
        ).left

      case (_, Some(t), _, _, _) =>
        NonEmptyList(
          entityConflictException(EntityReifiedRelationship, rIRISource, t)
        ).left

      case (_, _, Some(t), _, _) =>
        NonEmptyList(
          entityConflictException(EntityReifiedRelationship, rIRITarget, t)
        ).left

      case (_, _, _, Some(t), _) =>
        NonEmptyList(
          entityConflictException(EntityReifiedRelationship, uIRI, t)
        ).left

      case (_, _, _, _, Some(t)) =>
        require(uiIRI.isDefined)
        NonEmptyList(
          entityConflictException(EntityReifiedRelationship, uiIRI.get, t)
        ).left
    }

  def createModelScalarDataType
  (dt: OWLDatatype)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelScalarDataType =
    iri2typeTerm
      .get(dt.getIRI)
      .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelScalarDataType]({
      val _dt = types.ModelScalarDataType(dt)
      sc += _dt
      iri2typeTerm += dt.getIRI -> _dt
      \/-(_dt)
    }){
      case t: types.ModelScalarDataType =>
        NonEmptyList(
          entityAlreadyDefinedException(ScalarDataType, dt.getIRI, t)
        ).left
      case t                            =>
        NonEmptyList(
          entityConflictException(ScalarDataType, dt.getIRI, t)
        ).left
    }

  def addScalarDataType
  (scalarIRI: IRI)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelScalarDataType =
    iri2typeTerm
      .get(scalarIRI)
      .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelScalarDataType]({
      val scalarDT = owlDataFactory.getOWLDatatype(scalarIRI)
      for {
        result <- createModelScalarDataType(scalarDT)
      } yield {
        for {
          change <- Seq(
            new AddAxiom(ont,
              owlDataFactory
                .getOWLDeclarationAxiom(scalarDT))
          )
        } {
          val result = ontManager.applyChange(change)
          require(
            result == ChangeApplied.SUCCESSFULLY,
            s"\naddScalarDataType:\n$change")
        }
        result
      }
    }){
      case t: types.ModelScalarDataType =>
        NonEmptyList(
          entityAlreadyDefinedException(ScalarDataType, scalarIRI, t)
        ).left
      case t                            =>
        NonEmptyList(
          entityConflictException(ScalarDataType, scalarIRI, t)
        ).left
    }

  def createModelStructuredDataType
  (c: OWLClass)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelStructuredDataType =
    iri2typeTerm
      .get(c.getIRI)
      .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelStructuredDataType]({
      val _st = types.ModelStructuredDataType(c)
      st += _st
      iri2typeTerm += c.getIRI -> _st
      \/-(_st)
    }){
      case t: types.ModelStructuredDataType =>
        NonEmptyList(
          entityAlreadyDefinedException(StructuredDataType, c.getIRI, t)
        ).left
      case t                                =>
        NonEmptyList(
          entityConflictException(StructuredDataType, c.getIRI, t)
        ).left
    }

  def addStructuredDataType
  (structuredDataTypeIRI: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelStructuredDataType =

    iri2typeTerm
      .get(structuredDataTypeIRI)
      .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelStructuredDataType]({
      val structuredDataTypeC = owlDataFactory
        .getOWLClass(structuredDataTypeIRI)
      for {
        result <- createModelStructuredDataType(structuredDataTypeC)
      } yield {
        for {
          change <- Seq(
            new AddAxiom(ont,
              owlDataFactory
                .getOWLDeclarationAxiom(structuredDataTypeC)),
            new AddAxiom(ont,
              owlDataFactory
                .getOWLSubClassOfAxiom(structuredDataTypeC, backbone.StructuredDatatypeC))
          )
        } {
          val result = ontManager.applyChange(change)
          require(
            result == ChangeApplied.SUCCESSFULLY,
            s"\naddStructuredDataType:\n$change")
        }
        result
      }
    }){
      case t: types.ModelStructuredDataType =>
        NonEmptyList(
          entityAlreadyDefinedException(StructuredDataType, structuredDataTypeIRI, t)
        ).left
      case t                                =>
        NonEmptyList(
          entityConflictException(StructuredDataType, structuredDataTypeIRI, t)
        ).left
    }

  def createDataRelationshipFromEntityToScalar
  (esc: OWLDataProperty, source: ModelEntityDefinition, target: ModelScalarDataType)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar =
    iri2typeTerm
      .get(esc.getIRI)
      .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar]({
      val _esc = types.ModelDataRelationshipFromEntityToScalar(esc, source, target)
      e2sc += _esc
      iri2typeTerm += esc.getIRI -> _esc
      \/-(_esc)
    }){
      case t: types.ModelDataRelationshipFromEntityToScalar =>
        NonEmptyList(
          entityAlreadyDefinedException(EntityAspect, esc.getIRI, t)
        ).left
      case t                                                =>
        NonEmptyList(
          entityConflictException(EntityAspect, esc.getIRI, t)
        ).left
    }

  protected def makeDataRelationshipFromEntityToScalar
  (dIRI: IRI,
   source: types.ModelEntityDefinition,
   target: types.ModelScalarDataType)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar = {
    val escDP = owlDataFactory.getOWLDataProperty(dIRI)
    for {
      result <- createDataRelationshipFromEntityToScalar(escDP, source, target)
    } yield {
      for {
        change <- Seq(
                       new AddAxiom(ont,
                                    owlDataFactory
                                    .getOWLDeclarationAxiom(escDP)),
                       new AddAxiom(ont,
                                    owlDataFactory
                                    .getOWLSubDataPropertyOfAxiom(escDP, backbone.topDataPropertyDP)),
                       new AddAxiom(ont,
                                    owlDataFactory
                                    .getOWLDataPropertyDomainAxiom(escDP, source.e)),
                       new AddAxiom(ont,
                                    owlDataFactory
                                    .getOWLDataPropertyRangeAxiom(escDP, owlDataFactory.getOWLDatatype(target.iri)))
                     )
      } {
        val result = ontManager.applyChange(change)
        require(
                 result == ChangeApplied.SUCCESSFULLY,
                 s"\nmakeDataRelationshipFromEntityToScalar:\n$change")
      }
      result
    }
  }

  def addDataRelationshipFromEntityToScalar
  (dIRI: IRI,
   source: types.ModelEntityDefinition,
   target: types.ModelScalarDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar =
    iri2typeTerm
      .get(dIRI)
      .fold[NonEmptyList[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar]({
      (isTypeTermDefinedRecursively(source),
        isTypeTermDefinedRecursively(target)) match {
        case (true, true) =>
          makeDataRelationshipFromEntityToScalar(dIRI, source, target)
        case (false, true) =>
          NonEmptyList(
            entityScopeException(DataRelationshipFromEntityToScalar, dIRI,
              Map(RelationshipScopeAccessKind.Source -> source))
          ).left
        case (true, false) =>
          NonEmptyList(
            entityScopeException(DataRelationshipFromEntityToScalar, dIRI,
              Map(RelationshipScopeAccessKind.Target -> target))
          ).left
        case (false, false) =>
          NonEmptyList(
            entityScopeException(DataRelationshipFromEntityToScalar, dIRI,
              Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
          ).left
      }
    }) { term =>
      NonEmptyList(
        entityConflictException(DataRelationshipFromEntityToScalar, dIRI, term)
      ).left
    }

  def addDataRelationshipFromEntityToStructure
  (dIRI: IRI,
   source: types.ModelEntityDefinition,
   target: types.ModelStructuredDataType)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToStructure = ???

  def addDataRelationshipFromStructureToScalar
  (dIRI: IRI,
   source: types.ModelStructuredDataType,
   target: types.ModelScalarDataType)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelDataRelationshipFromStructureToScalar = ???

  def addDataRelationshipFromStructureToStructure
  (dIRI: IRI,
   source: types.ModelStructuredDataType,
   target: types.ModelStructuredDataType)
  : NonEmptyList[java.lang.Throwable] \/ types.ModelDataRelationshipFromStructureToStructure = ???

  def createEntityConceptSubClassAxiom
  (sub: types.ModelEntityConcept,
   sup: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityConceptSubClassAxiom =
    ax
    .find {
            case axiom: types.EntityConceptSubClassAxiom =>
              axiom.sub == sub && axiom.sup == sup
            case _                                       =>
              false
          }
    .fold[NonEmptyList[java.lang.Throwable] \/ types.EntityConceptSubClassAxiom](
        for {
          axiom <- store
                   .createOMFEntityConceptSubClassAxiomInstance(this,
                                                                EntityConceptSubClassAxiom(sub, sup))
        } yield {
          ax += axiom
          axiom
        }
    ){ other =>
      NonEmptyList(
        duplicateModelTermAxiomException(EntityConceptSubClassAxiomException, other)
      ).left
    }

  def addEntityConceptSubClassAxiom
  (sub: types.ModelEntityConcept,
   sup: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityConceptSubClassAxiom =
    ( isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(sup) ) match {
      case (true, true) =>
        for {
          axiom <- createEntityConceptSubClassAxiom(sub, sup)
        } yield {
          val subC = owlDataFactory.getOWLClass(sub.iri)
          val supC = owlDataFactory.getOWLClass(sup.iri)
          for {
            change <- Seq(
                           new AddAxiom(ont,
                                        owlDataFactory
                                        .getOWLSubClassOfAxiom(subC, supC))
                         )
          } {

            val result = ontManager.applyChange(change)
            require(
                     result == ChangeApplied.SUCCESSFULLY,
                     s"\naddEntityConceptSubClassAxiom:\n$change")
          }
          axiom
        }

      case (false, true) =>
        NonEmptyList(
          axiomScopeException(ConceptSubclassAxiomException, Map(Sub -> sub))
        ).left

      case (true, false) =>
        NonEmptyList(
          axiomScopeException(ConceptSubclassAxiomException, Map(Sup -> sup))
        ).left

      case (false, false) =>
        NonEmptyList(
          axiomScopeException(ConceptSubclassAxiomException, Map(Sub -> sub, Sup -> sup))
        ).left
    }

  def addEntityConceptUniversalRestrictionAxiom
  (sub: types.ModelEntityConcept,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityConceptUniversalRestrictionAxiom =
    ( isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(rel),
      isTypeTermDefinedRecursively(range) ) match {
      case (true, true, true) =>
        val subC = owlDataFactory.getOWLClass(sub.iri)
        val rangeC = owlDataFactory.getOWLClass(range.iri)
        for {
          axiom <-
          store
          .createOMFEntityConceptUniversalRestrictionAxiomInstance(this,
                                                                   EntityConceptUniversalRestrictionAxiom(sub,
                                                                                                          rel,
                                                                                                          range))
        } yield {
          for {
            change <-
            Seq(
                 new AddAxiom(ont,
                              owlDataFactory
                              .getOWLSubClassOfAxiom(subC,
                                                     owlDataFactory
                                                     .getOWLObjectAllValuesFrom(rel.unreified,
                                                                                rangeC)))
               )
          } {

            val result = ontManager.applyChange(change)
            require(
                     result == ChangeApplied.SUCCESSFULLY,
                     s"\naddEntityConceptUniversalRestrictionAxiom:\n$change")
          }
          ax += axiom
          axiom
        }

      case (false, _, _) =>
        NonEmptyList(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Sub -> sub))
        ).left

      case (_, false, _) =>
        NonEmptyList(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Rel -> rel))
        ).left

      case (_, _, false) =>
        NonEmptyList(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Range -> range))
        ).left

    }

  def addEntityConceptExistentialRestrictionAxiom
  (sub: types.ModelEntityConcept,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityConceptExistentialRestrictionAxiom =
    ( isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(rel),
      isTypeTermDefinedRecursively(range) ) match {
      case (true, true, true) =>
        val subC = owlDataFactory.getOWLClass(sub.iri)
        val rangeC = owlDataFactory.getOWLClass(range.iri)
        for {
          axiom <-
          store
          .createOMFEntityConceptExistentialRestrictionAxiomInstance(this,
                                                                     EntityConceptExistentialRestrictionAxiom(sub,
                                                                                                              rel,
                                                                                                              range))
        } yield {
          for {
            change <-
            Seq(new AddAxiom(ont,
                             owlDataFactory
                             .getOWLSubClassOfAxiom(subC,
                                                    owlDataFactory
                                                    .getOWLObjectSomeValuesFrom(rel.unreified,
                                                                                rangeC)))
               )
          } {

            val result = ontManager.applyChange(change)
            require(
                     result == ChangeApplied.SUCCESSFULLY,
                     s"\naddEntityConceptExistentialRestrictionAxiom:\n$change")
          }
          ax += axiom
          axiom
        }

      case (false, _, _) =>
        NonEmptyList(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Sub -> sub))
        ).left

      case (_, false, _) =>
        NonEmptyList(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Rel -> rel))
        ).left

      case (_, _, false) =>
        NonEmptyList(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Range -> range))
        ).left
    }

  def createEntityDefinitionAspectSubClassAxiom
  (sub: types.ModelEntityDefinition,
   sup: types.ModelEntityAspect)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityDefinitionAspectSubClassAxiom =
    ax
    .find {
            case axiom: types.EntityDefinitionAspectSubClassAxiom =>
              axiom.sub == sub && axiom.sup == sup
            case _                                                =>
              false
          }
    .fold[NonEmptyList[java.lang.Throwable] \/ types.EntityDefinitionAspectSubClassAxiom](
        for {
          axiom <-
          store
          .createOMFEntityDefinitionAspectSubClassAxiomInstance(this,
                                                                EntityDefinitionAspectSubClassAxiom(sub, sup))
        } yield {
          ax += axiom
          axiom
        }
    ){ other =>
      NonEmptyList(
        duplicateModelTermAxiomException(EntityDefinitionAspectSubClassAxiomException, other)
      ).left
    }

  def addEntityDefinitionAspectSubClassAxiom
  (sub: types.ModelEntityDefinition,
   sup: types.ModelEntityAspect)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityDefinitionAspectSubClassAxiom =
    ( isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(sup) ) match {
      case (true, true) =>
        for {
          axiom <- createEntityDefinitionAspectSubClassAxiom(sub, sup)
        } yield {
          for {
            change <-
            Seq(new AddAxiom(ont,
                             owlDataFactory
                             .getOWLSubClassOfAxiom(sub.e, sup.e))
               )
          } {

            val result = ontManager.applyChange(change)
            require(
                     result == ChangeApplied.SUCCESSFULLY,
                     s"\naddEntityDefinitionAspectSubClassAxiom:\n$change")
          }
          axiom
        }

      case (false, true) =>
        NonEmptyList(
          axiomScopeException(EntityDefinitionAspectSubClassAxiomException, Map(Sub -> sub))
        ).left

      case (true, false) =>
        NonEmptyList(
          axiomScopeException(EntityDefinitionAspectSubClassAxiomException, Map(Sup -> sub))
        ).left

      case (false, false) =>
        NonEmptyList(
          axiomScopeException(EntityDefinitionAspectSubClassAxiomException, Map(Sub -> sub, Sup -> sub))
        ).left
    }


  def createOMFEntityConceptDesignationTerminologyGraphAxiom
  (entityConceptDesignation: types.ModelEntityConcept,
   designationTerminologyGraph: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityConceptDesignationTerminologyGraphAxiom =
    ax
    .find {
            case axiom: types.EntityConceptDesignationTerminologyGraphAxiom =>
              axiom.entityConceptDesignation == entityConceptDesignation &&
              axiom.designationTerminologyGraph == designationTerminologyGraph
            case _                                                          =>
              false
          }
    .fold[NonEmptyList[java.lang.Throwable] \/ types.EntityConceptDesignationTerminologyGraphAxiom]({
      val axInstance = EntityConceptDesignationTerminologyGraphAxiom(entityConceptDesignation,
        designationTerminologyGraph)
      for {
        axiom <- store.createOMFEntityConceptDesignationTerminologyGraphAxiomInstance(this, axInstance)
      } yield {
        ax += axiom
        axiom
      }
    }){ other =>
      NonEmptyList(
        duplicateModelTermAxiomException(EntityConceptDesignationTerminologyGraphAxiomException, other)
      ).left
    }

  def addEntityConceptDesignationTerminologyGraphAxiom
  (entityConceptDesignation: types.ModelEntityConcept,
   designationTerminologyGraph: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityConceptDesignationTerminologyGraphAxiom =
    for {
      axiom <- createOMFEntityConceptDesignationTerminologyGraphAxiom(entityConceptDesignation,
                                                                      designationTerminologyGraph)
    } yield {
      ax += axiom
      axiom
    }

  def createEntityReifiedRelationshipSubClassAxiom
  (sub: types.ModelEntityReifiedRelationship,
   sup: types.ModelEntityReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityReifiedRelationshipSubClassAxiom =
    ax
    .find {
            case axiom: types.EntityReifiedRelationshipSubClassAxiom =>
              axiom.sub == sub && axiom.sup == sup
            case _                                                   =>
              false
          }
    .fold[NonEmptyList[java.lang.Throwable] \/ types.EntityReifiedRelationshipSubClassAxiom]({
      val axInstance = EntityReifiedRelationshipSubClassAxiom(sub, sup)
      for {
        axiom <- store
          .createOMFEntityReifiedRelationshipSubClassAxiomInstance(this, axInstance)
      } yield {
        ax += axiom
        axiom
      }
    }){ other =>
      NonEmptyList(
        duplicateModelTermAxiomException(EntityReifiedRelationshipSubClassAxiomException, other)
      ).left
    }

  def addEntityReifiedRelationshipSubClassAxiom
  (sub: types.ModelEntityReifiedRelationship,
   sup: types.ModelEntityReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.EntityReifiedRelationshipSubClassAxiom =
    (isTypeTermDefinedRecursively(sub), isTypeTermDefinedRecursively(sup)) match {
      case (true, true) =>
        for {
          axiom <- createEntityReifiedRelationshipSubClassAxiom(sub, sup)
        } yield {
          for {
            change <- Seq(new AddAxiom(ont,
                                       owlDataFactory
                                       .getOWLSubClassOfAxiom(sub.e, sup.e)),
                          new AddAxiom(ont,
                                       owlDataFactory
                                       .getOWLSubObjectPropertyOfAxiom(sub.rSource, sup.rSource)),
                          new AddAxiom(ont,
                                       owlDataFactory
                                       .getOWLSubObjectPropertyOfAxiom(sub.rTarget, sup.rTarget))
                         )
          } {
            val result = ontManager.applyChange(change)
            require(
                     result == ChangeApplied.SUCCESSFULLY,
                     s"\naddEntityReifiedRelationshipSubClassAxiom:\n$change")
          }
          axiom
        }

      case (false, true) =>
        NonEmptyList(
          axiomScopeException(ReifiedRelationshipSubclassAxiomException, Map(Sub -> sub))
        ).left

      case (true, false) =>
        NonEmptyList(
          axiomScopeException(ReifiedRelationshipSubclassAxiomException, Map(Sup -> sup))
        ).left

      case (false, false) =>
        NonEmptyList(
          axiomScopeException(ReifiedRelationshipSubclassAxiomException, Map(Sub -> sub, Sup -> sup))
        ).left
    }

  def createScalarDataTypeFacetRestrictionAxiom
  (sub: types.ModelScalarDataType,
   sup: types.ModelScalarDataType,
   restrictions: Iterable[ConstrainingFacet])
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.ScalarDataTypeFacetRestrictionAxiom =
    ax
    .find {
            case axiom: types.ScalarDataTypeFacetRestrictionAxiom =>
              axiom.sub == sub &&
              axiom.sup == sup &&
              axiom.restrictions.toSet.diff(restrictions.toSet).isEmpty
            case _                                                =>
              false
          }
    .fold[NonEmptyList[java.lang.Throwable] \/ types.ScalarDataTypeFacetRestrictionAxiom](
        for {
          axiom <-
          store
          .createOMFScalarDataTypeFacetRestrictionAxiomInstance(this,
                                                                ScalarDataTypeFacetRestrictionAxiom(sub,
                                                                                                    sup,
                                                                                                    restrictions))
        } yield {
          ax += axiom
          axiom
        }
    ){ other =>
      NonEmptyList(
        duplicateModelTermAxiomException(ScalarDataTypeFacetRestrictionAxiomException, other)
      ).left
    }

  def addScalarDataTypeFacetRestrictionAxiom
  (sub: types.ModelScalarDataType,
   sup: types.ModelScalarDataType,
   restrictions: Iterable[ConstrainingFacet])
  (implicit store: OWLAPIOMFGraphStore)
  : NonEmptyList[java.lang.Throwable] \/ types.ScalarDataTypeFacetRestrictionAxiom =
    ( isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(sup) ) match {
      case (true, true) =>
        for {
          axiom <- createScalarDataTypeFacetRestrictionAxiom(sub, sup, restrictions)
        } yield {
//          Try(for {
//                restriction <- restrictions
//                change <- new AddAxiom(ont,
//                  owlDataFactory
//                  .getOWLDatatypeRestriction(sub.sc, restriction.))
//            )
//          } {
//
//            val result = ontManager.applyChange(change)
//            require(
//                     result == ChangeApplied.SUCCESSFULLY,
//                     s"\naddScalarDataTypeFacetRestrictionAxiom:\n$change")
//          })
//          .flatMap { _ => axiom }
          ???
        }

      case (false, true) =>
        NonEmptyList(
          axiomScopeException(ScalarDataTypeFacetRestrictionAxiomException, Map(Sub -> sub))
        ).left

      case (true, false) =>
        NonEmptyList(
          axiomScopeException(ScalarDataTypeFacetRestrictionAxiomException, Map(Sup -> sub))
        ).left

      case (false, false) =>
        NonEmptyList(
          axiomScopeException(ScalarDataTypeFacetRestrictionAxiomException, Map(Sub -> sub, Sup -> sub))
        ).left
    }


}
