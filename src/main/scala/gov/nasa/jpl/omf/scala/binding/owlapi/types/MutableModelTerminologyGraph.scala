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

package gov.nasa.jpl.omf.scala.binding.owlapi.types

import java.lang.System
import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.ChangeApplied

import scala.collection.JavaConversions._
import scala.collection.immutable._
import scala.{Any, Boolean, Enumeration, Int, None, Option, Some, StringContext, Unit}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._

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
  ReifiedRelationshipContextualizationAxiomException,
  ReifiedRelationshipRestrictionAxiomException,
  EntityDefinitionAspectSubClassAxiomException,
  EntityConceptSubClassAxiomException,
  EntityReifiedRelationshipSubClassAxiomException,
  EntityConceptDesignationTerminologyGraphAxiomException,
  ScalarDataTypeFacetRestrictionAxiomException,
  ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteralException,
  TerminologyGraphDirectExtensionAxiomException,
  TerminologyGraphDirectNestingAxiomException = Value
}

import gov.nasa.jpl.omf.scala.binding.owlapi.types.AxiomExceptionKind._

object AxiomScopeAccessKind extends Enumeration {
  type AxiomScopeAccessKind = Value
  val Sub, Sup, Domain, Rel, Range = Value
}

import gov.nasa.jpl.omf.scala.binding.owlapi.types.AxiomScopeAccessKind._

sealed abstract class MutableModelTerminologyGraphException
(override val message: String)
  extends OMFError.OMFException(message, OMFError.emptyThrowables) {
  require(null != message)

  def this(kind: EntityExceptionKind, iri: IRI, message: String)
  = this(s"Cannot create $kind with IRI=$iri because "+message)

  def this(kind: AxiomExceptionKind, message: String)
  = this(s"Cannot create $kind because "+message)
}

case class EntityAlreadyDefinedException
(kind: EntityExceptionKind,
 iri: IRI,
 term: ModelTypeTerm)
  extends MutableModelTerminologyGraphException(kind, iri, s"it is already defined as: $term") {

  require(null != kind)
  require(null != iri)
  require(null != term)
}

case class EntityConflictException
(kind: EntityExceptionKind,
 iri: IRI,
 conflictingTerm: ModelTypeTerm)
  extends MutableModelTerminologyGraphException(kind, iri, s"this IRI refers to: $conflictingTerm") {

  require(null != kind)
  require(null != iri)
  require(null != conflictingTerm)
}

case class EntityScopeException
(kind: EntityExceptionKind,
 iri: IRI,
 unaccessibleTerms: Map[RelationshipScopeAccessKind, ModelTypeTerm])
  extends MutableModelTerminologyGraphException(kind, iri,
    s"""|there are ${unaccessibleTerms.size} terms out of scope of
        |the graph: """.stripMargin +
      (unaccessibleTerms.map { case (k, t) => s"$k: $t" } mkString ", "))

case class AxiomScopeException
(kind: AxiomExceptionKind,
 unaccessibleTerms: Map[AxiomScopeAccessKind, ModelTypeTerm])
  extends MutableModelTerminologyGraphException(kind,
    s"""|there are ${unaccessibleTerms.size} terms out of scope of
        |the graph: """.stripMargin +
      (unaccessibleTerms.map { case (k, t) => s"$k: $t" } mkString ", "))

case class DuplicateModelTermAxiomException
(kind: AxiomExceptionKind,
 axiom: ModelTermAxiom)
  extends MutableModelTerminologyGraphException(kind, s"the axiom is already asserted $axiom")

case class DuplicateTerminologyGraphAxiomException
(kind: AxiomExceptionKind,
 axiom: TerminologyGraphAxiom)
  extends MutableModelTerminologyGraphException(kind, s"the axiom is already asserted $axiom")

case class MutableModelTerminologyGraph
(override val uuid: UUID,
 override val name: LocalName,
 override val kind: TerminologyKind,
 override val ont: OWLOntology,
 override val extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance],
 backbone: OMFBackbone)
(override implicit val ops: OWLAPIOMFOps)
  extends ModelTerminologyGraph(uuid, name, kind, ont, extraProvenanceMetadata)(ops) {

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: MutableModelTerminologyGraph => true
    case _ => false
  }

  override val hashCode: Int = (uuid, name, kind, extraProvenanceMetadata, ont).##

  override def equals(other: Any): Boolean = other match {
    case that: MutableModelTerminologyGraph =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.kind == that.kind) &&
        (this.extraProvenanceMetadata == that.extraProvenanceMetadata) &&
        (this.ont == that.ont) &&
        (this.aspects == that.aspects) &&
        (this.concepts == that.concepts) &&
        (this.reifiedRelationships == that.reifiedRelationships) &&
        (this.unreifiedRelationships == that.unreifiedRelationships) &&
        (this.sc == that.sc) &&
        (this.st == that.st) &&
        (this.e2sc == that.e2sc) &&
        (this.e2st == that.e2st) &&
        (this.s2sc == that.s2sc) &&
        (this.s2st == that.s2st) &&
        (this.ax == that.ax) &&
        (this.gx == that.gx)
    case _ =>
      false
  }

  override val mutabilityKind: String = "mutable"
  override val isImmutableModelTerminologyGraph = false
  override val isMutableModelTerminologyGraph = true

  override val kindIRI: IRI = makeKindIRI("mutable")

  val LOG: Boolean = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.MutableModelTerminologyGraph")

  def setTerminologyGraphLocalName
  (shortName: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit =
    for {
      c1 <-
      getTerminologyGraphShortNameAnnotation
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { annotation =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new RemoveOntologyAnnotation(ont, annotation),
          ifError = "Failed to remove the tbox ontology 'rdfs:label' annotation")
      }
      c2 <-
      shortName
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { label =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddOntologyAnnotation(
            ont,
            owlDataFactory.getOWLAnnotation(omfStore.RDFS_LABEL, owlDataFactory.getOWLLiteral(label))),
          ifError = "Failed to add the tbox ontology 'rdfs:label' annotation",
          ifSuccess = (() => {
            omfStore.setTerminologyGraphShortName(this, label)
            if (LOG)
              System.out.println(s"setTerminologyGraphShortName: $kindIRI name='$label'")
          }).some)
      }
    } yield ()

  def setTerminologyGraphUUID
  (uuid: UUID)
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit
  = for {
      c1 <-
      getTerminologyGraphUUIDAnnotation
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { annotation =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new RemoveOntologyAnnotation(ont, annotation),
          ifError = "Failed to remove the tbox ontology 'uuid' annotation")
      }
      c2 <-
      applyOntologyChangeOrNoOp(
          ontManager,
          new AddOntologyAnnotation(
            ont,
            createOMFProvenanceAnnotation(uuid)),
          ifError = "Failed to add the tbox ontology 'uuid' annotation",
          ifSuccess = (() => {
            omfStore.setTerminologyGraphUUID(this, uuid)
            if (LOG)
              System.out.println(s"setTerminologyGraphUUID: $kindIRI uuid='$uuid'")
          }).some)
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

  override protected val gx =
    scala.collection.mutable.ListBuffer[TerminologyGraphAxiom]()

  override def getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition] =
    (aspects.map(a => a.e -> a) ++
      concepts.map(c => c.e -> c) ++
      reifiedRelationships.map(r => r.e -> r)).toMap

  override def getScalarDatatypeDefinitionMap: Map[OWLDatatype, ModelScalarDataType] =
    sc
      .map(t => t.sc -> t)
      .toMap

  override protected val iri2typeTerm = scala.collection.mutable.HashMap[IRI, ModelTypeTerm]()

  def createOMFProvenanceAnnotation
  (uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OWLAnnotation
  = owlDataFactory.getOWLAnnotation(
      store.ANNOTATION_HAS_UUID,
      owlDataFactory.getOWLLiteral(uuid.toString)
    )

  def createTerminologyGraphDirectExtensionAxiom
  (extendedG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom
  = for {
    uuid <- ops.terminologyGraphExtensionUUID(this, extendedG)
    ax <- createTerminologyGraphDirectExtensionAxiom(uuid, extendedG)
  } yield ax

  def createTerminologyGraphDirectExtensionAxiom
  (uuid: UUID,
   extendedG: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom
  = gx
    .find {
      case axiom: types.TerminologyGraphDirectExtensionAxiom =>
        axiom.extendedParent == extendedG
      case _ =>
        false
    }
    .fold[Set[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom](
    for {
      axiom <- store
        .createOMFTerminologyGraphDirectExtensionAxiom(uuid, this, extendedG)
    } yield {
      gx += axiom
      axiom
    }
  ) { other =>
    Set(
      duplicateTerminologyGraphAxiomException(TerminologyGraphDirectExtensionAxiomException, other)
    ).left
  }

  def addTerminologyGraphExtension
  (uuid: UUID,
   extendedG: ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectExtensionAxiom
  = for {
    axiom <- createTerminologyGraphDirectExtensionAxiom(uuid, extendedG)
    _ <- applyOntologyChangeOrNoOp(ontManager,
      new AddImport(ont, owlDataFactory
        .getOWLImportsDeclaration(extendedG.iri)),
      "addTerminologyGraphExtension error")
  } yield axiom

  def createTerminologyGraphDirectNestingAxiom
  (parentC: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectNestingAxiom
  = for {
    uuid <- ops.terminologyNestingAxiomUUID(parentC, this)
    ax <- createTerminologyGraphDirectNestingAxiom(uuid, parentC)
  } yield ax

  def createTerminologyGraphDirectNestingAxiom
  (uuid: UUID,
   parentC: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectNestingAxiom
  = gx
    .find {
      case axiom: types.TerminologyGraphDirectNestingAxiom =>
        axiom.nestingContext == parentC
      case _ =>
        false
    }
    .fold[Set[java.lang.Throwable] \/ types.TerminologyGraphDirectNestingAxiom](
    for {
      axiom <- store
        .createOMFTerminologyGraphDirectNestingAxiom(uuid, parentC, this)
    } yield {
      gx += axiom
      axiom
    }
  ) { other =>
    Set(
      duplicateTerminologyGraphAxiomException(TerminologyGraphDirectNestingAxiomException, other)
    ).left
  }

  def addNestedTerminologyGraph
  (uuid: UUID,
   parentContext: ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.TerminologyGraphDirectNestingAxiom
  = for {
    axiom <- createTerminologyGraphDirectNestingAxiom(uuid, parentContext)
    _ <- applyOntologyChangesOrNoOp(ontManager,
      Seq(
        new AddOntologyAnnotation(ont, owlDataFactory
          .getOWLAnnotation(
            store.ANNOTATION_HAS_CONTEXT,
            parentContext.iri,
            java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid))))
      ),
      "addNestedTerminologyGraph error")
  } yield axiom

  def setTermShortName
  (term: types.ModelTypeTerm,
   shortName: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit
  = for {
      c1 <-
      getTermLocalNameAnnotationAssertionAxiom(term)
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { annotationAssertionAxiom =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new RemoveAxiom(ont, annotationAssertionAxiom),
          ifError = "Failed to remove a tbox term 'rdfs:label' annotation assertion axiom")
      }
      c2 <-
      shortName
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { label =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(
            ont,
            owlDataFactory
              .getOWLAnnotationAssertionAxiom(
                omfStore.RDFS_LABEL,
                term.iri,
                owlDataFactory.getOWLLiteral(label))),
          ifError = "Failed to add a tbox term 'rdfs:label' annotation assertion axiom",
          ifSuccess = (() => {
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
  : Set[java.lang.Throwable] \/ Unit =
    for {
      c1 <-
      getTermUUIDAnnotationAssertionAxiom(term)
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { annotationAssertionAxiom =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new RemoveAxiom(ont, annotationAssertionAxiom),
          ifError = "Failed to remove a tbox term 'uuid' annotation assertion axiom")
      }

      c2 <-
      uuid
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { _uuid =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(
            ont,
            owlDataFactory
              .getOWLAnnotationAssertionAxiom(omfStore.ANNOTATION_HAS_UUID,
                term.iri,
                owlDataFactory.getOWLLiteral(_uuid))),
          ifError = "Failed to add a tbox term 'uuid' annotation assertion axiom",
          ifSuccess = (() => {
            omfStore.setTermUUID(this, term, _uuid)
            if (LOG)
              System.out.println(s"setTermUUID: ${term.iri} name='${_uuid}'")
          }).some)
      }

    } yield ()

  def setTermID
  (term: types.ModelTypeTerm,
   id: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit
  = for {
      c1 <-
      getTermIDAnnotationAssertionAxiom(term)
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { annotationAssertionAxiom =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new RemoveAxiom(ont, annotationAssertionAxiom),
          ifError = "Failed to remove a tbox term 'id' annotation assertion axiom")
      }

      c2 <-
      id
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { _id =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(
            ont,
            owlDataFactory
              .getOWLAnnotationAssertionAxiom(omfStore.ANNOTATION_HAS_ID,
                term.iri,
                owlDataFactory.getOWLLiteral(_id))),
          ifError = {
            System.out.println(s"@@setTermID(F): ${term.iri} name='${_id}'")
            "Failed to add a tbox term 'id' annotation assertion axiom"
          },
          ifSuccess = (() => {
            omfStore.setTermID(this, term, _id)
            //if (LOG)
              System.out.println(s"@@setTermID(S): ${term.iri} name='${_id}'")
          }).some)
      }

    } yield ()

  def setTermURL
  (term: types.ModelTypeTerm,
   url: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Unit =
    for {
      c1 <-
      getTermURLAnnotationAssertionAxiom(term)
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { annotationAssertionAxiom =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new RemoveAxiom(ont, annotationAssertionAxiom),
          ifError = "Failed to remove a tbox term 'url' annotation assertion axiom")
      }

      c2 <-
      url
        .fold[Set[java.lang.Throwable] \/ Unit](
        \/-(())
      ) { _url =>
        applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(
            ont,
            owlDataFactory
              .getOWLAnnotationAssertionAxiom(omfStore.ANNOTATION_HAS_URL,
                term.iri,
                owlDataFactory.getOWLLiteral(_url))),
          ifError = "Failed to add a tbox term 'url' annotation assertion axiom",
          ifSuccess = (() => {
            omfStore.setTermURL(this, term, _url)
            if (LOG)
              System.out.println(s"setTermURL: ${term.iri} name='${_url}'")
          }).some)
      }

    } yield ()

  def createModelEntityAspect
  (a: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityAspect
  = for {
    n <- getTermLocalNameFromAssertionOrFromIRI(ont, a.getIRI)
    u <- getTermUUIDFromAssertionOrFromIRI(ont, a.getIRI)
    term <- createModelEntityAspect(a, n, u)
  } yield term

  def createModelEntityAspect
  (a: OWLClass, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityAspect
  = iri2typeTerm
      .get(a.getIRI)
      .fold[Set[java.lang.Throwable] \/ types.ModelEntityAspect] {
      val _a = types.ModelEntityAspect(a, name, uuid)
      aspects += _a
      iri2typeTerm += a.getIRI -> _a
      \/-(_a)
    } {
      case t: types.ModelEntityAspect =>
        Set(
          entityAlreadyDefinedException(EntityAspect, a.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityAspect, a.getIRI, t)
        ).left
    }

  def addEntityAspect
  (aspectIRI: IRI, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityAspect
  = iri2typeTerm
      .get(aspectIRI)
      .fold[Set[java.lang.Throwable] \/ types.ModelEntityAspect]{
      val aspectC = owlDataFactory.getOWLClass(aspectIRI)
      for {
        result <- createModelEntityAspect(aspectC, name, uuid)
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(aspectC)),
            new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(aspectC, backbone.AspectC)),
            new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
                store.RDFS_LABEL, aspectIRI, owlDataFactory.getOWLLiteral(name))),
            new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
                store.ANNOTATION_HAS_UUID, aspectIRI, owlDataFactory.getOWLLiteral(uuid.toString)))),
          "addAspect Error")
      } yield result
    } {
      case t: types.ModelEntityAspect =>
        Set(
          entityAlreadyDefinedException(EntityAspect, aspectIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityAspect, aspectIRI, t)
        ).left
    }

  def createModelEntityConcept
  (c: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityConcept
  = for {
    n <- getTermLocalNameFromAssertionOrFromIRI(ont, c.getIRI)
    u <- getTermUUIDFromAssertionOrFromIRI(ont, c.getIRI)
    a <- isAnnotatedAbstract(ont, c.getIRI)
    term <- createModelEntityConcept(c, n, u, a)
  } yield term

  def createModelEntityConcept
  (c: OWLClass, name: LocalName, uuid: UUID,
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityConcept
  = iri2typeTerm
      .get(c.getIRI)
      .fold[Set[java.lang.Throwable] \/ types.ModelEntityConcept]{
      val _c = types.ModelEntityConcept(c, name, uuid, isAbstract)
      concepts += _c
      iri2typeTerm += c.getIRI -> _c
      \/-(_c)
    } {
      case t: types.ModelEntityConcept =>
        Set(
          entityAlreadyDefinedException(EntityConcept, c.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityConcept, c.getIRI, t)
        ).left
    }

  def addEntityConcept
  (conceptIRI: IRI, name: LocalName, uuid: UUID,
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityConcept
  = iri2typeTerm
      .get(conceptIRI)
      .fold[Set[java.lang.Throwable] \/ types.ModelEntityConcept] {
      val conceptC = owlDataFactory.getOWLClass(conceptIRI)
      for {
        result <- createModelEntityConcept(conceptC, name, uuid, isAbstract)
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(ont,
            owlDataFactory
              .getOWLDeclarationAxiom(conceptC)),
            new AddAxiom(ont,
              owlDataFactory
                .getOWLAnnotationAssertionAxiom(isAbstractAP,
                  conceptIRI,
                  owlDataFactory.getOWLLiteral(isAbstract))),
            new AddAxiom(ont,
              owlDataFactory
                .getOWLSubClassOfAxiom(conceptC, backbone.EntityC)),
            new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
              store.RDFS_LABEL, conceptIRI, owlDataFactory.getOWLLiteral(name))),
            new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
              store.ANNOTATION_HAS_UUID, conceptIRI, owlDataFactory.getOWLLiteral(uuid.toString)))),
          "addConcept Error")
      } yield result
    } {
      case t: types.ModelEntityConcept =>
        Set(
          entityAlreadyDefinedException(EntityConcept, conceptIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityConcept, conceptIRI, t)
        ).left
    }

  def createEntityReifiedRelationship
  (r: OWLClass,
   u: OWLObjectProperty, ui: Option[OWLObjectProperty],
   source: ModelEntityDefinition, rSource: OWLObjectProperty,
   target: ModelEntityDefinition, rTarget: OWLObjectProperty,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship
  = for {
    rn <- getTermLocalNameFromAssertionOrFromIRI(ont, r.getIRI)
    ru <- getTermUUIDFromAssertionOrFromIRI(ont, r.getIRI)
    ra <- isAnnotatedAbstract(ont, r.getIRI)
    term <- createEntityReifiedRelationship(
      r, rn, ru, u, ui, source, rSource, target, rTarget, characteristics, ra)
  } yield term

  def createEntityReifiedRelationship
  (r: OWLClass, name: LocalName, uuid: UUID,
   u: OWLObjectProperty, ui: Option[OWLObjectProperty],
   source: ModelEntityDefinition, rSource: OWLObjectProperty,
   target: ModelEntityDefinition, rTarget: OWLObjectProperty,
   characteristics: Iterable[RelationshipCharacteristics],
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship
  = iri2typeTerm
    .get(r.getIRI)
    .fold[Set[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship] {
    val _r = types.ModelEntityReifiedRelationship(r, name, uuid, u, ui,
      source, rSource, target, rTarget, characteristics, isAbstract)
    reifiedRelationships += _r
    iri2typeTerm += r.getIRI -> _r
    \/-(_r)
  } {
    case t: types.ModelEntityReifiedRelationship =>
      Set(
        entityAlreadyDefinedException(EntityReifiedRelationship, r.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityReifiedRelationship, r.getIRI, t)
      ).left
  }

  protected def makeEntityReifiedRelationship
  (rIRI: IRI, name: LocalName, uuid: UUID,
   rIRISource: IRI, rIRITarget: IRI,
   uIRI: IRI, uiIRI: Option[IRI],
   source: ModelEntityDefinition, target: ModelEntityDefinition,
   characteristics: Iterable[RelationshipCharacteristics],
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship
  = {
    val sourceC = owlDataFactory.getOWLClass(source.iri)
    val targetC = owlDataFactory.getOWLClass(target.iri)
    val r = owlDataFactory.getOWLClass(rIRI)
    val rSource = owlDataFactory.getOWLObjectProperty(rIRISource)
    val rTarget = owlDataFactory.getOWLObjectProperty(rIRITarget)
    val u = owlDataFactory.getOWLObjectProperty(uIRI)
    val ui = if (uiIRI.isEmpty) None else Some(owlDataFactory.getOWLObjectProperty(uiIRI.get))

    for {
      result <- createEntityReifiedRelationship(r, name, uuid, u, ui,
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
          new AddAxiom(ont, rule),
          new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
            store.RDFS_LABEL, rIRI, owlDataFactory.getOWLLiteral(name))),
          new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
            store.ANNOTATION_HAS_UUID, rIRI, owlDataFactory.getOWLLiteral(uuid.toString)))
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
          result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
          s"\nmakeEntityReifiedRelationship (result=$result):\n$change")
      }

      result
    }

  }

  def addEntityReifiedRelationship
  (rIRI: IRI, name: LocalName, uuid: UUID,
   rIRISource: IRI, rIRITarget: IRI,
   uIRI: IRI, uiIRI: Option[IRI],
   source: ModelEntityDefinition, target: ModelEntityDefinition,
   characteristics: Iterable[RelationshipCharacteristics],
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelEntityReifiedRelationship
  = ( lookupTypeTerm(rIRI, recursively = true),
      lookupTypeTerm(rIRISource, recursively = true),
      lookupTypeTerm(rIRITarget, recursively = true),
      lookupTypeTerm(uIRI, recursively = true),
      lookupTypeTerm(uiIRI, recursively = true) ) match {
      case (None, None, None, None, None) =>
        (isTypeTermDefinedRecursively(source), isTypeTermDefinedRecursively(target)) match {
          case (true, true) =>
            makeEntityReifiedRelationship(rIRI, name, uuid, rIRISource, rIRITarget, uIRI, uiIRI,
              source, target, characteristics, isAbstract)
          case (false, true) =>
            Set(
              entityScopeException(EntityReifiedRelationship, rIRI,
                Map(RelationshipScopeAccessKind.Source -> source))
            ).left

          case (true, false) =>
            Set(
              entityScopeException(EntityReifiedRelationship, rIRI,
                Map(RelationshipScopeAccessKind.Target -> target))
            ).left

          case (false, false) =>
            Set(
              entityScopeException(EntityReifiedRelationship, rIRI,
                Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
            ).left
        }

      case (Some(t), _, _, _, _) =>
        Set(
          entityConflictException(EntityReifiedRelationship, rIRI, t)
        ).left

      case (_, Some(t), _, _, _) =>
        Set(
          entityConflictException(EntityReifiedRelationship, rIRISource, t)
        ).left

      case (_, _, Some(t), _, _) =>
        Set(
          entityConflictException(EntityReifiedRelationship, rIRITarget, t)
        ).left

      case (_, _, _, Some(t), _) =>
        Set(
          entityConflictException(EntityReifiedRelationship, uIRI, t)
        ).left

      case (_, _, _, _, Some(t)) =>
        require(uiIRI.isDefined)
        Set(
          entityConflictException(EntityReifiedRelationship, uiIRI.get, t)
        ).left
    }

  def createModelScalarDataType
  (dt: OWLDatatype)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelScalarDataType
  = for {
    n <- getTermLocalNameFromAssertionOrFromIRI(ont, dt.getIRI)
    u <- getTermUUIDFromAssertionOrFromIRI(ont, dt.getIRI)
    term <- createModelScalarDataType(dt, n, u)
  } yield term

  def createModelScalarDataType
  (dt: OWLDatatype, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelScalarDataType
  = iri2typeTerm
      .get(dt.getIRI)
      .fold[Set[java.lang.Throwable] \/ types.ModelScalarDataType]{
      val _dt = types.ModelScalarDataType(dt, name, uuid)
      sc += _dt
      iri2typeTerm += dt.getIRI -> _dt
      \/-(_dt)
    } {
      case t: types.ModelScalarDataType =>
        Set(
          entityAlreadyDefinedException(ScalarDataType, dt.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ScalarDataType, dt.getIRI, t)
        ).left
    }

  def addScalarDataType
  (scalarIRI: IRI, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelScalarDataType
  = iri2typeTerm
      .get(scalarIRI)
      .fold[Set[java.lang.Throwable] \/ types.ModelScalarDataType] {
      val scalarDT = owlDataFactory.getOWLDatatype(scalarIRI)
      for {
        result <- createModelScalarDataType(scalarDT, name, uuid)
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(ont,
              owlDataFactory
                .getOWLDeclarationAxiom(scalarDT)),
            new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
              store.RDFS_LABEL, scalarIRI, owlDataFactory.getOWLLiteral(name))),
            new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
              store.ANNOTATION_HAS_UUID, scalarIRI, owlDataFactory.getOWLLiteral(uuid.toString)))
          ),
          "addScalarDataType error")
      } yield result
    } {
      case t: types.ModelScalarDataType =>
        Set(
          entityAlreadyDefinedException(ScalarDataType, scalarIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ScalarDataType, scalarIRI, t)
        ).left
    }

  def createModelStructuredDataType
  (dt: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelStructuredDataType
  = for {
    n <- getTermLocalNameFromAssertionOrFromIRI(ont, dt.getIRI)
    u <- getTermUUIDFromAssertionOrFromIRI(ont, dt.getIRI)
    term <- createModelStructuredDataType(dt, n, u)
  } yield term

  def createModelStructuredDataType
  (dt: OWLClass, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelStructuredDataType
  = iri2typeTerm
      .get(dt.getIRI)
      .fold[Set[java.lang.Throwable] \/ types.ModelStructuredDataType]{
      val _st = types.ModelStructuredDataType(dt, name, uuid)
      st += _st
      iri2typeTerm += dt.getIRI -> _st
      \/-(_st)
    } {
      case t: types.ModelStructuredDataType =>
        Set(
          entityAlreadyDefinedException(StructuredDataType, dt.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(StructuredDataType, dt.getIRI, t)
        ).left
    }

  def addStructuredDataType
  (structuredDataTypeIRI: IRI, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelStructuredDataType
  = iri2typeTerm
      .get(structuredDataTypeIRI)
      .fold[Set[java.lang.Throwable] \/ types.ModelStructuredDataType]{
      val structuredDataTypeC = owlDataFactory
        .getOWLClass(structuredDataTypeIRI)
      for {
        result <- createModelStructuredDataType(structuredDataTypeC, name, uuid)
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(ont,
              owlDataFactory
                .getOWLDeclarationAxiom(structuredDataTypeC)),
            new AddAxiom(ont,
              owlDataFactory
                .getOWLSubClassOfAxiom(structuredDataTypeC, backbone.StructuredDatatypeC)),
            new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
              store.RDFS_LABEL, structuredDataTypeIRI, owlDataFactory.getOWLLiteral(name))),
            new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
              store.ANNOTATION_HAS_UUID, structuredDataTypeIRI, owlDataFactory.getOWLLiteral(uuid.toString)))
          ),
          "addStructuredDataType error")
        } yield result
      } {
    case t: types.ModelStructuredDataType =>
      Set(
        entityAlreadyDefinedException(StructuredDataType, structuredDataTypeIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(StructuredDataType, structuredDataTypeIRI, t)
      ).left
  }

  def createDataRelationshipFromEntityToScalar
  (esc: OWLDataProperty,
   source: ModelEntityDefinition,
   target: ModelScalarDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar
  = for {
    n <- getTermLocalNameFromAssertionOrFromIRI(ont, esc.getIRI)
    u <- getTermUUIDFromAssertionOrFromIRI(ont, esc.getIRI)
    term <- createDataRelationshipFromEntityToScalar(esc, n, u, source, target)
  } yield term

  def createDataRelationshipFromEntityToScalar
  (esc: OWLDataProperty, name: LocalName, uuid: UUID,
   source: ModelEntityDefinition,
   target: ModelScalarDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar
  = {
    val escIRI: IRI = esc.getIRI
    iri2typeTerm
      .get(escIRI)
      .fold[Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar](
      for {
        _esc <- store.createDataRelationshipFromEntityToScalar(
          this, types.ModelDataRelationshipFromEntityToScalar(esc, name, uuid, source, target))
      } yield {
        e2sc += _esc
        iri2typeTerm += escIRI -> _esc
        _esc
      }) {
      case t: types.ModelDataRelationshipFromEntityToScalar =>
        Set(
          entityAlreadyDefinedException(EntityAspect, escIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityAspect, escIRI, t)
        ).left
    }
  }

  protected def makeDataRelationshipFromEntityToScalar
  (dIRI: IRI, name: LocalName, uuid: UUID,
   source: types.ModelEntityDefinition,
   target: types.ModelScalarDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar
  = {
    val escDP = owlDataFactory.getOWLDataProperty(dIRI)
    for {
      term <- createDataRelationshipFromEntityToScalar(escDP, name, uuid, source, target)
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
              .getOWLDataPropertyRangeAxiom(escDP, owlDataFactory.getOWLDatatype(target.iri))),
          new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
            store.RDFS_LABEL, dIRI, owlDataFactory.getOWLLiteral(name))),
          new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(
            store.ANNOTATION_HAS_UUID, dIRI, owlDataFactory.getOWLLiteral(uuid.toString)))
        )
      } {
        val result = ontManager.applyChange(change)
        require(
          result == ChangeApplied.SUCCESSFULLY,
          s"\nmakeDataRelationshipFromEntityToScalar:\n$change")
      }
      term
    }
  }

  def addDataRelationshipFromEntityToScalar
  (dIRI: IRI, name: LocalName, uuid: UUID,
   source: types.ModelEntityDefinition,
   target: types.ModelScalarDataType)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar
  = iri2typeTerm
      .get(dIRI)
      .fold[Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToScalar]({
      (isTypeTermDefinedRecursively(source),
        isTypeTermDefinedRecursively(target)) match {
        case (true, true) =>
          makeDataRelationshipFromEntityToScalar(dIRI, name, uuid, source, target)
        case (false, true) =>
          Set(
            entityScopeException(DataRelationshipFromEntityToScalar, dIRI,
              Map(RelationshipScopeAccessKind.Source -> source))
          ).left
        case (true, false) =>
          Set(
            entityScopeException(DataRelationshipFromEntityToScalar, dIRI,
              Map(RelationshipScopeAccessKind.Target -> target))
          ).left
        case (false, false) =>
          Set(
            entityScopeException(DataRelationshipFromEntityToScalar, dIRI,
              Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
          ).left
      }
    }) { term =>
      Set(
        entityConflictException(DataRelationshipFromEntityToScalar, dIRI, term)
      ).left
    }

  def addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  (entityDomain: types.ModelEntityDefinition,
   scalarDataProperty: types.ModelDataRelationshipFromEntityToScalar,
   literalRange: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  = for {
    uuid <- scalarDataRelationshipRestrictionAxiomFromEntityToLiteralUUID(this, entityDomain, scalarDataProperty)
    ax <- addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(uuid, entityDomain, scalarDataProperty, literalRange)
  } yield ax

  def addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  (uuid: UUID,
   entityDomain: types.ModelEntityDefinition,
   scalarDataProperty: types.ModelDataRelationshipFromEntityToScalar,
   literalRange: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  = (isTypeTermDefinedRecursively(entityDomain),
    isTypeTermDefinedRecursively(scalarDataProperty)) match {
    case (true, true) =>
      for {
        axiom <-
        createScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(
          uuid, entityDomain, scalarDataProperty, literalRange)
      } yield {
        val restrictedEntityC = owlDataFactory.getOWLClass(entityDomain.iri)
        val restrictingDP = owlDataFactory.getOWLDataProperty(scalarDataProperty.iri)
        for {
          change <- Seq(
            new AddAxiom(ont,
              owlDataFactory.getOWLSubClassOfAxiom(
                restrictedEntityC,
                owlDataFactory.getOWLDataSomeValuesFrom(
                  restrictingDP,
                  owlDataFactory.getOWLDataOneOf(owlDataFactory.getOWLLiteral(literalRange))),
                java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid))))
          )
        } {
          val result = ontManager.applyChange(change)
          require(
            result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
            s"\naddScalarDataRelationshipRestrictionAxiomFromEntityToLiteral:\n$change")
        }
        axiom
      }
    case (false, true) =>
      Set(
        axiomScopeException(
          ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteralException,
          Map(Domain -> entityDomain))
      ).left
    case (true, false) =>
      Set(
        axiomScopeException(
          ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteralException,
          Map(Rel -> scalarDataProperty))
      ).left
    case (false, false) =>
      Set(
        axiomScopeException(
          ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteralException,
          Map(Domain -> entityDomain, Rel -> scalarDataProperty))
      ).left
  }

  def createScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  (uuid: UUID,
   entityDomain: types.ModelEntityDefinition,
   scalarDataProperty: types.ModelDataRelationshipFromEntityToScalar,
   literalRange: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral
  = ax
    .find {
      case axiom: types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral =>
        axiom.restrictedEntity == entityDomain && axiom.restrictingDataProperty == scalarDataProperty
      case _ =>
        false
    }
    .fold[Set[java.lang.Throwable] \/ types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral](
    for {
      axiom <-
      store
        .createOMFScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(
          this,
          types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(
            uuid, entityDomain, scalarDataProperty, literalRange))
    } yield {
      ax += axiom
      axiom
    }
  ) { other =>
    Set(
      duplicateModelTermAxiomException(ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteralException, other)
    ).left
  }

  def addDataRelationshipFromEntityToStructure
  (dIRI: IRI, name: LocalName, uuid: UUID,
   source: types.ModelEntityDefinition,
   target: types.ModelStructuredDataType)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromEntityToStructure
  = ???

  def addDataRelationshipFromStructureToScalar
  (dIRI: IRI, name: LocalName, uuid: UUID,
   source: types.ModelStructuredDataType,
   target: types.ModelScalarDataType)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromStructureToScalar
  = ???

  def addDataRelationshipFromStructureToStructure
  (dIRI: IRI, name: LocalName, uuid: UUID,
   source: types.ModelStructuredDataType,
   target: types.ModelStructuredDataType)
  : Set[java.lang.Throwable] \/ types.ModelDataRelationshipFromStructureToStructure
  = ???

  def createEntityConceptSubClassAxiom
  (uuid: UUID,
   sub: types.ModelEntityConcept,
   sup: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityConceptSubClassAxiom
  = ax
      .find {
        case axiom: types.EntityConceptSubClassAxiom =>
          axiom.sub == sub && axiom.sup == sup
        case _ =>
          false
      }
      .fold[Set[java.lang.Throwable] \/ types.EntityConceptSubClassAxiom](
      for {
        axiom <- store
          .createOMFEntityConceptSubClassAxiomInstance(this,
            EntityConceptSubClassAxiom(uuid, sub, sup))
      } yield {
        ax += axiom
        axiom
      }
    ) { other =>
      Set(
        duplicateModelTermAxiomException(EntityConceptSubClassAxiomException, other)
      ).left
    }

  def addEntityConceptSubClassAxiom
  (uuid: UUID,
   sub: types.ModelEntityConcept,
   sup: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityConceptSubClassAxiom
  = (isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(sup)) match {
      case (true, true) =>
        for {
          axiom <- createEntityConceptSubClassAxiom(uuid, sub, sup)
        } yield {
          val subC = owlDataFactory.getOWLClass(sub.iri)
          val supC = owlDataFactory.getOWLClass(sup.iri)
          for {
            change <- Seq(
              new AddAxiom(ont,
                owlDataFactory
                  .getOWLSubClassOfAxiom(
                    subC,
                    supC,
                    java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid))))
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
        Set(
          axiomScopeException(ConceptSubclassAxiomException, Map(Sub -> sub))
        ).left

      case (true, false) =>
        Set(
          axiomScopeException(ConceptSubclassAxiomException, Map(Sup -> sup))
        ).left

      case (false, false) =>
        Set(
          axiomScopeException(ConceptSubclassAxiomException, Map(Sub -> sub, Sup -> sup))
        ).left
    }

  def addEntityDefinitionUniversalRestrictionAxiom
  (sub: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionUniversalRestrictionAxiom
  = for {
    uuid <- entityUniversalRestrictionAxiomUUID(this, sub, rel, range)
    ax <- addEntityDefinitionUniversalRestrictionAxiom(uuid, sub, rel, range)
  } yield ax

  def addEntityDefinitionUniversalRestrictionAxiom
  (uuid: UUID,
   sub: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionUniversalRestrictionAxiom
  = (isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(rel),
      isTypeTermDefinedRecursively(range)) match {
      case (true, true, true) =>
        val subC = owlDataFactory.getOWLClass(sub.iri)
        val rangeC = owlDataFactory.getOWLClass(range.iri)
        for {
          axiom <-
          store
            .createOMFEntityDefinitionUniversalRestrictionAxiomInstance(this,
              EntityDefinitionUniversalRestrictionAxiom(uuid, sub, rel, range))
          _ <- applyOntologyChangeOrNoOp(
            ontManager,
            new AddAxiom(ont,
              owlDataFactory
                .getOWLSubClassOfAxiom(
                  subC,
                  owlDataFactory.getOWLObjectAllValuesFrom(rel.unreified, rangeC),
                  java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid)))),
            ifError = {
              "addEntityDefinitionUniversalRestrictionAxiom Error"
            })
        } yield {
          ax += axiom
          axiom
        }

      case (false, _, _) =>
        Set(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Sub -> sub))
        ).left

      case (_, false, _) =>
        Set(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Rel -> rel))
        ).left

      case (_, _, false) =>
        Set(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Range -> range))
        ).left

    }

  def addEntityDefinitionExistentialRestrictionAxiom
  (sub: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionExistentialRestrictionAxiom
  = for {
    uuid <- entityExistentialRestrictionAxiomUUID(this, sub, rel, range)
    ax <- addEntityDefinitionExistentialRestrictionAxiom(uuid, sub, rel, range)
  } yield ax

  def addEntityDefinitionExistentialRestrictionAxiom
  (uuid: UUID,
   sub: types.ModelEntityDefinition,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionExistentialRestrictionAxiom
  = (isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(rel),
      isTypeTermDefinedRecursively(range)) match {
      case (true, true, true) =>
        val subC = owlDataFactory.getOWLClass(sub.iri)
        val rangeC = owlDataFactory.getOWLClass(range.iri)
        for {
          axiom <-
          store
            .createOMFEntityDefinitionExistentialRestrictionAxiomInstance(this,
              EntityDefinitionExistentialRestrictionAxiom(uuid, sub, rel, range))
          _ <- store.applyModelTermAxiomChanges(
            axiom,
            "addEntityDefinitionExistentialRestrictionAxiom",
            Seq(
              new AddAxiom(ont,
                owlDataFactory
                .getOWLSubClassOfAxiom(
                  subC,
                  owlDataFactory.getOWLObjectSomeValuesFrom(rel.unreified, rangeC),
                  java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid))))
            ))
          } yield {
          ax += axiom
          axiom
        }

      case (false, _, _) =>
        Set(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Sub -> sub))
        ).left

      case (_, false, _) =>
        Set(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Rel -> rel))
        ).left

      case (_, _, false) =>
        Set(
          axiomScopeException(
            ConceptRestrictionAxiomException,
            Map(AxiomScopeAccessKind.Range -> range))
        ).left
    }

  def createEntityDefinitionAspectSubClassAxiom
  (uuid: UUID,
   sub: types.ModelEntityDefinition,
   sup: types.ModelEntityAspect)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionAspectSubClassAxiom
  = ax
      .find {
        case axiom: types.EntityDefinitionAspectSubClassAxiom =>
          axiom.sub == sub && axiom.sup == sup
        case _ =>
          false
      }
      .fold[Set[java.lang.Throwable] \/ types.EntityDefinitionAspectSubClassAxiom](
      for {
        axiom <-
        store
          .createOMFEntityDefinitionAspectSubClassAxiomInstance(this,
            EntityDefinitionAspectSubClassAxiom(uuid, sub, sup))
      } yield {
        ax += axiom
        axiom
      }
    ) { other =>
      Set(
        duplicateModelTermAxiomException(EntityDefinitionAspectSubClassAxiomException, other)
      ).left
    }

  def addEntityDefinitionAspectSubClassAxiom
  (uuid: UUID,
   sub: types.ModelEntityDefinition,
   sup: types.ModelEntityAspect)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityDefinitionAspectSubClassAxiom
  = (isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(sup)) match {
      case (true, true) =>
        for {
          axiom <- createEntityDefinitionAspectSubClassAxiom(uuid, sub, sup)
        } yield {
          for {
            change <-
            Seq(new AddAxiom(ont,
              owlDataFactory
                .getOWLSubClassOfAxiom(
                  sub.e,
                  sup.e,
                  java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid))))
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
        Set(
          axiomScopeException(EntityDefinitionAspectSubClassAxiomException, Map(Sub -> sub))
        ).left

      case (true, false) =>
        Set(
          axiomScopeException(EntityDefinitionAspectSubClassAxiomException, Map(Sup -> sub))
        ).left

      case (false, false) =>
        Set(
          axiomScopeException(EntityDefinitionAspectSubClassAxiomException, Map(Sub -> sub, Sup -> sub))
        ).left
    }

  def createOMFEntityConceptDesignationTerminologyGraphAxiom
  (uuid: UUID,
   entityConceptDesignation: types.ModelEntityConcept,
   designationTerminologyGraph: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityConceptDesignationTerminologyGraphAxiom
  = ax
      .find {
        case axiom: types.EntityConceptDesignationTerminologyGraphAxiom =>
          axiom.entityConceptDesignation == entityConceptDesignation &&
            axiom.designationTerminologyGraph == designationTerminologyGraph
        case _ =>
          false
      }
      .fold[Set[java.lang.Throwable] \/ types.EntityConceptDesignationTerminologyGraphAxiom]({
      val axInstance = EntityConceptDesignationTerminologyGraphAxiom(
        uuid, entityConceptDesignation, designationTerminologyGraph)
      for {
        axiom <- store.createOMFEntityConceptDesignationTerminologyGraphAxiomInstance(this, axInstance)
      } yield {
        ax += axiom
        axiom
      }
    }) { other =>
      Set(
        duplicateModelTermAxiomException(EntityConceptDesignationTerminologyGraphAxiomException, other)
      ).left
    }

  def addEntityConceptDesignationTerminologyGraphAxiom
  (uuid: UUID,
   entityConceptDesignation: types.ModelEntityConcept,
   designationTerminologyGraph: types.ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityConceptDesignationTerminologyGraphAxiom
  = for {
      axiom <- createOMFEntityConceptDesignationTerminologyGraphAxiom(
        uuid, entityConceptDesignation, designationTerminologyGraph)
    } yield {
      ax += axiom
      axiom
    }

  def createEntityReifiedRelationshipSubClassAxiom
  (uuid: UUID,
   sub: types.ModelEntityReifiedRelationship,
   sup: types.ModelEntityReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityReifiedRelationshipSubClassAxiom
  = ax
      .find {
        case axiom: types.EntityReifiedRelationshipSubClassAxiom =>
          axiom.sub == sub && axiom.sup == sup
        case _ =>
          false
      }
      .fold[Set[java.lang.Throwable] \/ types.EntityReifiedRelationshipSubClassAxiom]({
      val axInstance = EntityReifiedRelationshipSubClassAxiom(uuid, sub, sup)
      for {
        axiom <- store
          .createOMFEntityReifiedRelationshipSubClassAxiomInstance(this, axInstance)
      } yield {
        ax += axiom
        axiom
      }
    }) { other =>
      Set(
        duplicateModelTermAxiomException(EntityReifiedRelationshipSubClassAxiomException, other)
      ).left
    }

  def addEntityReifiedRelationshipSubClassAxiom
  (uuid: UUID,
   sub: types.ModelEntityReifiedRelationship,
   sup: types.ModelEntityReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.EntityReifiedRelationshipSubClassAxiom
  = (isTypeTermDefinedRecursively(sub), isTypeTermDefinedRecursively(sup)) match {
      case (true, true) =>
        for {
          axiom <- createEntityReifiedRelationshipSubClassAxiom(uuid, sub, sup)
        } yield {
          for {
            change <- Seq(
              new AddAxiom(ont,
                owlDataFactory
                  .getOWLSubClassOfAxiom(
                    sub.e,
                    sup.e,
                    java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid)))),
              new AddAxiom(ont,
                owlDataFactory
                  .getOWLSubObjectPropertyOfAxiom(
                    sub.rSource,
                    sup.rSource,
                    java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid)))),
              new AddAxiom(ont,
                owlDataFactory
                  .getOWLSubObjectPropertyOfAxiom(
                    sub.rTarget,
                    sup.rTarget,
                    java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid))))
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
        Set(
          axiomScopeException(ReifiedRelationshipSubclassAxiomException, Map(Sub -> sub))
        ).left

      case (true, false) =>
        Set(
          axiomScopeException(ReifiedRelationshipSubclassAxiomException, Map(Sup -> sup))
        ).left

      case (false, false) =>
        Set(
          axiomScopeException(ReifiedRelationshipSubclassAxiomException, Map(Sub -> sub, Sup -> sup))
        ).left
    }

  def createScalarDataTypeFacetRestrictionAxiom
  (uuid: UUID,
   sub: types.ModelScalarDataType,
   sup: types.ModelScalarDataType,
   fundamentalFacets: Iterable[FundamentalFacet],
   constrainingFacets: Iterable[ConstrainingFacet])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ScalarDataTypeFacetRestrictionAxiom
  = ax
    .find {
      case axiom: types.ScalarDataTypeFacetRestrictionAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[Set[java.lang.Throwable] \/ types.ScalarDataTypeFacetRestrictionAxiom](
    for {
      axiom <-
      store
        .createOMFScalarDataTypeFacetRestrictionAxiomInstance(
          this,
          ScalarDataTypeFacetRestrictionAxiom(uuid, sub, sup, fundamentalFacets, constrainingFacets))
    } yield {
      ax += axiom
      axiom
    }
  ) { other =>
    Set(
      duplicateModelTermAxiomException(ScalarDataTypeFacetRestrictionAxiomException, other)
    ).left
  }

  def addScalarDataTypeFacetRestrictionAxiom
  (uuid: UUID,
   sub: types.ModelScalarDataType,
   sup: types.ModelScalarDataType,
   fundamentalFacets: Iterable[FundamentalFacet],
   constrainingFacets: Iterable[ConstrainingFacet])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ types.ScalarDataTypeFacetRestrictionAxiom
  = (isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(sup)) match {
      case (true, true) =>
        for {
          axiom <- createScalarDataTypeFacetRestrictionAxiom(uuid, sub, sup, fundamentalFacets, constrainingFacets)
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
          axiom
        }

      case (false, true) =>
        Set(
          axiomScopeException(ScalarDataTypeFacetRestrictionAxiomException, Map(Sub -> sub))
        ).left

      case (true, false) =>
        Set(
          axiomScopeException(ScalarDataTypeFacetRestrictionAxiomException, Map(Sup -> sub))
        ).left

      case (false, false) =>
        Set(
          axiomScopeException(ScalarDataTypeFacetRestrictionAxiomException, Map(Sub -> sub, Sup -> sub))
        ).left
    }


}