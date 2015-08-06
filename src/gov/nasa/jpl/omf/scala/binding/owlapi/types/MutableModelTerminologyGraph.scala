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

import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.ChangeApplied

import scala.collection.JavaConversions._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

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
  EntityReifiedRelationshipSubClassAxiomException = Value
}

import gov.nasa.jpl.omf.scala.binding.owlapi.types.AxiomExceptionKind._

object AxiomScopeAccessKind extends Enumeration {
  type AxiomScopeAccessKind = Value
  val Sub, Sup, Rel, Range = Value
}

import gov.nasa.jpl.omf.scala.binding.owlapi.types.AxiomScopeAccessKind._

sealed abstract class MutableModelTerminologyGraphException(val message: String)
  extends IllegalArgumentException(message) {
  require(null != message)
}

case class EntityAlreadyDefinedException
(kind: EntityExceptionKind,
 iri: IRI,
 term: ModelTypeTerm)
  extends MutableModelTerminologyGraphException(
    s"Cannot create $kind with IRI='$iri' because it is already defined as: $term") {

  require(null != kind)
  require(null != iri)
  require(null != term)
}

case class EntityConflictException
(kind: EntityExceptionKind,
 iri: IRI,
 conflictingTerm: ModelTypeTerm)
  extends MutableModelTerminologyGraphException(
    s"Cannot create $kind with IRI='$iri' because this IRI refers to: $conflictingTerm") {

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
        |there are ${unaccessibleTerms.size} terms out of scope of the graph: """.stripMargin +
      (unaccessibleTerms.map { case (kind, term) => s"$kind: $term" } mkString ", "))

case class AxiomScopeException
(kind: AxiomExceptionKind,
 unaccessibleTerms: Map[AxiomScopeAccessKind, ModelTypeTerm])
  extends MutableModelTerminologyGraphException(
    s"""Cannot create $kind because
        |there are ${unaccessibleTerms.size} terms out of scope of the graph: """.stripMargin +
      (unaccessibleTerms.map { case (kind, term) => s"$kind: $term" } mkString ", "))

case class DuplicateModelTermAxiomException
(kind: AxiomExceptionKind,
 axiom: ModelTermAxiom)
  extends MutableModelTerminologyGraphException(
    s"""Cannot create $kind because
        |the axiom is already asserted $axiom""".stripMargin)

case class MutableModelTerminologyGraph
(override val kind: TerminologyKind,
 override val ont: OWLOntology)
(override implicit val ops: OWLAPIOMFOps)
  extends ModelTerminologyGraph(kind, ont)(ops) {

  override val isImmutableModelTerminologyGraph = true
  override val isMutableModelTerminologyGraph = false

  override val kindIRI: IRI = makeKindIRI("mutable")

  val LOG: Boolean = true

  def setTerminologyGraphShortName
  (shortName: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Try[Unit] =
    for {
      c1 <- getTerminologyGraphShortNameAnnotation match {
        case Some(annotation) =>
          ontManager.applyChange(new RemoveOntologyAnnotation(ont, annotation)) match {
            case ChangeApplied.SUCCESSFULLY =>
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to remove the tbox ontology 'rdfs:label' annotation"))
          }
        case None =>
          Success(Unit)
      }
      c2 <- shortName match {
        case None =>
          Success(Unit)
        case Some(label) =>
          ontManager.applyChange(new AddOntologyAnnotation(
            ont,
            owlDataFactory.getOWLAnnotation(
              omfStore.RDFS_LABEL,
              owlDataFactory.getOWLLiteral(label)))) match {
            case ChangeApplied.SUCCESSFULLY =>
              if (LOG)
                System.out.println(s"setTerminologyGraphShortName: $kindIRI name='$label'")
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to add the tbox ontology 'rdfs:label' annotation"))
          }
      }
    } yield ()

  def setTerminologyGraphUUID
  (uuid: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Try[Unit] =
    for {
      c1 <- getTerminologyGraphUUIDAnnotation match {
        case Some(annotation) =>
          ontManager.applyChange(new RemoveOntologyAnnotation(ont, annotation)) match {
            case ChangeApplied.SUCCESSFULLY =>
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to remove the tbox ontology 'uuid' annotation"))
          }
        case None =>
          Success(Unit)
      }
      c2 <- uuid match {
        case None =>
          Success(Unit)
        case Some(id) =>
          ontManager.applyChange(new AddOntologyAnnotation(
            ont,
            owlDataFactory.getOWLAnnotation(
              omfStore.ANNOTATION_HAS_UUID,
              owlDataFactory.getOWLLiteral(id)))) match {
            case ChangeApplied.SUCCESSFULLY =>
              if (LOG)
                System.out.println(s"setTerminologyGraphUUID: $kindIRI uuid='$id'")
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to add the tbox ontology 'uuid' annotation"))
          }
      }
    } yield ()


  import ops._

  val rdfs_labelAP = owlDataFactory.getOWLAnnotationProperty(rdfs_label)
  val isAbstractAP = owlDataFactory.getOWLAnnotationProperty(AnnotationIsAbstract)
  val isDerivedAP = owlDataFactory.getOWLAnnotationProperty(AnnotationIsDerived)

  override protected val aspects = scala.collection.mutable.ListBuffer[ModelEntityAspect]()
  override protected val concepts = scala.collection.mutable.ListBuffer[ModelEntityConcept]()
  override protected val reifiedRelationships = scala.collection.mutable.ListBuffer[ModelEntityReifiedRelationship]()
  override protected val unreifiedRelationships = scala.collection.mutable.ListBuffer[ModelEntityUnreifiedRelationship]()
  override protected val sc = scala.collection.mutable.ListBuffer[ModelScalarDataType]()
  override protected val st = scala.collection.mutable.ListBuffer[ModelStructuredDataType]()
  override protected val e2sc = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromEntityToScalar]()
  override protected val e2st = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromEntityToStructure]()
  override protected val s2sc = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromStructureToScalar]()
  override protected val s2st = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromStructureToStructure]()
  override protected val ax = scala.collection.mutable.ListBuffer[ModelTermAxiom]()

  override def getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition] =
    ((aspects map (a => a.e -> a)) ++
      (concepts map (c => c.e -> c)) ++
      (reifiedRelationships map (r => r.e -> r))) toMap

  override def getScalarDatatypeDefinitionMap: Map[OWLDatatype, ModelScalarDataType] =
    sc map (t => t.sc -> t) toMap

  override protected val iri2typeTerm = scala.collection.mutable.HashMap[IRI, ModelTypeTerm]()

  val backbone = Backbone.createBackbone(ont, kind, ops).get

  def addTerminologyGraphExtension
  (extendedG: ModelTerminologyGraph)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.TerminologyGraphDirectExtensionAxiom] =
    for {
      axiom <- store.createTerminologyGraphDirectExtensionAxiom(this, extendedG)
    } yield {
      for {
        change <- Seq(
          new AddImport(ont, ontManager.getOWLDataFactory.getOWLImportsDeclaration(extendedG.iri))
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
  : Try[Unit] =
    for {
      c1 <- getTermShortNameAnnotationAssertionAxiom(term) match {
        case Some(annotationAssertionAxiom) =>
          ontManager.applyChange(new RemoveAxiom(ont, annotationAssertionAxiom)) match {
            case ChangeApplied.SUCCESSFULLY =>
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to remove a tbox ontology 'rdfs:label' annotation assertion axiom"))
          }
        case None =>
          Success(Unit)
      }
      c2 <- shortName match {
        case None =>
          Success(Unit)
        case Some(label) =>
          ontManager.applyChange(new AddAxiom(
            ont,
            owlDataFactory.getOWLAnnotationAssertionAxiom(
              omfStore.RDFS_LABEL,
              term.iri,
              owlDataFactory.getOWLLiteral(label)))) match {
            case ChangeApplied.SUCCESSFULLY =>
              if (LOG)
                System.out.println(s"setTermShortName: ${term.iri} name='$label'")
              omfStore.setTermShortName(this, term, label)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to add a tbox ontology 'uuid' annotation assertion axiom"))
          }
      }
    } yield ()

  def setTermUUID
  (term: types.ModelTypeTerm,
   uuid: Option[String])
  (implicit omfStore: OWLAPIOMFGraphStore)
  : Try[Unit] =
    for {
      c1 <- getTermUUIDAnnotationAssertionAxiom(term) match {
        case Some(annotationAssertionAxiom) =>
          ontManager.applyChange(new RemoveAxiom(ont, annotationAssertionAxiom)) match {
            case ChangeApplied.SUCCESSFULLY =>
              Success(Unit)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to remove a tbox ontology 'uuid' annotation assertion axiom"))
          }
        case None =>
          Success(Unit)
      }
      c2 <- uuid match {
        case None =>
          Success(Unit)
        case Some(id) =>
          ontManager.applyChange(new AddAxiom(
            ont,
            owlDataFactory.getOWLAnnotationAssertionAxiom(
              omfStore.ANNOTATION_HAS_UUID,
              term.iri,
              owlDataFactory.getOWLLiteral(id)))) match {
            case ChangeApplied.SUCCESSFULLY =>
              if (LOG)
                System.out.println(s"setTermUUID: ${term.iri} uuid='$id'")
              omfStore.setTermUUID(this, term, id)
            case ChangeApplied.UNSUCCESSFULLY =>
              Failure(new IllegalArgumentException(
                s"Failed to add a tbox ontology 'uuid' annotation assertion axiom"))
          }
      }
    } yield ()

  def createModelEntityAspect
  (a: OWLClass)
  : Try[types.ModelEntityAspect] =
    iri2typeTerm.get(a.getIRI) match {
      case None =>
        val _a = types.ModelEntityAspect(a)
        aspects += _a
        iri2typeTerm += a.getIRI -> _a
        Success(_a)
      case Some(t: types.ModelEntityAspect) =>
        Failure(EntityAlreadyDefinedException(EntityAspect, a.getIRI, t))
      case Some(t) =>
        Failure(EntityConflictException(EntityAspect, a.getIRI, t))
    }

  def addEntityAspect
  (aspectIRI: IRI)
  : Try[types.ModelEntityAspect] =
    iri2typeTerm get aspectIRI match {
      case None =>
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
      case Some(t: types.ModelEntityAspect) =>
        Failure(EntityAlreadyDefinedException(EntityAspect, aspectIRI, t))
      case Some(t) =>
        Failure(EntityConflictException(EntityAspect, aspectIRI, t))
    }

  def createModelEntityConcept
  (c: OWLClass,
   isAbstract: Boolean)
  : Try[types.ModelEntityConcept] =
    iri2typeTerm.get(c.getIRI) match {
      case None =>
        val _c = types.ModelEntityConcept(c, isAbstract)
        concepts += _c
        iri2typeTerm += c.getIRI -> _c
        Success(_c)
      case Some(t: types.ModelEntityConcept) =>
        Failure(EntityAlreadyDefinedException(EntityConcept, c.getIRI, t))
      case Some(t) =>
        Failure(EntityConflictException(EntityConcept, c.getIRI, t))
    }

  def addEntityConcept
  (conceptIRI: IRI,
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.ModelEntityConcept] =

    iri2typeTerm get conceptIRI match {
      case None =>
        val conceptC = owlDataFactory.getOWLClass(conceptIRI)
        for {
          result <- createModelEntityConcept(conceptC, isAbstract)
        } yield {
          for {
            change <- Seq(
              new AddAxiom(ont,
                owlDataFactory.getOWLDeclarationAxiom(conceptC)),
              new AddAxiom(ont,
                owlDataFactory.getOWLAnnotationAssertionAxiom(
                  isAbstractAP, conceptIRI, owlDataFactory.getOWLLiteral(isAbstract))),
              new AddAxiom(ont,
                owlDataFactory.getOWLSubClassOfAxiom(conceptC, backbone.EntityC)))
          } {
            val result = ontManager.applyChange(change)
            require(
              result == ChangeApplied.SUCCESSFULLY,
              s"\naddEntityConcept:\n$change")
          }
          result
        }
      case Some(t: types.ModelEntityConcept) =>
        Failure(EntityAlreadyDefinedException(EntityConcept, conceptIRI, t))
      case Some(t) =>
        Failure(EntityConflictException(EntityConcept, conceptIRI, t))
    }

  def createEntityReifiedRelationship
  (r: OWLClass,
   u: OWLObjectProperty, ui: Option[OWLObjectProperty],
   source: ModelEntityDefinition, rSource: OWLObjectProperty,
   target: ModelEntityDefinition, rTarget: OWLObjectProperty,
   characteristics: Iterable[RelationshipCharacteristics],
   isAbstract: Boolean)
  : Try[types.ModelEntityReifiedRelationship] =
    iri2typeTerm.get(r.getIRI) match {
      case None =>
        val _r = types.ModelEntityReifiedRelationship(
          r,
          u, ui,
          source, rSource,
          target, rTarget,
          characteristics, isAbstract)
        reifiedRelationships += _r
        iri2typeTerm += r.getIRI -> _r
        Success(_r)
      case Some(t: types.ModelEntityReifiedRelationship) =>
        Failure(EntityAlreadyDefinedException(EntityReifiedRelationship, r.getIRI, t))
      case Some(t) =>
        Failure(EntityConflictException(EntityReifiedRelationship, r.getIRI, t))
    }

  protected def makeEntityReifiedRelationship
  (rIRI: IRI,
   rIRISource: IRI, rIRITarget: IRI,
   uIRI: IRI, uiIRI: Option[IRI],
   source: ModelEntityDefinition, target: ModelEntityDefinition,
   characteristics: Iterable[RelationshipCharacteristics],
   isAbstract: Boolean)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.ModelEntityReifiedRelationship] = {

    val sourceC = owlDataFactory.getOWLClass(source.iri)
    val targetC = owlDataFactory.getOWLClass(target.iri)
    val r = owlDataFactory.getOWLClass(rIRI)
    val rSource = owlDataFactory.getOWLObjectProperty(rIRISource)
    val rTarget = owlDataFactory.getOWLObjectProperty(rIRITarget)
    val u = owlDataFactory.getOWLObjectProperty(uIRI)
    val ui = if (uiIRI.isEmpty) None else Some(owlDataFactory.getOWLObjectProperty(uiIRI.get))

    for {
      result <- createEntityReifiedRelationship(
        r, u, ui,
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
        change <- Seq(
          new AddAxiom(ont,
            owlDataFactory.getOWLDeclarationAxiom(r)),
          new AddAxiom(ont,
            owlDataFactory.getOWLAnnotationAssertionAxiom(
              isAbstractAP, rIRI, owlDataFactory.getOWLLiteral(isAbstract))),
          new AddAxiom(ont,
            owlDataFactory.getOWLSubClassOfAxiom(r, backbone.ReifiedObjectPropertyC)),

          new AddAxiom(ont,
            owlDataFactory.getOWLDeclarationAxiom(rSource)),
          new AddAxiom(ont,
            owlDataFactory.getOWLSubObjectPropertyOfAxiom(rSource, backbone.topReifiedObjectPropertySourceOP)),
          new AddAxiom(ont,
            owlDataFactory.getOWLObjectPropertyDomainAxiom(rSource, r)),
          new AddAxiom(ont,
            owlDataFactory.getOWLObjectPropertyRangeAxiom(rSource, sourceC)),
          new AddAxiom(ont,
            owlDataFactory.getOWLFunctionalObjectPropertyAxiom(rSource)),

          new AddAxiom(ont,
            owlDataFactory.getOWLDeclarationAxiom(rTarget)),
          new AddAxiom(ont,
            owlDataFactory.getOWLSubObjectPropertyOfAxiom(rTarget, backbone.topReifiedObjectPropertyTargetOP)),
          new AddAxiom(ont,
            owlDataFactory.getOWLObjectPropertyDomainAxiom(rTarget, r)),
          new AddAxiom(ont,
            owlDataFactory.getOWLObjectPropertyRangeAxiom(rTarget, targetC)),
          new AddAxiom(ont,
            owlDataFactory.getOWLFunctionalObjectPropertyAxiom(rTarget)),

          new AddAxiom(ont,
            owlDataFactory.getOWLDeclarationAxiom(u)),
          new AddAxiom(ont,
            owlDataFactory.getOWLSubObjectPropertyOfAxiom(u, backbone.topReifiedObjectPropertyOP)),
          new AddAxiom(ont,
            owlDataFactory.getOWLObjectPropertyDomainAxiom(u, sourceC)),
          new AddAxiom(ont,
            owlDataFactory.getOWLObjectPropertyRangeAxiom(u, targetC)),

          //                    new AddAxiom(ont,
          //                      owlDataFactory.getOWLSubPropertyChainOfAxiom(
          //                        List(owlDataFactory.getOWLObjectInverseOf(rSource), rTarget), u)),
          new AddAxiom(ont, rule)
        ) ++
          (if (ui.isDefined)
            Seq(
              new AddAxiom(ont,
                owlDataFactory.getOWLDeclarationAxiom(ui.get)),
              new AddAxiom(ont,
                owlDataFactory.getOWLAnnotationAssertionAxiom(
                  isDerivedAP, ui.get.getIRI, owlDataFactory.getOWLLiteral(true))),
              new AddAxiom(ont,
                owlDataFactory.getOWLSubObjectPropertyOfAxiom(ui.get, backbone.topReifiedObjectPropertyOP)),
              new AddAxiom(ont,
                owlDataFactory.getOWLObjectPropertyDomainAxiom(ui.get, targetC)),
              new AddAxiom(ont,
                owlDataFactory.getOWLObjectPropertyRangeAxiom(ui.get, sourceC))

              //              new AddAxiom(ont,
              //                owlDataFactory.getOWLSubPropertyChainOfAxiom(
              //                  List(owlDataFactory.getOWLObjectInverseOf(rTarget), rSource), ui.get))
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
  : Try[types.ModelEntityReifiedRelationship] =
    (lookupTypeTerm(rIRI, recursively = true),
      lookupTypeTerm(rIRISource, recursively = true),
      lookupTypeTerm(rIRITarget, recursively = true),
      lookupTypeTerm(uIRI, recursively = true),
      lookupTypeTerm(uiIRI, recursively = true)) match {
      case (None, None, None, None, None) =>
        (isTypeTermDefinedRecursively(source), isTypeTermDefinedRecursively(target)) match {
          case (true, true) =>
            makeEntityReifiedRelationship(
              rIRI, rIRISource, rIRITarget, uIRI, uiIRI,
              source, target, characteristics, isAbstract)
          case (false, true) =>
            Failure(EntityScopeException(
              EntityReifiedRelationship, rIRI, Map(Source -> source)))

          case (true, false) =>
            Failure(EntityScopeException(
              EntityReifiedRelationship, rIRI, Map(Target -> target)))

          case (false, false) =>
            Failure(EntityScopeException(
              EntityReifiedRelationship, rIRI, Map(Source -> source, Target -> target)))
        }

      case (Some(t), _, _, _, _) =>
        Failure(EntityConflictException(
          EntityReifiedRelationship, rIRI, t))

      case (_, Some(t), _, _, _) =>
        Failure(EntityConflictException(
          EntityReifiedRelationship, rIRISource, t))

      case (_, _, Some(t), _, _) =>
        Failure(EntityConflictException(
          EntityReifiedRelationship, rIRITarget, t))

      case (_, _, _, Some(t), _) =>
        Failure(EntityConflictException(
          EntityReifiedRelationship, uIRI, t))

      case (_, _, _, _, Some(t)) =>
        require(uiIRI.isDefined)
        Failure(EntityConflictException(
          EntityReifiedRelationship, uiIRI.get, t))
    }

  def createModelScalarDataType
  (dt: OWLDatatype)
  : Try[types.ModelScalarDataType] =
    iri2typeTerm.get(dt.getIRI) match {
      case None =>
        val _dt = types.ModelScalarDataType(dt)
        sc += _dt
        iri2typeTerm += dt.getIRI -> _dt
        Success(_dt)
      case Some(t: types.ModelScalarDataType) =>
        Failure(EntityAlreadyDefinedException(ScalarDataType, dt.getIRI, t))
      case Some(t) =>
        Failure(EntityConflictException(ScalarDataType, dt.getIRI, t))
    }

  def addScalarDataType
  (scalarIRI: IRI)
  : Try[types.ModelScalarDataType] =
    iri2typeTerm get scalarIRI match {
      case None =>
        val scalarDT = owlDataFactory.getOWLDatatype(scalarIRI)
        for {
          result <- createModelScalarDataType(scalarDT)
        } yield {
          for {
            change <- Seq(
              new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(scalarDT))
            )
          } {
            val result = ontManager.applyChange(change)
            require(
              result == ChangeApplied.SUCCESSFULLY,
              s"\naddScalarDataType:\n$change")
          }
          result
        }
      case Some(t: types.ModelScalarDataType) =>
        Failure(EntityAlreadyDefinedException(ScalarDataType, scalarIRI, t))
      case Some(t) =>
        Failure(EntityConflictException(ScalarDataType, scalarIRI, t))
    }

  def createModelStructuredDataType
  (c: OWLClass)
  : Try[types.ModelStructuredDataType] =
    iri2typeTerm.get(c.getIRI) match {
      case None =>
        val _st = types.ModelStructuredDataType(c)
        st += _st
        iri2typeTerm += c.getIRI -> _st
        Success(_st)
      case Some(t: types.ModelStructuredDataType) =>
        Failure(EntityAlreadyDefinedException(StructuredDataType, c.getIRI, t))
      case Some(t) =>
        Failure(EntityConflictException(StructuredDataType, c.getIRI, t))
    }

  def addStructuredDataType
  (structuredDataTypeIRI: IRI)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.ModelStructuredDataType] =

    iri2typeTerm get structuredDataTypeIRI match {
      case None =>
        val structuredDataTypeC = owlDataFactory.getOWLClass(structuredDataTypeIRI)
        for {
          result <- createModelStructuredDataType(structuredDataTypeC)
        } yield {
          for {
            change <- Seq(
              new AddAxiom(ont,
                owlDataFactory.getOWLDeclarationAxiom(structuredDataTypeC)),
              new AddAxiom(ont,
                owlDataFactory.getOWLSubClassOfAxiom(structuredDataTypeC, backbone.StructuredDatatypeC))
            )
          } {
            val result = ontManager.applyChange(change)
            require(
              result == ChangeApplied.SUCCESSFULLY,
              s"\naddStructuredDataType:\n$change")
          }
          result
        }
      case Some(t: types.ModelStructuredDataType) =>
        Failure(EntityAlreadyDefinedException(StructuredDataType, structuredDataTypeIRI, t))
      case Some(t) =>
        Failure(EntityConflictException(StructuredDataType, structuredDataTypeIRI, t))
    }

  def createDataRelationshipFromEntityToScalar
  (esc: OWLDataProperty, source: ModelEntityDefinition, target: ModelScalarDataType)
  : Try[types.ModelDataRelationshipFromEntityToScalar] =
    iri2typeTerm.get(esc.getIRI) match {
      case None =>
        val _esc = types.ModelDataRelationshipFromEntityToScalar(esc, source, target)
        e2sc += _esc
        iri2typeTerm += esc.getIRI -> _esc
        Success(_esc)
      case Some(t: types.ModelDataRelationshipFromEntityToScalar) =>
        Failure(EntityAlreadyDefinedException(EntityAspect, esc.getIRI, t))
      case Some(t) =>
        Failure(EntityConflictException(EntityAspect, esc.getIRI, t))
    }

  protected def makeDataRelationshipFromEntityToScalar
  (dIRI: IRI,
   source: types.ModelEntityDefinition,
   target: types.ModelScalarDataType)
  : Try[types.ModelDataRelationshipFromEntityToScalar] = {
    val escDP = owlDataFactory.getOWLDataProperty(dIRI)
    for {
      result <- createDataRelationshipFromEntityToScalar(escDP, source, target)
    } yield {
      for {
        change <- Seq(
          new AddAxiom(ont,
            owlDataFactory.getOWLDeclarationAxiom(escDP)),
          new AddAxiom(ont,
            owlDataFactory.getOWLSubDataPropertyOfAxiom(escDP, backbone.topDataPropertyDP)),
          new AddAxiom(ont,
            owlDataFactory.getOWLDataPropertyDomainAxiom(escDP, source.e)),
          new AddAxiom(ont,
            owlDataFactory.getOWLDataPropertyRangeAxiom(escDP, owlDataFactory.getOWLDatatype(target.iri)))
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
  : Try[types.ModelDataRelationshipFromEntityToScalar] = iri2typeTerm get dIRI match {
    case None =>
      (isTypeTermDefinedRecursively(source),
        isTypeTermDefinedRecursively(target)
        ) match {
        case (true, true) =>
          makeDataRelationshipFromEntityToScalar(dIRI, source, target)
        case (false, true) =>
          Failure(EntityScopeException(
            DataRelationshipFromEntityToScalar, dIRI, Map(Source -> source)))

        case (true, false) =>
          Failure(EntityScopeException(
            DataRelationshipFromEntityToScalar, dIRI, Map(Target -> target)))

        case (false, false) =>
          Failure(EntityScopeException(
            DataRelationshipFromEntityToScalar, dIRI, Map(Source -> source, Target -> target)))
      }

    case Some(term) =>
      Failure(EntityConflictException(
        DataRelationshipFromEntityToScalar, dIRI, term))
  }

  def addDataRelationshipFromEntityToStructure
  (dIRI: IRI,
   source: types.ModelEntityDefinition,
   target: types.ModelStructuredDataType)
  : Try[types.ModelDataRelationshipFromEntityToStructure] = ???

  def addDataRelationshipFromStructureToScalar
  (dIRI: IRI,
   source: types.ModelStructuredDataType,
   target: types.ModelScalarDataType)
  : Try[types.ModelDataRelationshipFromStructureToScalar] = ???

  def addDataRelationshipFromStructureToStructure
  (dIRI: IRI,
   source: types.ModelStructuredDataType,
   target: types.ModelStructuredDataType)
  : Try[types.ModelDataRelationshipFromStructureToStructure] = ???

  def createEntityConceptSubClassAxiom
  (sub: types.ModelEntityConcept,
   sup: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.EntityConceptSubClassAxiom] =
    ax.find {
      case axiom: types.EntityConceptSubClassAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    } match {
      case None =>
        for {
          axiom <- store.createOMFEntityConceptSubClassAxiomInstance(
            this,
            EntityConceptSubClassAxiom(sub, sup))
        } yield {
          ax += axiom
          axiom
        }
      case Some(ax) =>
        Failure(DuplicateModelTermAxiomException(EntityConceptSubClassAxiomException, ax))
    }

  def addEntityConceptSubClassAxiom
  (sub: types.ModelEntityConcept,
   sup: types.ModelEntityConcept)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.EntityConceptSubClassAxiom] =
    (isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(sup)) match {
      case (true, true) =>
        for {
          axiom <- createEntityConceptSubClassAxiom(sub, sup)
        } yield {
          val subC = owlDataFactory.getOWLClass(sub.iri)
          val supC = owlDataFactory.getOWLClass(sup.iri)
          for {
            change <- Seq(
              new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(subC, supC))
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
        Failure(AxiomScopeException(ConceptSubclassAxiomException, Map(Sub -> sub)))

      case (true, false) =>
        Failure(AxiomScopeException(ConceptSubclassAxiomException, Map(Sup -> sup)))

      case (false, false) =>
        Failure(AxiomScopeException(ConceptSubclassAxiomException, Map(Sub -> sub, Sup -> sup)))
    }

  def addEntityConceptUniversalRestrictionAxiom
  (sub: types.ModelEntityConcept,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.EntityConceptUniversalRestrictionAxiom] =
    (isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(rel),
      isTypeTermDefinedRecursively(range)) match {
      case (true, true, true) =>
        val subC = owlDataFactory.getOWLClass(sub.iri)
        val rangeC = owlDataFactory.getOWLClass(range.iri)
        for {
          axiom <- store.createOMFEntityConceptUniversalRestrictionAxiomInstance(
            this, EntityConceptUniversalRestrictionAxiom(sub, rel, range))
        } yield {
          for {
            change <- Seq(
              new AddAxiom(ont,
                owlDataFactory.getOWLSubClassOfAxiom(
                  subC,
                  owlDataFactory.getOWLObjectAllValuesFrom(rel.unreified, rangeC)))
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
        Failure(AxiomScopeException(ConceptRestrictionAxiomException, Map(Sub -> sub)))

      case (_, false, _) =>
        Failure(AxiomScopeException(ConceptRestrictionAxiomException, Map(Rel -> rel)))

      case (_, _, false) =>
        Failure(AxiomScopeException(ConceptRestrictionAxiomException, Map(Range -> range)))

    }

  def addEntityConceptExistentialRestrictionAxiom
  (sub: types.ModelEntityConcept,
   rel: types.ModelEntityReifiedRelationship,
   range: types.ModelEntityDefinition)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.EntityConceptExistentialRestrictionAxiom] =
    (isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(rel),
      isTypeTermDefinedRecursively(range)) match {
      case (true, true, true) =>
        val subC = owlDataFactory.getOWLClass(sub.iri)
        val rangeC = owlDataFactory.getOWLClass(range.iri)
        for {
          axiom <- store.createOMFEntityConceptExistentialRestrictionAxiomInstance(
            this, EntityConceptExistentialRestrictionAxiom(sub, rel, range))
        } yield {
          for {
            change <- Seq(
              new AddAxiom(ont,
                owlDataFactory.getOWLSubClassOfAxiom(
                  subC,
                  owlDataFactory.getOWLObjectSomeValuesFrom(rel.unreified, rangeC)))
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
        Failure(AxiomScopeException(ConceptRestrictionAxiomException, Map(Sub -> sub)))

      case (_, false, _) =>
        Failure(AxiomScopeException(ConceptRestrictionAxiomException, Map(Rel -> rel)))

      case (_, _, false) =>
        Failure(AxiomScopeException(ConceptRestrictionAxiomException, Map(Range -> range)))
    }

  def createEntityDefinitionAspectSubClassAxiom
  (sub: types.ModelEntityDefinition,
   sup: types.ModelEntityAspect)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.EntityDefinitionAspectSubClassAxiom] =
    ax.find {
      case axiom: types.EntityDefinitionAspectSubClassAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    } match {
      case None =>
        for {
          axiom <- store.createOMFEntityDefinitionAspectSubClassAxiomInstance(
            this,
            EntityDefinitionAspectSubClassAxiom(sub, sup))
        } yield {
          ax += axiom
          axiom
        }
      case Some(ax) =>
        Failure(DuplicateModelTermAxiomException(EntityDefinitionAspectSubClassAxiomException, ax))
    }

  def addEntityDefinitionAspectSubClassAxiom
  (sub: types.ModelEntityDefinition,
   sup: types.ModelEntityAspect)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.EntityDefinitionAspectSubClassAxiom] =
    (isTypeTermDefinedRecursively(sub),
      isTypeTermDefinedRecursively(sup)) match {
      case (true, true) =>
        for {
          axiom <- createEntityDefinitionAspectSubClassAxiom(sub, sup)
        } yield {
          for {
            change <- Seq(
              new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(sub.e, sup.e))
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
        Failure(AxiomScopeException(EntityDefinitionAspectSubClassAxiomException, Map(Sub -> sub)))

      case (true, false) =>
        Failure(AxiomScopeException(EntityDefinitionAspectSubClassAxiomException, Map(Sup -> sub)))

      case (false, false) =>
        Failure(AxiomScopeException(EntityDefinitionAspectSubClassAxiomException, Map(Sub -> sub, Sup -> sub)))
    }

  def createEntityReifiedRelationshipSubClassAxiom
  (sub: types.ModelEntityReifiedRelationship,
   sup: types.ModelEntityReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.EntityReifiedRelationshipSubClassAxiom] =
    ax.find {
      case axiom: types.EntityReifiedRelationshipSubClassAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    } match {
      case None =>
        for {
          axiom <- store.createOMFEntityReifiedRelationshipSubClassAxiomInstance(
            this,
            EntityReifiedRelationshipSubClassAxiom(sub, sup))
        } yield {
          ax += axiom
          axiom
        }
      case Some(ax) =>
        Failure(DuplicateModelTermAxiomException(EntityReifiedRelationshipSubClassAxiomException, ax))
    }

  def addEntityReifiedRelationshipSubClassAxiom
  (sub: types.ModelEntityReifiedRelationship,
   sup: types.ModelEntityReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Try[types.EntityReifiedRelationshipSubClassAxiom] =
    (isTypeTermDefinedRecursively(sub), isTypeTermDefinedRecursively(sup)) match {
      case (true, true) =>
        for {
          axiom <- createEntityReifiedRelationshipSubClassAxiom(sub, sup)
        } yield {
          for {
            change <- Seq(
              new AddAxiom(ont,
                owlDataFactory.getOWLSubClassOfAxiom(sub.e, sup.e)),
              new AddAxiom(ont,
                owlDataFactory.getOWLSubObjectPropertyOfAxiom(sub.rSource, sup.rSource)),
              new AddAxiom(ont,
                owlDataFactory.getOWLSubObjectPropertyOfAxiom(sub.rTarget, sup.rTarget))
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
        Failure(AxiomScopeException(ReifiedRelationshipSubclassAxiomException, Map(Sub -> sub)))

      case (true, false) =>
        Failure(AxiomScopeException(ReifiedRelationshipSubclassAxiomException, Map(Sup -> sup)))

      case (false, false) =>
        Failure(AxiomScopeException(ReifiedRelationshipSubclassAxiomException, Map(Sub -> sub, Sup -> sup)))
    }
}