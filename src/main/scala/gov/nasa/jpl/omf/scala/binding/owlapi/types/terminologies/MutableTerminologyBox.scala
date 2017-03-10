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

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.{AnnotationEntry, AnnotationProperty, LexicalValue, LocalName}
import gov.nasa.jpl.omf.scala.binding.owlapi.AxiomExceptionKind
import gov.nasa.jpl.omf.scala.binding.owlapi.EntityExceptionKind
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{axiomScopeException, duplicateModelTermAxiomException, duplicateTerminologyGraphAxiomException, entityAlreadyDefinedException, entityConflictException, entityScopeException, terms}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics.RelationshipCharacteristics
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWLFacet

import scala.collection.JavaConversions._
import scala.compat.java8.StreamConverters._
import scala.collection.immutable.{Iterable, Map, Seq, Set}
import scala.{Any, Boolean, Int, None, Option, Some, StringContext}
import scala.Predef.{???, ArrowAssoc, String, require}
import scalaz._
import Scalaz._

trait MutableTerminologyBox extends TerminologyBox {

  import ops._

  override protected val aspects =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#Aspect]()

  override protected val concepts =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#Concept]()

  override protected val reifiedRelationships =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#ReifiedRelationship]()

  override protected val unreifiedRelationships =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#UnreifiedRelationship]()

  override protected val sc =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#Scalar]()

  override protected val st =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#Structure]()

  override protected val scalarOneOfRestrictions =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#ScalarOneOfRestriction]()

  override protected val binaryScalarRestrictions =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#BinaryScalarRestriction]()

  override protected val iriScalarRestrictions =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#IRIScalarRestriction]()

  override protected val numericScalarRestrictions =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#NumericScalarRestriction]()

  override protected val plainLiteralScalarRestrictions =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#PlainLiteralScalarRestriction]()

  override protected val stringScalarRestrictions =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#StringScalarRestriction]()

  override protected val synonymScalarRestrictions =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#SynonymScalarRestriction]()

  override protected val timeScalarRestrictions =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#TimeScalarRestriction]()

  override protected val e2sc =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#EntityScalarDataProperty]()

  override protected val e2st =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#EntityStructuredDataProperty]()

  override protected val s2sc =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#ScalarDataProperty]()

  override protected val s2st =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#StructuredDataProperty]()

  override protected val ax =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#Axiom]()

  override protected val gx =
    scala.collection.mutable.ListBuffer[OWLAPIOMF#TerminologyBoxAxiom]()

  override protected var cAxiom: Option[OWLAPIOMF#ConceptDesignationTerminologyAxiom] = None

  override protected val eAxioms =
    scala.collection.mutable.HashSet[OWLAPIOMF#TerminologyExtensionAxiom]()

  override protected var nAxiom: Option[OWLAPIOMF#TerminologyNestingAxiom] = None

  override protected val bAxioms =
    scala.collection.mutable.HashSet[OWLAPIOMF#BundledTerminologyAxiom]()

  override protected val rTAxioms =
    scala.collection.mutable.HashSet[OWLAPIOMF#RootConceptTaxonomyAxiom]()

  override protected val aTAxioms =
    scala.collection.mutable.HashSet[OWLAPIOMF#AnonymousConceptTaxonomyAxiom]()

  override protected val sTAxioms =
    scala.collection.mutable.HashSet[OWLAPIOMF#SpecificDisjointConceptAxiom]()

  override protected val annotations =
    scala.collection.mutable.Map[AnnotationProperty, Seq[AnnotationEntry]]()

  override protected val iri2typeTerm = scala.collection.mutable.HashMap[IRI, OWLAPIOMF#Term]()

  val LOG: Boolean = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.MutableTerminologyBox")

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: MutableTerminologyBox => true
    case _ => false
  }

  def addAnnotation
  (subject: OWLAPIOMF#TerminologyThing,
   property: AnnotationProperty,
   value: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ AnnotationEntry
  = {
    val a = AnnotationEntry(
      moduleUUID=uuid.toString,
      subjectUUID=subject.uuid.toString,
      value)
    annotations.update(property, annotations.getOrElse(property, Seq.empty) :+ a)
    a.right
  }

  def removeAnnotations
  (subject: OWLAPIOMF#TerminologyThing,
   property: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ Seq[AnnotationEntry]
  = {
    val sUUID = subject.uuid.toString
    val (keep, removed) = annotations.getOrElse(property, Seq.empty).partition(_.subjectUUID == sUUID)
    annotations.update(property, keep)
    removed.right
  }

  // Terminology Axioms

  def createTerminologyExtensionAxiom
  (extendedG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ TerminologyExtensionAxiom
  = for {
    uuid <- ops.terminologyExtensionUUID(this, extendedG)
    ax <- createTerminologyExtensionAxiom(uuid, extendedG)
  } yield ax

  def createTerminologyExtensionAxiom
  (uuid: UUID,
   extendedG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ TerminologyExtensionAxiom
  = eAxioms
    .find { _.extendedTerminology == extendedG }
    .fold[Set[java.lang.Throwable] \/ TerminologyExtensionAxiom](
    for {
      axiom <- store
        .createOMFTerminologyGraphDirectExtensionAxiom(uuid, this, extendedG)
    } yield {
      eAxioms += axiom
      gx += axiom
      axiom
    }
  ) { other =>
    Set(
      duplicateTerminologyGraphAxiomException(AxiomExceptionKind.TerminologyGraphDirectExtensionAxiomException, other)
    ).left
  }

  def addTerminologyGraphExtension
  (uuid: UUID,
   extendedG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ TerminologyExtensionAxiom
  = for {
    axiom <- createTerminologyExtensionAxiom(uuid, extendedG)
    _ <- applyOntologyChangeOrNoOp(ontManager,
      new AddImport(ont, owlDataFactory
        .getOWLImportsDeclaration(extendedG.iri)),
      "addTerminologyGraphExtension error")
  } yield axiom

  /**
    * Constructs an OMF Aspect as part of resolving its representation as an OWL Class
    * @param a An OWL Class representing an OMF Aspect
    * @param store
    * @return The OMF Aspect corresponding to its OWL Class `a` representation
    */
  def createModelEntityAspect
  (a: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Aspect
  = for {
    n <- getFragment(a.getIRI)
    u = generateUUID(fromIRI(a.getIRI))
    term <- createModelEntityAspect(a, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, a.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelEntityAspect
  (a: OWLClass, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Aspect
  = iri2typeTerm
    .get(a.getIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#Aspect] {
    val _a = terms.Aspect(a, name, uuid)
    aspects += _a
    iri2typeTerm += a.getIRI -> _a
    \/-(_a)
  } {
    case t: OWLAPIOMF#Aspect =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.EntityAspect, a.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.EntityAspect, a.getIRI, t)
      ).left
  }

  def addEntityAspect
  (aspectIRI: IRI, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Aspect
  = iri2typeTerm
    .get(aspectIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#Aspect]{
    val aspectC = owlDataFactory.getOWLClass(aspectIRI)
    for {
      result <- createModelEntityAspect(aspectC, name, uuid)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(aspectC)),
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(aspectC, backbone.AspectC))),
        "addAspect Error")
    } yield result
  } {
    case t: OWLAPIOMF#Aspect =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.EntityAspect, aspectIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.EntityAspect, aspectIRI, t)
      ).left
  }

  /**
    * Constructs an OMF Concept as part of resolving its representation as an OWL Class
    * @param c An OWL Class representing an OMF Concept
    * @param store
    * @return The OMF Concept corresponding to its OWL Class `c` representation
    */
  def createModelEntityConcept
  (c: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Concept
  = for {
    n <- getFragment(c.getIRI)
    u = generateUUID(fromIRI(c.getIRI))
    term <- createModelEntityConcept(c, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, c.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelEntityConcept
  (c: OWLClass, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Concept
  = iri2typeTerm
    .get(c.getIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#Concept]{
    val _c = terms.Concept(c, name, uuid)
    concepts += _c
    iri2typeTerm += c.getIRI -> _c
    \/-(_c)
  } {
    case t: Concept =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.EntityConcept, c.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.EntityConcept, c.getIRI, t)
      ).left
  }

  def addEntityConcept
  (conceptIRI: IRI, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Concept
  = iri2typeTerm
    .get(conceptIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#Concept] {
    val conceptC = owlDataFactory.getOWLClass(conceptIRI)
    for {
      result <- createModelEntityConcept(conceptC, name, uuid)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont,
            owlDataFactory
              .getOWLDeclarationAxiom(conceptC)),
          new AddAxiom(ont,
            owlDataFactory
              .getOWLSubClassOfAxiom(conceptC, backbone.EntityC))),
        "addConcept Error")
    } yield result
  } {
    case t: OWLAPIOMF#Concept =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.EntityConcept, conceptIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.EntityConcept, conceptIRI, t)
      ).left
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
  (r: OWLClass,
   u: OWLObjectProperty, ui: Option[OWLObjectProperty],
   source: Entity, rSource: OWLObjectProperty,
   target: Entity, rTarget: OWLObjectProperty,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ReifiedRelationship
  = for {
    rn <- getFragment(r.getIRI)
    ru = generateUUID(fromIRI(r.getIRI))
    un <- getFragment(u.getIRI)
    in <- ui.fold[Set[java.lang.Throwable] \/ Option[String]](None.right) { i =>
      getFragment(i.getIRI).map(Some(_))
    }
    term <- createEntityReifiedRelationship(
      r, rn, ru,
      un, u, in, ui,
      source, rSource, target, rTarget, characteristics)
    aas = getRelevantSubjectAnnotationAssertions(ont, r.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createEntityReifiedRelationship
  (r: OWLClass, name: LocalName, uuid: UUID,
   unreifiedPropertyName: LocalName, u: OWLObjectProperty,
   inversePropertyName: Option[LocalName], ui: Option[OWLObjectProperty],
   source: Entity, rSource: OWLObjectProperty,
   target: Entity, rTarget: OWLObjectProperty,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ReifiedRelationship
  = iri2typeTerm
    .get(r.getIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#ReifiedRelationship] {
    val _r = terms.ReifiedRelationship(r, name, uuid,
      unreifiedPropertyName, u, inversePropertyName, ui,
      source, rSource, target, rTarget, characteristics)
    reifiedRelationships += _r
    iri2typeTerm += r.getIRI -> _r
    \/-(_r)
  } {
    case t: OWLAPIOMF#ReifiedRelationship =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.EntityReifiedRelationship, r.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.EntityReifiedRelationship, r.getIRI, t)
      ).left
  }

  protected def makeEntityReifiedRelationship
  (rIRI: IRI,
   name: LocalName,
   unreifiedRelationshipName: LocalName,
   unreifiedInverseRelationshipName: Option[LocalName],
   uuid: UUID,
   rIRISource: IRI, rIRITarget: IRI,
   uIRI: IRI, uiIRI: Option[IRI],
   source: Entity, target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ReifiedRelationship
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

      rule = owlDataFactory.getSWRLRule(Set(body1, body2), Set(head))

      _ <-
      applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            r)),
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
            r, backbone.ReifiedObjectPropertyC)),

          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            rSource)),
          new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
            rSource, backbone.topReifiedObjectPropertySourceOP)),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
            rSource, r)),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
            rSource, sourceC)),
          new AddAxiom(ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom(
            rSource)),

          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            rTarget)),
          new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
            rTarget, backbone.topReifiedObjectPropertyTargetOP)),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
            rTarget, r)),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
            rTarget, targetC)),
          new AddAxiom(ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom(
            rTarget)),

          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            u)),
          new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
            u, backbone.topReifiedObjectPropertyOP)),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
            u, sourceC)),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
            u, targetC)),

          new AddAxiom(ont, rule)
        ) ++
          ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
                _ui)),
              new AddAxiom(ont, owlDataFactory.getOWLAnnotationAssertionAxiom(isDerivedAP,
                _ui.getIRI, owlDataFactory.getOWLLiteral(true))),
              new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
                _ui, backbone.topReifiedObjectPropertyOP)),
              new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
                _ui, targetC)),
              new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
                _ui, sourceC)),
              new AddAxiom(ont, owlDataFactory.getOWLInverseObjectPropertiesAxiom(u, _ui))
            )
          } ++
          (if (characteristics.contains(RelationshipCharacteristics.isFunctional))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLInverseFunctionalObjectPropertyAxiom(
                rSource)),
              new AddAxiom(ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom(
                u))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLInverseFunctionalObjectPropertyAxiom(
                  _ui))
              )
            }
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isInverseFunctional))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLInverseFunctionalObjectPropertyAxiom(
                rTarget)),
              new AddAxiom(ont, owlDataFactory.getOWLInverseFunctionalObjectPropertyAxiom(
                u))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom(
                  _ui))
              )
            }
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isSymmetric))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLSymmetricObjectPropertyAxiom(
                u))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSymmetricObjectPropertyAxiom(
                  _ui))
              )
            }
            else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isAsymmetric))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLAsymmetricObjectPropertyAxiom(
                u))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLAsymmetricObjectPropertyAxiom(
                  _ui))
              )
            }
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isReflexive))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLReflexiveObjectPropertyAxiom(
                u))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLReflexiveObjectPropertyAxiom(
                  _ui))
              )
            }
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isIrreflexive))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLIrreflexiveObjectPropertyAxiom(
                u))
            ) ++ ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLIrreflexiveObjectPropertyAxiom(
                  _ui))
              )
            }
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isEssential))
            Seq(
              new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                sourceC, owlDataFactory.getOWLObjectExactCardinality(1, u, targetC)))
            )
          else
            Seq.empty) ++
          (if (characteristics.contains(RelationshipCharacteristics.isInverseEssential))
            ui.fold[Seq[OWLOntologyChange]](Seq.empty) { _ui =>
              Seq(
                new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(
                  targetC, owlDataFactory.getOWLObjectExactCardinality(1, _ui, sourceC)))
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
   uuid: UUID,
   rIRISource: IRI, rIRITarget: IRI,
   uIRI: IRI, uiIRI: Option[IRI],
   source: Entity, target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ReifiedRelationship
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
            entityScopeException(EntityExceptionKind.EntityReifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Source -> source))
          ).left

        case (true, false) =>
          Set(
            entityScopeException(EntityExceptionKind.EntityReifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Target -> target))
          ).left

        case (false, false) =>
          Set(
            entityScopeException(EntityExceptionKind.EntityReifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
          ).left
      }

    case (Some(t), _, _, _, _) =>
      Set(
        entityConflictException(EntityExceptionKind.EntityReifiedRelationship, rIRI, t)
      ).left

    case (_, Some(t), _, _, _) =>
      Set(
        entityConflictException(EntityExceptionKind.EntityReifiedRelationship, rIRISource, t)
      ).left

    case (_, _, Some(t), _, _) =>
      Set(
        entityConflictException(EntityExceptionKind.EntityReifiedRelationship, rIRITarget, t)
      ).left

    case (_, _, _, Some(t), _) =>
      Set(
        entityConflictException(EntityExceptionKind.EntityReifiedRelationship, uIRI, t)
      ).left

    case (_, _, _, _, Some(t)) =>
      require(uiIRI.isDefined)
      Set(
        entityConflictException(EntityExceptionKind.EntityReifiedRelationship, uiIRI.get, t)
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
  (r: OWLObjectProperty,
   source: Entity,
   target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#UnreifiedRelationship
  = for {
    rn <- getFragment(r.getIRI)
    ru = generateUUID(fromIRI(r.getIRI))
    term <- createEntityUnreifiedRelationship(
      r, rn, ru, source, target, characteristics)
    aas = getRelevantSubjectAnnotationAssertions(ont, r.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createEntityUnreifiedRelationship
  (r: OWLObjectProperty, name: LocalName, uuid: UUID,
   source: Entity,
   target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#UnreifiedRelationship
  = iri2typeTerm
    .get(r.getIRI)
    .fold[Set[java.lang.Throwable] \/ UnreifiedRelationship] {
    val _r = terms.UnreifiedRelationship(r, name, uuid, source, target, characteristics)
    unreifiedRelationships += _r
    iri2typeTerm += r.getIRI -> _r
    \/-(_r)
  } {
    case t: OWLAPIOMF#UnreifiedRelationship =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.EntityUnreifiedRelationship, r.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.EntityUnreifiedRelationship, r.getIRI, t)
      ).left
  }

  protected def makeUnreifiedRelationship
  (rIRI: IRI, name: LocalName, uuid: UUID,
   source: Entity, target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#UnreifiedRelationship
  = {
    val sourceC = owlDataFactory.getOWLClass(source.iri)
    val targetC = owlDataFactory.getOWLClass(target.iri)
    val r = owlDataFactory.getOWLObjectProperty(rIRI)

    for {
      result <- createEntityUnreifiedRelationship(r, name, uuid,
        source,
        target,
        characteristics)
    } yield {

      for {
        change <-
        Seq(
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(
            r)),
          new AddAxiom(ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom(
            r, backbone.topUnreifiedObjectPropertyOP)),

          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyDomainAxiom(
            r, sourceC)),
          new AddAxiom(ont, owlDataFactory.getOWLObjectPropertyRangeAxiom(
            r, targetC))
        )
      } {
        val result = ontManager.applyChange(change)
        require(
          result == ChangeApplied.SUCCESSFULLY || result == ChangeApplied.NO_OPERATION,
          s"\nmakeUnreifiedRelationship (result=$result):\n$change")
      }

      result
    }

  }

  def addEntityUnreifiedRelationship
  (rIRI: IRI, name: LocalName, uuid: UUID,
   source: Entity, target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#UnreifiedRelationship
  = lookupTerm(rIRI, recursively = true) match {
    case None =>
      (isTypeTermDefinedRecursively(source), isTypeTermDefinedRecursively(target)) match {
        case (true, true) =>
          makeUnreifiedRelationship(rIRI, name, uuid, source, target, characteristics)
        case (false, true) =>
          Set(
            entityScopeException(EntityExceptionKind.EntityUnreifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Source -> source))
          ).left

        case (true, false) =>
          Set(
            entityScopeException(EntityExceptionKind.EntityUnreifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Target -> target))
          ).left

        case (false, false) =>
          Set(
            entityScopeException(EntityExceptionKind.EntityUnreifiedRelationship, rIRI,
              Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
          ).left
      }

    case Some(t) =>
      Set(
        entityConflictException(EntityExceptionKind.EntityUnreifiedRelationship, rIRI, t)
      ).left
  }

  override def getEntityDefinitionMap
  : Map[OWLClass, OWLAPIOMF#Entity]
  = (aspects.map(a => a.e -> a) ++
    concepts.map(c => c.e -> c) ++
    reifiedRelationships.map(r => r.e -> r)).toMap

  override def getScalarDatatypeDefinitionMap
  : Map[OWLDatatype, OWLAPIOMF#Scalar]
  = sc
    .map(t => t.e -> t)
    .toMap

  /**
    * Constructs an OMF Scalar as part of resolving its representation as an OWL Datatype
    * @param dt
    * @param store
    * @return
    */
  def createModelScalarDataType
  (dt: OWLDatatype)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Scalar
  = for {
    n <- getFragment(dt.getIRI)
    u = generateUUID(fromIRI(dt.getIRI))
    term <- createModelScalarDataType(dt, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, dt.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelScalarDataType
  (dt: OWLDatatype, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Scalar
  = iri2typeTerm
    .get(dt.getIRI)
    .fold[Set[java.lang.Throwable] \/ Scalar]{
    val _dt = terms.Scalar(dt, name, uuid)
    sc += _dt
    iri2typeTerm += dt.getIRI -> _dt
    store.registerOMFModelScalarDataTypeInstance(this, _dt).map(_ => _dt)
  } {
    case t: Scalar =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.ScalarDataType, dt.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.ScalarDataType, dt.getIRI, t)
      ).left
  }

  def addScalarDataType
  (scalarIRI: IRI, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Scalar
  = iri2typeTerm
    .get(scalarIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#Scalar] {
    val scalarDT = owlDataFactory.getOWLDatatype(scalarIRI)
    for {
      result <- createModelScalarDataType(scalarDT, name, uuid)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont,
            owlDataFactory
              .getOWLDeclarationAxiom(scalarDT))
        ),
        "addScalarDataType error")
    } yield result
  } {
    case t: OWLAPIOMF#Scalar =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.ScalarDataType, scalarIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.ScalarDataType, scalarIRI, t)
      ).left
  }

  /**
    * Constructs an OMF Structure as part of resolving its representation as an OWL Class
    * @param dt
    * @param store
    * @return
    */
  def createModelStructuredDataType
  (dt: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Structure
  = for {
    n <- getFragment(dt.getIRI)
    u = generateUUID(fromIRI(dt.getIRI))
    term <- createModelStructuredDataType(dt, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, dt.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelStructuredDataType
  (dt: OWLClass, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Structure
  = iri2typeTerm
    .get(dt.getIRI)
    .fold[Set[java.lang.Throwable] \/ Structure]{
    val _st = terms.Structure(dt, name, uuid)
    st += _st
    iri2typeTerm += dt.getIRI -> _st
    \/-(_st)
  } {
    case t: Structure =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.StructuredDataType, dt.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.StructuredDataType, dt.getIRI, t)
      ).left
  }

  def addStructuredDataType
  (structuredDataTypeIRI: IRI, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#Structure
  = iri2typeTerm
    .get(structuredDataTypeIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#Structure]{
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
              .getOWLSubClassOfAxiom(structuredDataTypeC, backbone.StructuredDatatypeC))
        ),
        "addStructuredDataType error")
    } yield result
  } {
    case t: OWLAPIOMF#Structure =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.StructuredDataType, structuredDataTypeIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.StructuredDataType, structuredDataTypeIRI, t)
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
  (restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ScalarOneOfRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(fromIRI(restrictionDT.getIRI))
    term <- createScalarOneOfRestriction(
      restrictionDT, n, u, restrictedRange)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createScalarOneOfRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ScalarOneOfRestriction
  = iri2typeTerm
    .get(restrictionDT.getIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#ScalarOneOfRestriction]{
    val _rdt = terms.ScalarOneOfRestriction(
      restrictionDT, uuid, name, restrictedRange)
    scalarOneOfRestrictions += _rdt
    iri2typeTerm += restrictionDT.getIRI -> _rdt
    store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
  } {
    case t: OWLAPIOMF#ScalarOneOfRestriction =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.ScalarOneOfRestriction, restrictionDT.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.ScalarOneOfRestriction, restrictionDT.getIRI, t)
      ).left
  }

  def addScalarOneOfRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ScalarOneOfRestriction
  = iri2typeTerm
    .get(dataTypeIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#ScalarOneOfRestriction]{
    val restrictionDT = owlDataFactory
      .getOWLDatatype(dataTypeIRI)
    for {
      result <- createScalarOneOfRestriction(restrictionDT, name, uuid, restrictedRange)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ),
        "addScalarOneOfRestriction error")
    } yield {
      iri2typeTerm += restrictionDT.getIRI -> result
      result
    }
  } {
    case t: OWLAPIOMF#ScalarOneOfRestriction =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.ScalarOneOfRestriction, dataTypeIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.ScalarOneOfRestriction, dataTypeIRI, t)
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
   value: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ScalarOneOfLiteralAxiom
  = for {
    axiomUUID <- scalarOneOfLiteralAxiomUUID(this, scalarOneOfRestriction, value)
    ax <- createScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value)
  } yield ax

  def createScalarOneOfLiteralAxiom
  (axiomUUID: UUID,
   scalarOneOfRestriction: OWLAPIOMF#ScalarOneOfRestriction,
   value: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ScalarOneOfLiteralAxiom
  = types.termAxioms.ScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value).right

  def addScalarOneOfLiteralAxiom
  (axiomUUID: UUID,
   scalarOneOfRestriction: OWLAPIOMF#ScalarOneOfRestriction,
   value: String)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ScalarOneOfLiteralAxiom
  = for {
    axiom <- createScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value)
    restrictionDT = scalarOneOfRestriction.e
    restrictedDT = scalarOneOfRestriction.restrictedDataRange.e
    result <- ont
      .datatypeDefinitions(restrictionDT)
      .toScala[Set]
      .headOption
      .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#ScalarOneOfLiteralAxiom]{
      for {
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(ont, owlDataFactory
              .getOWLDatatypeDefinitionAxiom(
                restrictionDT,
                owlDataFactory.getOWLDataOneOf(owlDataFactory.getOWLLiteral(value, restrictedDT))))
          ),
          "addScalarOneOfLiteralAxiom error")
      } yield axiom
    } { restrictionDF =>
      restrictionDF.getDataRange match {
        case dof: OWLDataOneOf =>
          val values = java.util.stream.Stream.concat(
            dof.values,
            java.util.stream.Stream.of[OWLLiteral](owlDataFactory.getOWLLiteral(value, restrictedDT)))
          for {
            _ <- applyOntologyChanges(ontManager,
              Seq(
                new RemoveAxiom(ont, restrictionDF),
                new AddAxiom(ont, owlDataFactory
                  .getOWLDatatypeDefinitionAxiom(
                    restrictionDT,
                    owlDataFactory.getOWLDataOneOf(values)))
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
  (restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#BinaryScalarRestriction
  = for {
      n <- getFragment(restrictionDT.getIRI)
      u = generateUUID(fromIRI(restrictionDT.getIRI))
      term <- createBinaryScalarRestriction(
        restrictionDT, n, u, restrictedRange,
        length, minLength, maxLength)
      aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
      _ <- store.ops.addAnnotationAssertions(this, term, aas)
    } yield term

  def createBinaryScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#BinaryScalarRestriction
  = if (store.isBinaryKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#BinaryScalarRestriction] {
      val _rdt = terms.BinaryScalarRestriction(
        restrictionDT, uuid, name, restrictedRange,
        length, minLength, maxLength)
      binaryScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#BinaryScalarRestriction =>
        Set(
          entityAlreadyDefinedException(EntityExceptionKind.BinaryScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityExceptionKind.BinaryScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createBinaryScalarRestriction: restricted data range must be binary per OWL2 section 4.5: $restrictedRange")
    ).left

  def addBinaryScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#BinaryScalarRestriction
  = iri2typeTerm
    .get(dataTypeIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#BinaryScalarRestriction]{
    val restrictionDT = owlDataFactory
      .getOWLDatatype(dataTypeIRI)
    for {
      result <- createBinaryScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        length, minLength, maxLength)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ) ++ length.map { l =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.LENGTH, l))))
        } ++ minLength.map { minL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.MIN_LENGTH, minL))))
        } ++ maxLength.map { maxL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, maxL))))
        },
        "addBinaryScalarRestriction error")
    } yield result
  } {
    case t: OWLAPIOMF#BinaryScalarRestriction =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.BinaryScalarRestriction, dataTypeIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.BinaryScalarRestriction, dataTypeIRI, t)
      ).left
  }

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
  (restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#IRIScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(fromIRI(restrictionDT.getIRI))
    term <- createIRIScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      length, minLength, maxLength, pattern)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createIRIScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#IRIScalarRestriction
  = if (store.isIRIKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#IRIScalarRestriction]{
      val _rdt = terms.IRIScalarRestriction(
        restrictionDT, uuid, name, restrictedRange,
        length, minLength, maxLength, pattern)
      iriScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#IRIScalarRestriction =>
        Set(
          entityAlreadyDefinedException(EntityExceptionKind.IRIScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityExceptionKind.IRIScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createBinaryScalarRestriction: restricted data range must be IRI per OWL2 section 4.6: $restrictedRange")
    ).left

  def addIRIScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#IRIScalarRestriction
  = iri2typeTerm
    .get(dataTypeIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#IRIScalarRestriction]{
    val restrictionDT = owlDataFactory
      .getOWLDatatype(dataTypeIRI)
    for {
      result <- createIRIScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        length, minLength, maxLength, pattern)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ) ++ length.map { l =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.LENGTH, l))))
        } ++ minLength.map { minL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.MIN_LENGTH, minL))))
        } ++ maxLength.map { maxL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, maxL))))
        } ++ pattern.map { patt =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.PATTERN, owlDataFactory.getOWLLiteral(patt)))))
        },
        "addIRIScalarRestriction error")
    } yield result
  } {
    case t: OWLAPIOMF#IRIScalarRestriction =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.IRIScalarRestriction, dataTypeIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.IRIScalarRestriction, dataTypeIRI, t)
      ).left
  }

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
  (restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[String],
   maxInclusive: Option[String],
   minExclusive: Option[String],
   maxExclusive: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#NumericScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(fromIRI(restrictionDT.getIRI))
    term <- createNumericScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      minInclusive, maxInclusive, minExclusive, maxExclusive)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createNumericScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[String],
   maxInclusive: Option[String],
   minExclusive: Option[String],
   maxExclusive: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#NumericScalarRestriction
  = if (store.isNumericKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#NumericScalarRestriction] {
      val _rdt = terms.NumericScalarRestriction(
        restrictionDT, uuid, name, restrictedRange,
        minInclusive, maxInclusive, minExclusive, maxExclusive)
      numericScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#NumericScalarRestriction =>
        Set(
          entityAlreadyDefinedException(EntityExceptionKind.NumericScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityExceptionKind.NumericScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createBinaryScalarRestriction: restricted data range must be numeric per OWL2 sections 4.1, 4.2: $restrictedRange")
    ).left

  def addNumericScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[String],
   maxInclusive: Option[String],
   minExclusive: Option[String],
   maxExclusive: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#NumericScalarRestriction
  = iri2typeTerm
    .get(dataTypeIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#NumericScalarRestriction]{
    val restrictionDT = owlDataFactory
      .getOWLDatatype(dataTypeIRI)
    for {
      result <- createNumericScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        minInclusive, maxInclusive, minExclusive, maxExclusive)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ) ++ minInclusive.map { minI =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MIN_INCLUSIVE,
                  owlDataFactory.getOWLLiteral(minI)))))
        } ++ maxInclusive.map { maxI =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MAX_INCLUSIVE,
                  owlDataFactory.getOWLLiteral(maxI)))))
        } ++ minExclusive.map { patt =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MIN_EXCLUSIVE,
                  owlDataFactory.getOWLLiteral(patt)))))
        } ++ maxExclusive.map { patt =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MAX_EXCLUSIVE,
                  owlDataFactory.getOWLLiteral(patt)))))
        },
        "addNumericScalarRestriction error")
    } yield result
  } {
    case t: OWLAPIOMF#NumericScalarRestriction =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.NumericScalarRestriction, dataTypeIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.NumericScalarRestriction, dataTypeIRI, t)
      ).left
  }

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
  (restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String],
   language: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#PlainLiteralScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(fromIRI(restrictionDT.getIRI))
    term <- createPlainLiteralScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      length, minLength, maxLength, pattern, language)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createPlainLiteralScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String],
   language: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#PlainLiteralScalarRestriction
  = if (store.isPlainLiteralKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#PlainLiteralScalarRestriction] {
      val _rdt = terms.PlainLiteralScalarRestriction(
        restrictionDT, uuid, name, restrictedRange,
        length, minLength, maxLength, pattern, language)
      plainLiteralScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#PlainLiteralScalarRestriction =>
        Set(
          entityAlreadyDefinedException(EntityExceptionKind.PlainLiteralScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityExceptionKind.PlainLiteralScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createBinaryScalarRestriction: restricted data range must be plain literal per OWL2 section 4.3: $restrictedRange")
    ).left

  def addPlainLiteralScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String],
   language: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#PlainLiteralScalarRestriction
  = iri2typeTerm
    .get(dataTypeIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#PlainLiteralScalarRestriction]{
    val restrictionDT = owlDataFactory
      .getOWLDatatype(dataTypeIRI)
    for {
      result <- createPlainLiteralScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        length, minLength, maxLength, pattern, language)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ) ++ length.map { l =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.LENGTH, l))))
        } ++ minLength.map { minL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.MIN_LENGTH, minL))))
        } ++ maxLength.map { maxL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, maxL))))
        } ++ pattern.map { patt =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.PATTERN,
                  owlDataFactory.getOWLLiteral(patt)))))
        } ++ language.map { lang =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.LANG_RANGE,
                  owlDataFactory.getOWLLiteral(lang)))))
        },
        "addPlainLiteralScalarRestriction error")
    } yield result
  } {
    case t: OWLAPIOMF#PlainLiteralScalarRestriction =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.PlainLiteralScalarRestriction, dataTypeIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.PlainLiteralScalarRestriction, dataTypeIRI, t)
      ).left
  }

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
  (restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#StringScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(fromIRI(restrictionDT.getIRI))
    term <- createStringScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      length, minLength, maxLength, pattern)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createStringScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#StringScalarRestriction
  = if (store.isStringKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#StringScalarRestriction] {
      val _rdt = terms.StringScalarRestriction(
        restrictionDT, uuid, name, restrictedRange,
        length, minLength, maxLength, pattern)
      stringScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#StringScalarRestriction =>
        Set(
          entityAlreadyDefinedException(EntityExceptionKind.StringScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityExceptionKind.StringScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createBinaryScalarRestriction: restricted data range must be string per OWL2 section 4.3: $restrictedRange")
    ).left

  def addStringScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[Int],
   minLength: Option[Int],
   maxLength: Option[Int],
   pattern: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#StringScalarRestriction
  = iri2typeTerm
    .get(dataTypeIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#StringScalarRestriction]{
    val restrictionDT = owlDataFactory
      .getOWLDatatype(dataTypeIRI)
    for {
      result <- createStringScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        length, minLength, maxLength, pattern)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ) ++ length.map { l =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.LENGTH, l))))
        } ++ minLength.map { minL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.MIN_LENGTH, minL))))
        } ++ maxLength.map { maxL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, maxL))))
        } ++ pattern.map { patt =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.PATTERN,
                  owlDataFactory.getOWLLiteral(patt)))))
        },
      "addStringScalarRestriction error")
    } yield result
  } {
    case t: OWLAPIOMF#StringScalarRestriction =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.StringScalarRestriction, dataTypeIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.StringScalarRestriction, dataTypeIRI, t)
      ).left
  }

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
  (restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#SynonymScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(fromIRI(restrictionDT.getIRI))
    term <- createSynonymScalarRestriction(
      restrictionDT, n, u, restrictedRange)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createSynonymScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#SynonymScalarRestriction
  = iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#SynonymScalarRestriction] {
    val _rdt = terms.SynonymScalarRestriction(
      restrictionDT, uuid, name, restrictedRange)
    synonymScalarRestrictions += _rdt
    iri2typeTerm += restrictionDT.getIRI -> _rdt
    store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
  } {
    case t: OWLAPIOMF#SynonymScalarRestriction =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.SynonymScalarRestriction, restrictionDT.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.SynonymScalarRestriction, restrictionDT.getIRI, t)
      ).left
  }

  def addSynonymScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#SynonymScalarRestriction
  = iri2typeTerm
    .get(dataTypeIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#SynonymScalarRestriction]{
    val restrictionDT = owlDataFactory
      .getOWLDatatype(dataTypeIRI)
    for {
      result <- createSynonymScalarRestriction(
        restrictionDT, name, uuid, restrictedRange)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ),
        "addSynonymScalarRestriction error")
    } yield result
  } {
    case t: OWLAPIOMF#SynonymScalarRestriction =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.SynonymScalarRestriction, dataTypeIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.SynonymScalarRestriction, dataTypeIRI, t)
      ).left
  }

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
  (restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[String],
   maxInclusive: Option[String],
   minExclusive: Option[String],
   maxExclusive: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#TimeScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(fromIRI(restrictionDT.getIRI))
    term <- createTimeScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      minInclusive, maxInclusive, minExclusive, maxExclusive)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createTimeScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[String],
   maxInclusive: Option[String],
   minExclusive: Option[String],
   maxExclusive: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#TimeScalarRestriction
  = if (store.isTimeKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#TimeScalarRestriction] {
      val _rdt = terms.TimeScalarRestriction(
        restrictionDT, uuid, name, restrictedRange,
        minInclusive, maxInclusive, minExclusive, maxExclusive)
      timeScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#TimeScalarRestriction =>
        Set(
          entityAlreadyDefinedException(EntityExceptionKind.TimeScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityExceptionKind.TimeScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createTimeScalarRestriction: restricted data range must be time per OWL2 section 4.7: $restrictedRange")
    ).left

  def addTimeScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[String],
   maxInclusive: Option[String],
   minExclusive: Option[String],
   maxExclusive: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#TimeScalarRestriction
  = iri2typeTerm
    .get(dataTypeIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#TimeScalarRestriction]{
    val restrictionDT = owlDataFactory
      .getOWLDatatype(dataTypeIRI)
    for {
      result <- createTimeScalarRestriction(
        restrictionDT, name, uuid, restrictedRange,
        minInclusive, maxInclusive, minExclusive, maxExclusive)
      _ <- applyOntologyChanges(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ) ++ minInclusive.map { minI =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MIN_INCLUSIVE,
                  owlDataFactory.getOWLLiteral(minI)))))
        } ++ maxInclusive.map { maxI =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MAX_INCLUSIVE,
                  owlDataFactory.getOWLLiteral(maxI)))))
        } ++ minExclusive.map { minE =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MIN_EXCLUSIVE,
                  owlDataFactory.getOWLLiteral(minE)))))
        } ++ maxExclusive.map { maxE =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                restrictedRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MAX_EXCLUSIVE,
                  owlDataFactory.getOWLLiteral(maxE)))))
        },
        "addTimeScalarRestriction error")
    } yield result
  } {
    case t: OWLAPIOMF#TimeScalarRestriction =>
      Set(
        entityAlreadyDefinedException(EntityExceptionKind.TimeScalarRestriction, dataTypeIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(EntityExceptionKind.TimeScalarRestriction, dataTypeIRI, t)
      ).left
  }

  def createDataRelationshipFromEntityToScalar
  (esc: OWLDataProperty,
   isIdentityCriteria: Boolean,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityScalarDataProperty
  = for {
    n <- getFragment(esc.getIRI)
    u = generateUUID(fromIRI(esc.getIRI))
    term <- createDataRelationshipFromEntityToScalar(esc, n, isIdentityCriteria, u, source, target)
  } yield term

  def createDataRelationshipFromEntityToScalar
  (esc: OWLDataProperty, name: LocalName, isIdentityCriteria: Boolean, uuid: UUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityScalarDataProperty
  = {
    val escIRI: IRI = esc.getIRI
    iri2typeTerm
      .get(escIRI)
      .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#EntityScalarDataProperty](
      for {
        _esc <- store.registerDataRelationshipFromEntityToScalar(
          this, terms.EntityScalarDataProperty(esc, name, isIdentityCriteria, uuid, source, target))
      } yield {
        e2sc += _esc
        iri2typeTerm += escIRI -> _esc
        _esc
      }) {
      case t: OWLAPIOMF#EntityScalarDataProperty =>
        Set(
          entityAlreadyDefinedException(EntityExceptionKind.DataRelationshipFromEntityToScalar, escIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(EntityExceptionKind.DataRelationshipFromEntityToScalar, escIRI, t)
        ).left
    }
  }

  protected def makeDataRelationshipFromEntityToScalar
  (dIRI: IRI, name: LocalName, isIdentityCriteria: Boolean, uuid: UUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityScalarDataProperty
  = {
    val escDP = owlDataFactory.getOWLDataProperty(dIRI)
    for {
      term <- createDataRelationshipFromEntityToScalar(escDP, name, isIdentityCriteria, uuid, source, target)
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
      term
    }
  }

  def addDataRelationshipFromEntityToScalar
  (dIRI: IRI, name: LocalName, isIdentityCriteria: Boolean, uuid: UUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ EntityScalarDataProperty
  = iri2typeTerm
    .get(dIRI)
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#EntityScalarDataProperty]({
    (isTypeTermDefinedRecursively(source),
      isTypeTermDefinedRecursively(target)) match {
      case (true, true) =>
        makeDataRelationshipFromEntityToScalar(dIRI, name, isIdentityCriteria, uuid, source, target)
      case (false, true) =>
        Set(
          entityScopeException(EntityExceptionKind.DataRelationshipFromEntityToScalar, dIRI,
            Map(RelationshipScopeAccessKind.Source -> source))
        ).left
      case (true, false) =>
        Set(
          entityScopeException(EntityExceptionKind.DataRelationshipFromEntityToScalar, dIRI,
            Map(RelationshipScopeAccessKind.Target -> target))
        ).left
      case (false, false) =>
        Set(
          entityScopeException(EntityExceptionKind.DataRelationshipFromEntityToScalar, dIRI,
            Map(RelationshipScopeAccessKind.Source -> source, RelationshipScopeAccessKind.Target -> target))
        ).left
    }
  }) { term =>
    Set(
      entityConflictException(EntityExceptionKind.DataRelationshipFromEntityToScalar, dIRI, term)
    ).left
  }

  def addDataRelationshipFromEntityToStructure
  (dIRI: IRI, name: LocalName, isIdentityCriteria: Boolean, uuid: UUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Structure)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityStructuredDataProperty
  = ???

  def addDataRelationshipFromStructureToScalar
  (dIRI: IRI, name: LocalName, uuid: UUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#DataRange)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ScalarDataProperty
  = ???

  def addDataRelationshipFromStructureToStructure
  (dIRI: IRI, name: LocalName, uuid: UUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#Structure)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#StructuredDataProperty
  = ???

  def createEntityConceptSubClassAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#Concept,
   sup: OWLAPIOMF#Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ConceptSpecializationAxiom
  = ax
    .find {
      case axiom: ConceptSpecializationAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#ConceptSpecializationAxiom](
    for {
      axiom <- store
        .registerOMFEntityConceptSubClassAxiomInstance(this,
          ConceptSpecializationAxiom(uuid, sub, sup))
    } yield {
      ax += axiom
      axiom
    }
  ) { other =>
    Set(
      duplicateModelTermAxiomException(AxiomExceptionKind.EntityConceptSubClassAxiomException, other)
    ).left
  }

  def addEntityConceptSubClassAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#Concept,
   sup: OWLAPIOMF#Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ConceptSpecializationAxiom
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

  def addEntityDefinitionUniversalRestrictionAxiom
  (sub: OWLAPIOMF#Entity,
   rel: OWLAPIOMF#ReifiedRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityUniversalRestrictionAxiom
  = for {
    uuid <- entityUniversalRestrictionAxiomUUID(this, sub, rel, range)
    ax <- addEntityDefinitionUniversalRestrictionAxiom(uuid, sub, rel, range)
  } yield ax

  def addEntityDefinitionUniversalRestrictionAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#Entity,
   rel: OWLAPIOMF#ReifiedRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityUniversalRestrictionAxiom
  = (isTypeTermDefinedRecursively(sub),
    isTypeTermDefinedRecursively(rel),
    isTypeTermDefinedRecursively(range)) match {
    case (true, true, true) =>
      val subC = owlDataFactory.getOWLClass(sub.iri)
      val rangeC = owlDataFactory.getOWLClass(range.iri)
      for {
        axiom <-
        store
          .registerOMFEntityDefinitionUniversalRestrictionAxiomInstance(this,
            EntityUniversalRestrictionAxiom(uuid, sub, rel, range))
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
   rel: OWLAPIOMF#ReifiedRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityExistentialRestrictionAxiom
  = for {
    uuid <- entityExistentialRestrictionAxiomUUID(this, sub, rel, range)
    ax <- addEntityDefinitionExistentialRestrictionAxiom(uuid, sub, rel, range)
  } yield ax

  def addEntityDefinitionExistentialRestrictionAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#Entity,
   rel: OWLAPIOMF#ReifiedRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityExistentialRestrictionAxiom
  = (isTypeTermDefinedRecursively(sub),
    isTypeTermDefinedRecursively(rel),
    isTypeTermDefinedRecursively(range)) match {
    case (true, true, true) =>
      val subC = owlDataFactory.getOWLClass(sub.iri)
      val rangeC = owlDataFactory.getOWLClass(range.iri)
      for {
        axiom <-
        store
          .registerOMFEntityDefinitionExistentialRestrictionAxiomInstance(this,
            EntityExistentialRestrictionAxiom(uuid, sub, rel, range))
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

  def addEntityScalarDataPropertyExistentialRestrictionAxiom
  (uuid: UUID,
   restrictedEntity: OWLAPIOMF#Entity,
   scalarProperty: OWLAPIOMF#EntityScalarDataProperty,
   range: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityScalarDataPropertyExistentialRestrictionAxiom
  = (isTypeTermDefinedRecursively(restrictedEntity),
    isTypeTermDefinedRecursively(scalarProperty),
    isTypeTermDefinedRecursively(range)) match {
    case (true, true, true) =>
      val subC = owlDataFactory.getOWLClass(restrictedEntity.iri)
      for {
        axiom <-
        store
          .registerOMFEntityScalarDataPropertyExistentialRestrictionAxiomInstance(this,
            EntityScalarDataPropertyExistentialRestrictionAxiom(uuid, restrictedEntity, scalarProperty, range))
        _ <- applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont,
            owlDataFactory
              .getOWLSubClassOfAxiom(
                subC,
                owlDataFactory.getOWLDataSomeValuesFrom(scalarProperty.e, range.e),
                java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid)))),
          ifError = {
            "addEntityScalarDataPropertyExistentialRestrictionAxiom Error"
          })
      } yield {
        ax += axiom
        axiom
      }

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
          Map(AxiomScopeAccessKind.Range -> range))
      ).left

  }

  def addEntityScalarDataPropertyUniversalRestrictionAxiom
  (uuid: UUID,
   restrictedEntity: OWLAPIOMF#Entity,
   scalarProperty: OWLAPIOMF#EntityScalarDataProperty,
   range: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityScalarDataPropertyUniversalRestrictionAxiom
  = (isTypeTermDefinedRecursively(restrictedEntity),
    isTypeTermDefinedRecursively(scalarProperty),
    isTypeTermDefinedRecursively(range)) match {
    case (true, true, true) =>
      val subC = owlDataFactory.getOWLClass(restrictedEntity.iri)
      for {
        axiom <-
        store
          .registerOMFEntityScalarDataPropertyUniversalRestrictionAxiomInstance(this,
            EntityScalarDataPropertyUniversalRestrictionAxiom(uuid, restrictedEntity, scalarProperty, range))
        _ <- applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont,
            owlDataFactory
              .getOWLSubClassOfAxiom(
                subC,
                owlDataFactory.getOWLDataAllValuesFrom(scalarProperty.e, range.e),
                java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid)))),
          ifError = {
            "addEntityScalarDataPropertyUniversalRestrictionAxiom Error"
          })
      } yield {
        ax += axiom
        axiom
      }

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
          Map(AxiomScopeAccessKind.Range -> range))
      ).left

  }


  def addEntityScalarDataPropertyParticularRestrictionAxiom
  (uuid: UUID,
   restrictedEntity: OWLAPIOMF#Entity,
   scalarProperty: OWLAPIOMF#EntityScalarDataProperty,
   literalValue: LexicalValue)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#EntityScalarDataPropertyParticularRestrictionAxiom
  = (isTypeTermDefinedRecursively(restrictedEntity),
    isTypeTermDefinedRecursively(scalarProperty)) match {
    case (true, true) =>
      val subC = owlDataFactory.getOWLClass(restrictedEntity.iri)
      for {
        axiom <-
        store
          .registerOMFEntityScalarDataPropertyParticularRestrictionAxiomInstance(this,
            EntityScalarDataPropertyParticularRestrictionAxiom(uuid, restrictedEntity, scalarProperty, literalValue))
        _ <- applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont,
            owlDataFactory
              .getOWLSubClassOfAxiom(
                subC,
                owlDataFactory.getOWLDataHasValue(scalarProperty.e, owlDataFactory.getOWLLiteral(literalValue)),
                java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid)))),
          ifError = {
            "addEntityScalarDataPropertyParticularRestrictionAxiom Error"
          })
      } yield {
        ax += axiom
        axiom
      }

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

  def createEntityDefinitionAspectSubClassAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#Entity,
   sup: OWLAPIOMF#Aspect)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#AspectSpecializationAxiom
  = ax
    .find {
      case axiom: AspectSpecializationAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[Set[java.lang.Throwable] \/ OWLAPIOMF#AspectSpecializationAxiom](
    for {
      axiom <-
      store
        .registerOMFEntityDefinitionAspectSubClassAxiomInstance(this,
          AspectSpecializationAxiom(uuid, sub, sup))
    } yield {
      ax += axiom
      axiom
    }
  ) { other =>
    Set(
      duplicateModelTermAxiomException(AxiomExceptionKind.EntityDefinitionAspectSubClassAxiomException, other)
    ).left
  }

  def addEntityDefinitionAspectSubClassAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#Entity,
   sup: OWLAPIOMF#Aspect)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#AspectSpecializationAxiom
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
  (uuid: UUID,
   entityConceptDesignation: OWLAPIOMF#Concept,
   designationTerminologyGraph: OWLAPIOMF#TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ConceptDesignationTerminologyAxiom
  = gx
    .find {
      case axiom: ConceptDesignationTerminologyAxiom =>
        axiom.designatedConcept == entityConceptDesignation &&
          axiom.designatedTerminology == designationTerminologyGraph
      case _ =>
        false
    }
    .fold[Set[java.lang.Throwable] \/ ConceptDesignationTerminologyAxiom]({
    val axInstance = ConceptDesignationTerminologyAxiom(
      uuid, entityConceptDesignation, designationTerminologyGraph)
    for {
      axiom <- store.registerEntityConceptDesignationTerminologyGraphAxiom(this, axInstance)
    } yield {
      gx += axiom
      axiom
    }
  }) { other =>
    Set(
      duplicateTerminologyGraphAxiomException(AxiomExceptionKind.EntityConceptDesignationTerminologyGraphAxiomException, other)
    ).left
  }

  def addEntityConceptDesignationTerminologyGraphAxiom
  (uuid: UUID,
   entityConceptDesignation: OWLAPIOMF#Concept,
   designationTerminologyGraph: OWLAPIOMF#TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ConceptDesignationTerminologyAxiom
  = for {
    axiom <- createOMFEntityConceptDesignationTerminologyGraphAxiom(
      uuid, entityConceptDesignation, designationTerminologyGraph)
  } yield {
    gx += axiom
    axiom
  }

  def createEntityReifiedRelationshipSubClassAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#ReifiedRelationship,
   sup: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ReifiedRelationshipSpecializationAxiom
  = ax
    .find {
      case axiom: ReifiedRelationshipSpecializationAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[Set[java.lang.Throwable] \/ ReifiedRelationshipSpecializationAxiom]({
    val axInstance = ReifiedRelationshipSpecializationAxiom(uuid, sub, sup)
    for {
      axiom <- store
        .registerOMFEntityReifiedRelationshipSubClassAxiomInstance(this, axInstance)
    } yield {
      ax += axiom
      axiom
    }
  }) { other =>
    Set(
      duplicateModelTermAxiomException(AxiomExceptionKind.EntityReifiedRelationshipSubClassAxiomException, other)
    ).left
  }

  def addEntityReifiedRelationshipSubClassAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#ReifiedRelationship,
   sup: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : Set[java.lang.Throwable] \/ OWLAPIOMF#ReifiedRelationshipSpecializationAxiom
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
        axiomScopeException(
          AxiomExceptionKind.ReifiedRelationshipSubclassAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub))
      ).left

    case (true, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ReifiedRelationshipSubclassAxiomException,
          Map(AxiomScopeAccessKind.Sup -> sup))
      ).left

    case (false, false) =>
      Set(
        axiomScopeException(
          AxiomExceptionKind.ReifiedRelationshipSubclassAxiomException,
          Map(AxiomScopeAccessKind.Sub -> sub, AxiomScopeAccessKind.Sup -> sup))
      ).left
  }

}
