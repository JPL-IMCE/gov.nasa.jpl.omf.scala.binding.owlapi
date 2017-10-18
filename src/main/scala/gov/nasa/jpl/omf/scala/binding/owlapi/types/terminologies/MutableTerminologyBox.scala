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
import java.util.{Collections,UUID}

import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue, LiteralValue}
import gov.nasa.jpl.omf.scala.binding.owlapi.AxiomExceptionKind
import gov.nasa.jpl.omf.scala.binding.owlapi.ElementExceptionKind
import gov.nasa.jpl.omf.scala.binding.owlapi.types.{axiomScopeException, duplicateModelTermAxiomException, duplicateTerminologyGraphAxiomException, entityAlreadyDefinedException, entityConflictException, entityScopeException, terms}
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologyAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms._
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{MutableModule, Resource}
import gov.nasa.jpl.omf.scala.core.OMFError
import gov.nasa.jpl.omf.scala.core.OMLString.LocalName
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics.RelationshipCharacteristics
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWLFacet

import scala.collection.JavaConversions._
import scala.compat.java8.StreamConverters._
import scala.collection.immutable.{Iterable, Map, Seq, Set}
import scala.{Any, Boolean, Int, None, Option, Some, StringContext, Unit}
import scala.Predef.{ArrowAssoc, String, require}
import scalaz._
import Scalaz._

trait MutableTerminologyBox
  extends TerminologyBox
    with MutableModule {

  import ops._

  val chainRule2SWRLRule = new scala.collection.mutable.HashMap[ChainRule, SWRLRule]()

  def makeVariable(index: Int)
  : OMFError.Throwables \/ SWRLIArgument
  = for {
    viri <- withFragment(iri, LocalName(s"v$index"))
    v = owlDataFactory.getSWRLVariable(viri)
  } yield v

  override type MS = MutableTerminologyBoxSignature[OWLAPIOMF]

  override val sig: MS

  override protected val iri2typeTerm = scala.collection.mutable.HashMap[IRI, OWLAPIOMF#Term]()

  val LOG: Boolean = "true" equalsIgnoreCase java.lang.System.getProperty("gov.nasa.jpl.omf.scala.binding.owlapi.log.MutableTerminologyBox")

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: MutableTerminologyBox => true
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
  : OMFError.Throwables \/ AnnotationPropertyValue
  = for {
    a <- new AnnotationPropertyValue(
      oug = uuidG,
      subjectUUID = subject.uuid.toString,
      propertyUUID = property.uuid,
      value = value).right[OMFError.Throwables]
    _ = sig.annotationPropertyValues.find { a => a.subjectUUID == subject.uuid.toString && a.propertyUUID == property.uuid } match {
      case Some(a0) =>
        sig.annotationPropertyValues -= a0
        sig.annotationPropertyValues += a
      case None =>
        sig.annotationPropertyValues += a
    }
    ont_ap = owlDataFactory.getOWLAnnotationProperty(property.iri)
    ont_lit = owlDataFactory.getOWLLiteral(value)
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
  (subject: OWLAPIOMF#Element,
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
  (uuid: UUID,
   extendedG: TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ TerminologyExtensionAxiom
  = sig.extensions
    .find { _.extendedTerminology == extendedG }
    .fold[OMFError.Throwables \/ TerminologyExtensionAxiom](
    for {
      axiom <- store
        .createOMFTerminologyGraphDirectExtensionAxiom(uuid, this, extendedG)
    } yield {
      sig.extensions += axiom
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
  : OMFError.Throwables \/ TerminologyExtensionAxiom
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
  (tboxUUID: UUID, a: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Aspect
  = for {
    n <- getFragment(a.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createModelEntityAspect(a, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, a.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelEntityAspect
  (a: OWLClass, name: LocalName, uuid: UUID)
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
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.EntityAspect, a.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityAspect, a.getIRI, t)
      ).left
  }

  def addEntityAspect
  (aspectIRI: IRI, name: LocalName, uuid: UUID)
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
          new AddAxiom(ont, owlDataFactory.getOWLDeclarationAxiom(aspectC)),
          new AddAxiom(ont, owlDataFactory.getOWLSubClassOfAxiom(aspectC, backbone.AspectC))),
        "addAspect Error")
    } yield result
  } {
    case t: OWLAPIOMF#Aspect =>
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.EntityAspect, aspectIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityAspect, aspectIRI, t)
      ).left
  }

  /**
    * Constructs an OMF Concept as part of resolving its representation as an OWL Class
    * @param c An OWL Class representing an OMF Concept
    * @param store
    * @return The OMF Concept corresponding to its OWL Class `c` representation
    */
  def createModelEntityConcept
  (tboxUUID: UUID,
   c: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Concept
  = for {
    n <- getFragment(c.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createModelEntityConcept(c, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, c.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelEntityConcept
  (c: OWLClass, name: LocalName, uuid: UUID)
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
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.EntityConcept, c.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityConcept, c.getIRI, t)
      ).left
  }

  def addEntityConcept
  (conceptIRI: IRI, name: LocalName, uuid: UUID)
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
        entityAlreadyDefinedException(ElementExceptionKind.EntityConcept, conceptIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityConcept, conceptIRI, t)
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
  (tboxUUID: UUID,
   r: OWLClass,
   u: OWLObjectProperty, ui: Option[OWLObjectProperty],
   source: Entity, rSource: OWLObjectProperty,
   target: Entity, rTarget: OWLObjectProperty,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationship
  = for {
    rn <- getFragment(r.getIRI)
    ru = generateUUID(tboxUUID, "name" -> LocalName(rn))
    un <- getFragment(u.getIRI)
    in <- ui.fold[OMFError.Throwables \/ Option[LocalName]](None.right) { i =>
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
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationship
  = iri2typeTerm
    .get(r.getIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationship] {
    val _r = terms.ReifiedRelationship(r, r.getIRI, name, uuid,
      unreifiedPropertyName, u, inversePropertyName, ui,
      source, rSource, target, rTarget, characteristics)
    sig.reifiedRelationships += _r
    iri2typeTerm += r.getIRI -> _r
    \/-(_r)
  } {
    case t: OWLAPIOMF#ReifiedRelationship =>
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.EntityReifiedRelationship, r.getIRI, t)
      ).left
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
   uuid: UUID,
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
  (tboxUUID: UUID,
   r: OWLObjectProperty,
   source: Entity,
   target: Entity,
   characteristics: Iterable[RelationshipCharacteristics])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#UnreifiedRelationship
  = for {
    rn <- getFragment(r.getIRI)
    ru = generateUUID(tboxUUID, "name" -> LocalName(rn))
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
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.EntityUnreifiedRelationship, r.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.EntityUnreifiedRelationship, r.getIRI, t)
      ).left
  }

  protected def makeUnreifiedRelationship
  (rIRI: IRI, name: LocalName, uuid: UUID,
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
  (tboxUUID: UUID,
   dt: OWLDatatype)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Scalar
  = for {
    n <- getFragment(dt.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createModelScalarDataType(dt, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, dt.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelScalarDataType
  (dt: OWLDatatype, name: LocalName, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Scalar
  = iri2typeTerm
    .get(dt.getIRI)
    .fold[OMFError.Throwables \/ Scalar]{
    val _dt = terms.Scalar(dt, dt.getIRI, name, uuid)
    sig.scalarDataTypes += _dt
    iri2typeTerm += dt.getIRI -> _dt
    store.registerOMFModelScalarDataTypeInstance(this, _dt).map(_ => _dt)
  } {
    case t: Scalar =>
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.ScalarDataType, dt.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.ScalarDataType, dt.getIRI, t)
      ).left
  }

  def addScalarDataType
  (scalarIRI: IRI, name: LocalName, uuid: UUID)
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
            owlDataFactory
              .getOWLDeclarationAxiom(scalarDT))
        ),
        s"addScalarDataType error: $scalarIRI, name=$name")
    } yield result
  } {
    case t: OWLAPIOMF#Scalar =>
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.ScalarDataType, scalarIRI, t)
      ).left
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
  (tboxUUID: UUID,
   dt: OWLClass)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#Structure
  = for {
    n <- getFragment(dt.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createModelStructuredDataType(dt, n, u)
    aas = getRelevantSubjectAnnotationAssertions(ont, dt.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createModelStructuredDataType
  (dt: OWLClass, name: LocalName, uuid: UUID)
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
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.StructuredDataType, dt.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.StructuredDataType, dt.getIRI, t)
      ).left
  }

  def addStructuredDataType
  (structuredDataTypeIRI: IRI, name: LocalName, uuid: UUID)
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
        entityAlreadyDefinedException(ElementExceptionKind.StructuredDataType, structuredDataTypeIRI, t)
      ).left
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
  (tboxUUID: UUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createScalarOneOfRestriction(
      restrictionDT, n, u, restrictedRange)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createScalarOneOfRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfRestriction
  = iri2typeTerm
    .get(restrictionDT.getIRI)
    .fold[OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfRestriction]{
    val _rdt = terms.ScalarOneOfRestriction(
      restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange)
    sig.scalarOneOfRestrictions += _rdt
    iri2typeTerm += restrictionDT.getIRI -> _rdt
    store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
  } {
    case t: OWLAPIOMF#ScalarOneOfRestriction =>
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.ScalarOneOfRestriction, restrictionDT.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.ScalarOneOfRestriction, restrictionDT.getIRI, t)
      ).left
  }

  def addScalarOneOfRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
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
        entityAlreadyDefinedException(ElementExceptionKind.ScalarOneOfRestriction, dataTypeIRI, t)
      ).left
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
   value: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfLiteralAxiom
  = for {
    axiomUUID <- scalarOneOfLiteralAxiomUUID(this, scalarOneOfRestriction)
    ax <- createScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value, valueType)
  } yield ax

  def createScalarOneOfLiteralAxiom
  (axiomUUID: UUID,
   scalarOneOfRestriction: OWLAPIOMF#ScalarOneOfRestriction,
   value: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfLiteralAxiom
  = types.termAxioms.ScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value, valueType).right

  def addScalarOneOfLiteralAxiom
  (axiomUUID: UUID,
   scalarOneOfRestriction: OWLAPIOMF#ScalarOneOfRestriction,
   value: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfLiteralAxiom
  = for {
    axiom <- createScalarOneOfLiteralAxiom(axiomUUID, scalarOneOfRestriction, value, valueType)
    restrictionDT = scalarOneOfRestriction.e
    restrictedDT = scalarOneOfRestriction.restrictedDataRange.e
    result <- ont
      .datatypeDefinitions(restrictionDT)
      .toScala[Set]
      .headOption
      .fold[OMFError.Throwables \/ OWLAPIOMF#ScalarOneOfLiteralAxiom]{
      for {
        _ <- applyOntologyChanges(ontManager,
          Seq(
            new AddAxiom(ont, owlDataFactory
              .getOWLDatatypeDefinitionAxiom(
                restrictionDT,
                owlDataFactory.getOWLDataOneOf(
                  LiteralConversions.toOWLLiteral(value, owlDataFactory,
                    valueType.map(_.e).orElse(Option.apply(restrictedDT))))))
          ),
          "addScalarOneOfLiteralAxiom error")
      } yield axiom
    } { restrictionDF =>
      restrictionDF.getDataRange match {
        case dof: OWLDataOneOf =>
          val values = java.util.stream.Stream.concat(
            dof.values,
            java.util.stream.Stream.of[OWLLiteral](
              LiteralConversions.toOWLLiteral(value, owlDataFactory, Option.apply(restrictedDT))))
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
  (tboxUUID: UUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#BinaryScalarRestriction
  = for {
      n <- getFragment(restrictionDT.getIRI)
      u = generateUUID(tboxUUID, "name" -> LocalName(n))
      term <- createBinaryScalarRestriction(
        restrictionDT, n, u, restrictedRange,
        length, minLength, maxLength)
      aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
      _ <- store.ops.addAnnotationAssertions(this, term, aas)
    } yield term

  def createBinaryScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#BinaryScalarRestriction
  = if (store.isBuiltInDatatypeMapConstructed || store.isBinaryKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#BinaryScalarRestriction] {
      val _rdt = terms.BinaryScalarRestriction(
        restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
        length, minLength, maxLength)
      sig.binaryScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#BinaryScalarRestriction =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.BinaryScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.BinaryScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createBinaryScalarRestriction: restricted data range must be binary per OWL2 section 4.5: $restrictedRange")
    ).left

  def addBinaryScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral])
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
    _ <- store.registerOMFModelScalarDataTypeInstance(this, rdr)
    _ <- applyOntologyChangesOrNoOp(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory
          .getOWLDeclarationAxiom(restrictionDT))
      ) ++ rdr.length.map { l =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(OWLFacet.LENGTH, Integer.parseInt(l)))))
      } ++ rdr.minLength.map { minL =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(OWLFacet.MIN_LENGTH, Integer.parseInt(minL)))))
      } ++ rdr.maxLength.map { maxL =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, Integer.parseInt(maxL)))))
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
  (tboxUUID: UUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral],
   pattern: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#IRIScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createIRIScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      length, minLength, maxLength, pattern)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createIRIScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral],
   pattern: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#IRIScalarRestriction
  = if (store.isBuiltInDatatypeMapConstructed || store.isIRIKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#IRIScalarRestriction]{
      val _rdt = terms.IRIScalarRestriction(
        restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
        length, minLength, maxLength, pattern)
      sig.iriScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#IRIScalarRestriction =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.IRIScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.IRIScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createBinaryScalarRestriction: restricted data range must be IRI per OWL2 section 4.6: $restrictedRange")
    ).left

  def addIRIScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral],
   pattern: Option[String])
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
    _ <- store.registerOMFModelScalarDataTypeInstance(this, rdr)
    _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ) ++ rdr.length.map { l =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.LENGTH, Integer.parseInt(l)))))
        } ++ rdr.minLength.map { minL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.MIN_LENGTH, Integer.parseInt(minL)))))
        } ++ rdr.maxLength.map { maxL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, Integer.parseInt(maxL)))))
        } ++ rdr.pattern.map { patt =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(OWLFacet.PATTERN, owlDataFactory.getOWLLiteral(patt)))))
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
  (tboxUUID: UUID,
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
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createNumericScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      minInclusive, maxInclusive, minExclusive, maxExclusive)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createNumericScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[tables.LiteralNumber],
   maxInclusive: Option[tables.LiteralNumber],
   minExclusive: Option[tables.LiteralNumber],
   maxExclusive: Option[tables.LiteralNumber])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#NumericScalarRestriction
  = if (store.isBuiltInDatatypeMapConstructed || store.isNumericKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#NumericScalarRestriction] {
      val _rdt = terms.NumericScalarRestriction(
        restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
        minInclusive, maxInclusive, minExclusive, maxExclusive)
      sig.numericScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#NumericScalarRestriction =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.NumericScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.NumericScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createBinaryScalarRestriction: restricted data range must be numeric per OWL2 sections 4.1, 4.2: $restrictedRange")
    ).left

  def addNumericScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
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
    _ <- store.registerOMFModelScalarDataTypeInstance(this, rdr)
    _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ) ++ rdr.minInclusive.map { minI =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MIN_INCLUSIVE,
                  LiteralConversions.toOWLLiteral(minI, owlDataFactory, Option.apply(rdr.restrictedDataRange.e))))))
        } ++ rdr.maxInclusive.map { maxI =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MAX_INCLUSIVE,
                  LiteralConversions.toOWLLiteral(maxI, owlDataFactory, Option.apply(rdr.restrictedDataRange.e))))))
        } ++ rdr.minExclusive.map { minE =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MIN_EXCLUSIVE,
                  LiteralConversions.toOWLLiteral(minE, owlDataFactory, Option.apply(rdr.restrictedDataRange.e))))))
        } ++ rdr.maxExclusive.map { maxE =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MAX_EXCLUSIVE,
                  LiteralConversions.toOWLLiteral(maxE, owlDataFactory, Option.apply(rdr.restrictedDataRange.e))))))
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
  (tboxUUID: UUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral],
   pattern: Option[String],
   language: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#PlainLiteralScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createPlainLiteralScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      length, minLength, maxLength, pattern, language)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createPlainLiteralScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral],
   pattern: Option[String],
   language: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#PlainLiteralScalarRestriction
  = if (store.isBuiltInDatatypeMapConstructed || store.isPlainLiteralKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#PlainLiteralScalarRestriction] {
      val _rdt = terms.PlainLiteralScalarRestriction(
        restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
        length, minLength, maxLength, pattern, language)
      sig.plainLiteralScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#PlainLiteralScalarRestriction =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.PlainLiteralScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.PlainLiteralScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createBinaryScalarRestriction: restricted data range must be plain literal per OWL2 section 4.3: $restrictedRange")
    ).left

  def addPlainLiteralScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral],
   pattern: Option[String],
   language: Option[String])
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
    _ <- store.registerOMFModelScalarDataTypeInstance(this, rdr)
  _ <- applyOntologyChangesOrNoOp(ontManager,
        Seq(
          new AddAxiom(ont, owlDataFactory
            .getOWLDeclarationAxiom(restrictionDT))
        ) ++ rdr.length.map { l =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.LENGTH,
                  Integer.parseInt(l)))))
        } ++ rdr.minLength.map { minL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MIN_LENGTH,
                  Integer.parseInt(minL)))))
        } ++ rdr.maxLength.map { maxL =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.MAX_LENGTH,
                  Integer.parseInt(maxL)))))
        } ++ rdr.pattern.map { patt =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.PATTERN,
                  owlDataFactory.getOWLLiteral(patt)))))
        } ++ rdr.language.map { lang =>
          new AddAxiom(ont, owlDataFactory
            .getOWLDatatypeDefinitionAxiom(
              restrictionDT,
              owlDataFactory.getOWLDatatypeRestriction(
                rdr.restrictedDataRange.e,
                owlDataFactory.getOWLFacetRestriction(
                  OWLFacet.LANG_RANGE,
                  owlDataFactory.getOWLLiteral(lang)))))
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
  (tboxUUID: UUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral],
   pattern: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StringScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createStringScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      length, minLength, maxLength, pattern)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createStringScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral],
   pattern: Option[String])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StringScalarRestriction
  = if (store.isBuiltInDatatypeMapConstructed || store.isStringKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#StringScalarRestriction] {
      val _rdt = terms.StringScalarRestriction(
        restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
        length, minLength, maxLength, pattern)
      sig.stringScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#StringScalarRestriction =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.StringScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.StringScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createBinaryScalarRestriction: restricted data range must be string per OWL2 section 4.3: $restrictedRange")
    ).left

  def addStringScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   length: Option[tables.PositiveIntegerLiteral],
   minLength: Option[tables.PositiveIntegerLiteral],
   maxLength: Option[tables.PositiveIntegerLiteral],
   pattern: Option[String])
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
    _ <- store.registerOMFModelScalarDataTypeInstance(this, rdr)
    _ <- applyOntologyChangesOrNoOp(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory
          .getOWLDeclarationAxiom(restrictionDT))
      ) ++ rdr.length.map { l =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(OWLFacet.LENGTH, Integer.parseInt(l)))))
      } ++ rdr.minLength.map { minL =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(OWLFacet.MIN_LENGTH, Integer.parseInt(minL)))))
      } ++ rdr.maxLength.map { maxL =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(OWLFacet.MAX_LENGTH, Integer.parseInt(maxL)))))
      } ++ rdr.pattern.map { patt =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.PATTERN,
                owlDataFactory.getOWLLiteral(patt)))))
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
  (tboxUUID: UUID,
   restrictionDT: OWLDatatype,
   restrictedRange: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#SynonymScalarRestriction
  = for {
    n <- getFragment(restrictionDT.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createSynonymScalarRestriction(
      restrictionDT, n, u, restrictedRange)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createSynonymScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
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
    store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
  } {
    case t: OWLAPIOMF#SynonymScalarRestriction =>
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.SynonymScalarRestriction, restrictionDT.getIRI, t)
      ).left
    case t =>
      Set(
        entityConflictException(ElementExceptionKind.SynonymScalarRestriction, restrictionDT.getIRI, t)
      ).left
  }

  def addSynonymScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
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
    _ <- store.registerOMFModelScalarDataTypeInstance(this, rdr)
    _ <- applyOntologyChangesOrNoOp(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory
          .getOWLDeclarationAxiom(restrictionDT))
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
  (tboxUUID: UUID,
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
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createTimeScalarRestriction(
      restrictionDT, n, u, restrictedRange,
      minInclusive, maxInclusive, minExclusive, maxExclusive)
    aas = getRelevantSubjectAnnotationAssertions(ont, restrictionDT.getIRI)
    _ <- store.ops.addAnnotationAssertions(this, term, aas)
  } yield term

  def createTimeScalarRestriction
  (restrictionDT: OWLDatatype, name: LocalName, uuid: UUID,
   restrictedRange: OWLAPIOMF#DataRange,
   minInclusive: Option[tables.LiteralDateTime],
   maxInclusive: Option[tables.LiteralDateTime],
   minExclusive: Option[tables.LiteralDateTime],
   maxExclusive: Option[tables.LiteralDateTime])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#TimeScalarRestriction
  = if (store.isBuiltInDatatypeMapConstructed || store.isTimeKind(restrictedRange)) {
    iri2typeTerm
      .get(restrictionDT.getIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#TimeScalarRestriction] {
      val _rdt = terms.TimeScalarRestriction(
        restrictionDT, restrictionDT.getIRI, uuid, name, restrictedRange,
        minInclusive, maxInclusive, minExclusive, maxExclusive)
      sig.timeScalarRestrictions += _rdt
      iri2typeTerm += restrictionDT.getIRI -> _rdt
      store.registerOMFModelScalarDataTypeInstance(this, _rdt).map(_ => _rdt)
    } {
      case t: OWLAPIOMF#TimeScalarRestriction =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.TimeScalarRestriction, restrictionDT.getIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.TimeScalarRestriction, restrictionDT.getIRI, t)
        ).left
    }
  } else
    Set[java.lang.Throwable](OMFError.omfError(
      s"createTimeScalarRestriction: restricted data range must be time per OWL2 section 4.7: $restrictedRange")
    ).left

  def addTimeScalarRestriction
  (dataTypeIRI: IRI, name: LocalName, uuid: UUID,
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
    _ <- store.registerOMFModelScalarDataTypeInstance(this, rdr)
    _ <- applyOntologyChangesOrNoOp(ontManager,
      Seq(
        new AddAxiom(ont, owlDataFactory
          .getOWLDeclarationAxiom(restrictionDT))
      ) ++ rdr.minInclusive.map { minI =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.MIN_INCLUSIVE,
                LiteralConversions.toOWLLiteral(minI, owlDataFactory, Option.apply(rdr.restrictedDataRange.e))))))
      } ++ rdr.maxInclusive.map { maxI =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.MAX_INCLUSIVE,
                LiteralConversions.toOWLLiteral(maxI, owlDataFactory, Option.apply(rdr.restrictedDataRange.e))))))
      } ++ rdr.minExclusive.map { minE =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.MIN_EXCLUSIVE,
                LiteralConversions.toOWLLiteral(minE, owlDataFactory, Option.apply(rdr.restrictedDataRange.e))))))
      } ++ rdr.maxExclusive.map { maxE =>
        new AddAxiom(ont, owlDataFactory
          .getOWLDatatypeDefinitionAxiom(
            restrictionDT,
            owlDataFactory.getOWLDatatypeRestriction(
              rdr.restrictedDataRange.e,
              owlDataFactory.getOWLFacetRestriction(
                OWLFacet.MAX_EXCLUSIVE,
                LiteralConversions.toOWLLiteral(maxE, owlDataFactory, Option.apply(rdr.restrictedDataRange.e))))))
      },
      s"addTimeScalarRestriction error: ${restrictionDT.getIRI}")
    _ = iri2typeTerm += restrictionDT.getIRI -> rdr
    _ = sig.timeScalarRestrictions += rdr
  } yield rdr

  def createDataRelationshipFromEntityToScalar
  (tboxUUID: UUID,
   esc: OWLDataProperty,
   isIdentityCriteria: Boolean,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataProperty
  = for {
    n <- getFragment(esc.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createDataRelationshipFromEntityToScalar(esc, n, isIdentityCriteria, u, source, target)
  } yield term

  def createDataRelationshipFromEntityToScalar
  (esc: OWLDataProperty, name: LocalName, isIdentityCriteria: Boolean, uuid: UUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataProperty
  = {
    val escIRI: IRI = esc.getIRI
    iri2typeTerm
      .get(escIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataProperty](
      for {
        _esc <- store.registerDataRelationshipFromEntityToScalar(
          this, terms.EntityScalarDataProperty(esc, esc.getIRI, name, isIdentityCriteria, uuid, source, target))
      } yield {
        sig.entityScalarDataProperties += _esc
        iri2typeTerm += escIRI -> _esc
        _esc
      }) {
      case t: OWLAPIOMF#EntityScalarDataProperty =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.DataRelationshipFromEntityToScalar, escIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.DataRelationshipFromEntityToScalar, escIRI, t)
        ).left
    }
  }

  protected def makeDataRelationshipFromEntityToScalar
  (dIRI: IRI, name: LocalName, isIdentityCriteria: Boolean, uuid: UUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataProperty
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
  (tboxUUID: UUID,
   esc: OWLObjectProperty,
   isIdentityCriteria: Boolean,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataProperty
  = for {
    n <- getFragment(esc.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createDataRelationshipFromEntityToStructure(esc, n, isIdentityCriteria, u, source, target)
  } yield term

  def createDataRelationshipFromEntityToStructure
  (esc: OWLObjectProperty, name: LocalName, isIdentityCriteria: Boolean, uuid: UUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataProperty
  = {
    val escIRI: IRI = esc.getIRI
    iri2typeTerm
      .get(escIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataProperty](
      for {
        _esc <- store.registerDataRelationshipFromEntityToStructure(
          this, terms.EntityStructuredDataProperty(esc, esc.getIRI, name, isIdentityCriteria, uuid, source, target))
      } yield {
        sig.entityStructuredDataProperties += _esc
        iri2typeTerm += escIRI -> _esc
        _esc
      }) {
      case t: OWLAPIOMF#EntityStructuredDataProperty =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.DataRelationshipFromEntityToStructure, escIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.DataRelationshipFromEntityToStructure, escIRI, t)
        ).left
    }
  }

  protected def makeDataRelationshipFromEntityToStructure
  (dIRI: IRI, name: LocalName, isIdentityCriteria: Boolean, uuid: UUID,
   source: OWLAPIOMF#Entity,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityStructuredDataProperty
  = {
    val escDP = owlDataFactory.getOWLObjectProperty(dIRI)
    for {
      term <- createDataRelationshipFromEntityToStructure(escDP, name, isIdentityCriteria, uuid, source, target)
    } yield {
      for {
        change <- Seq(
          new AddAxiom(ont,
            owlDataFactory
              .getOWLDeclarationAxiom(escDP)),
          new AddAxiom(ont,
            owlDataFactory
              .getOWLSubObjectPropertyOfAxiom(escDP, backbone.topReifiedStructuredDataPropertyOP)),
          new AddAxiom(ont,
            owlDataFactory
              .getOWLObjectPropertyDomainAxiom(escDP, source.e)),
          new AddAxiom(ont,
            owlDataFactory
              .getOWLObjectPropertyRangeAxiom(escDP, target.e))
        )
      } {
        val result = ontManager.applyChange(change)
        require(
          result == ChangeApplied.SUCCESSFULLY,
          s"\nmakeDataRelationshipFromEntityToStructure:\n$change")
      }
      term
    }
  }

  def addDataRelationshipFromEntityToStructure
  (dIRI: IRI, name: LocalName, isIdentityCriteria: Boolean, uuid: UUID,
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
  (tboxUUID: UUID,
   esc: OWLDataProperty,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarDataProperty
  = for {
    n <- getFragment(esc.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createDataRelationshipFromStructureToScalar(esc, n, u, source, target)
  } yield term

  def createDataRelationshipFromStructureToScalar
  (esc: OWLDataProperty, name: LocalName, uuid: UUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarDataProperty
  = {
    val escIRI: IRI = esc.getIRI
    iri2typeTerm
      .get(escIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#ScalarDataProperty](
      for {
        _esc <- store.registerDataRelationshipFromStructureToScalar(
          this, terms.ScalarDataProperty(esc, esc.getIRI, name, uuid, source, target))
      } yield {
        sig.scalarDataProperties += _esc
        iri2typeTerm += escIRI -> _esc
        _esc
      }) {
      case t: OWLAPIOMF#ScalarDataProperty =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.DataRelationshipFromStructureToScalar, escIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.DataRelationshipFromStructureToScalar, escIRI, t)
        ).left
    }
  }

  protected def makeDataRelationshipFromStructureToScalar
  (dIRI: IRI, name: LocalName, uuid: UUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ScalarDataProperty
  = {
    val escDP = owlDataFactory.getOWLDataProperty(dIRI)
    for {
      term <- createDataRelationshipFromStructureToScalar(escDP, name, uuid, source, target)
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
              .getOWLDataPropertyRangeAxiom(escDP, target.e))
        )
      } {
        val result = ontManager.applyChange(change)
        require(
          result == ChangeApplied.SUCCESSFULLY,
          s"\nmakeDataRelationshipFromStructureToScalar:\n$change")
      }
      term
    }
  }

  def addDataRelationshipFromStructureToScalar
  (dIRI: IRI, name: LocalName, uuid: UUID,
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
  (tboxUUID: UUID,
   esc: OWLObjectProperty,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StructuredDataProperty
  = for {
    n <- getFragment(esc.getIRI)
    u = generateUUID(tboxUUID, "name" -> LocalName(n))
    term <- createDataRelationshipFromStructureToStructure(esc, n, u, source, target)
  } yield term

  def createDataRelationshipFromStructureToStructure
  (esc: OWLObjectProperty, name: LocalName, uuid: UUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StructuredDataProperty
  = {
    val escIRI: IRI = esc.getIRI
    iri2typeTerm
      .get(escIRI)
      .fold[OMFError.Throwables \/ OWLAPIOMF#StructuredDataProperty](
      for {
        _esc <- store.registerDataRelationshipFromStructureToStructure(
          this, terms.StructuredDataProperty(esc, esc.getIRI, name, uuid, source, target))
      } yield {
        sig.structuredDataProperties += _esc
        iri2typeTerm += escIRI -> _esc
        _esc
      }) {
      case t: OWLAPIOMF#StructuredDataProperty =>
        Set(
          entityAlreadyDefinedException(ElementExceptionKind.DataRelationshipFromStructureToStructure, escIRI, t)
        ).left
      case t =>
        Set(
          entityConflictException(ElementExceptionKind.DataRelationshipFromStructureToStructure, escIRI, t)
        ).left
    }
  }

  protected def makeDataRelationshipFromStructureToStructure
  (dIRI: IRI, name: LocalName, uuid: UUID,
   source: OWLAPIOMF#Structure,
   target: OWLAPIOMF#Structure)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#StructuredDataProperty
  = {
    val escDP = owlDataFactory.getOWLObjectProperty(dIRI)
    for {
      term <- createDataRelationshipFromStructureToStructure(escDP, name, uuid, source, target)
    } yield {
      for {
        change <- Seq(
          new AddAxiom(ont,
            owlDataFactory
              .getOWLDeclarationAxiom(escDP)),
          new AddAxiom(ont,
            owlDataFactory
              .getOWLSubObjectPropertyOfAxiom(escDP, backbone.topReifiedStructuredDataPropertyOP)),
          new AddAxiom(ont,
            owlDataFactory
              .getOWLObjectPropertyDomainAxiom(escDP, source.e)),
          new AddAxiom(ont,
            owlDataFactory
              .getOWLObjectPropertyRangeAxiom(escDP, target.e))
        )
      } {
        val result = ontManager.applyChange(change)
        require(
          result == ChangeApplied.SUCCESSFULLY,
          s"\nmakeDataRelationshipFromStructureToStucture:\n$change")
      }
      term
    }
  }

  def addDataRelationshipFromStructureToStructure
  (dIRI: IRI, name: LocalName, uuid: UUID,
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
  = sig.aspectPredicates.find(_.bodySegment == seg) orElse
    sig.conceptPredicates.find(_.bodySegment == seg) orElse
    sig.reifiedRelationshipPredicates.find(_.bodySegment == seg) orElse
    sig.reifiedRelationshipPropertyPredicates.find(_.bodySegment == seg) orElse
    sig.reifiedRelationshipInversePropertyPredicates.find(_.bodySegment == seg) orElse
    sig.reifiedRelationshipSourcePropertyPredicates.find(_.bodySegment == seg) orElse
    sig.reifiedRelationshipSourceInversePropertyPredicates.find(_.bodySegment == seg) orElse
    sig.reifiedRelationshipTargetPropertyPredicates.find(_.bodySegment == seg) orElse
    sig.reifiedRelationshipTargetInversePropertyPredicates.find(_.bodySegment == seg) orElse
    sig.unreifiedRelationshipPropertyPredicates.find(_.bodySegment == seg) orElse
    sig.unreifiedRelationshipInversePropertyPredicates.find(_.bodySegment == seg)  match {
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
      sig.ruleBodySegments.find(_.previousSegment.contains(seg)) match {
        case Some(next) =>
          collectSegmentPredicates(next, predicates :+ pred)
        case None =>
          predicates.right
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
    val range
    : OMFError.Throwables \/ (SWRLIArgument, Int)
    = predicates.head match {
      case _: UnarySegmentPredicate =>
        (prevV -> vIndex).right
      case _: BinarySegmentPredicate =>
        if (predicates.tail.exists {
          case _: UnarySegmentPredicate => false
          case _: BinarySegmentPredicate => true
        })
          makeVariable(vIndex).map {
            _ -> (1 + vIndex)
          }
        else
          (prevV -> vIndex).right
    }
    range match {
      case \/-((rangeV, vNext)) =>
        predicates.head match {
          case p0: AspectPredicate =>
            val p1 = owlDataFactory.getSWRLClassAtom(p0.termPredicate.e, prevV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case p0: ConceptPredicate =>
            val p1 = owlDataFactory.getSWRLClassAtom(p0.termPredicate.e, prevV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case p0: ReifiedRelationshipPredicate =>
            val p1 = owlDataFactory.getSWRLClassAtom(p0.termPredicate.e, prevV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case p0: ReifiedRelationshipPropertyPredicate =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(p0.termPredicate.unreified, prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case p0: ReifiedRelationshipInversePropertyPredicate =>
            val p1 = p0.termPredicate.inverse match {
              case Some(inv) =>
                owlDataFactory.getSWRLObjectPropertyAtom(
                  inv, prevV, rangeV)
              case None =>
                owlDataFactory.getSWRLObjectPropertyAtom(
                  owlDataFactory.getOWLObjectInverseOf(p0.termPredicate.unreified), prevV, rangeV)
            }
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case p0: ReifiedRelationshipSourcePropertyPredicate =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(
              p0.termPredicate.rSource, prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case p0: ReifiedRelationshipSourceInversePropertyPredicate =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(
              owlDataFactory.getOWLObjectInverseOf(p0.termPredicate.rSource), prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case p0: ReifiedRelationshipTargetPropertyPredicate =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(
              p0.termPredicate.rTarget, prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case p0: ReifiedRelationshipTargetInversePropertyPredicate =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(
              owlDataFactory.getOWLObjectInverseOf(p0.termPredicate.rTarget), prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case p0: UnreifiedRelationshipPropertyPredicate =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(
              p0.termPredicate.e, prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

          case p0: UnreifiedRelationshipInversePropertyPredicate =>
            val p1 = owlDataFactory.getSWRLObjectPropertyAtom(
              owlDataFactory.getOWLObjectInverseOf(p0.termPredicate.e), prevV, rangeV)
            convertBodyPredicates2Atoms(vNext, rangeV, predicates.tail, nextV, atoms :+ p1)

        }
      case -\/(errors) =>
        -\/(errors)
    }
  }

  def makeChainRule
  (rule: OWLAPIOMF#ChainRule)
  : OMFError.Throwables \/ Unit
  = for {
    v0 <- makeVariable(0)
    v1 <- makeVariable(1)
    predicates <- collectRuleBodyPredicates(rule)
    a0 = owlDataFactory.getSWRLClassAtom(rule.head.source.e, v0)
    a1 = owlDataFactory.getSWRLClassAtom(rule.head.target.e, v1)
    name <- getFragment(iri)
    headAtom = owlDataFactory.getSWRLObjectPropertyAtom(rule.head.e, v0, v1)
    bodyAtoms <- convertBodyPredicates2Atoms(2, v0, predicates, v1, Seq.empty)
    r = owlDataFactory.getSWRLRule(
      Collections.singleton(a0),
      Collections.singleton(headAtom),
      Collections.singleton(
        owlDataFactory.getOWLAnnotation(
          owlDataFactory.getRDFSLabel(),
          owlDataFactory.getOWLLiteral(name, owlDataFactory.getStringOWLDatatype))
      ))
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
    u = generateUUID(this.uuid, "name" -> name)
    r <- iri2typeTerm.get(iri).fold[OMFError.Throwables \/ OWLAPIOMF#ChainRule] {
      val cr = types.terms.ChainRule(iri, name, u, head)
      sig.chainRules.add(cr)
      \/-(cr)
    } { t =>
      Set(
        entityAlreadyDefinedException(ElementExceptionKind.ChainRule, iri, t)
      ).left
    }
  } yield r

  def addChainRule
  (iri: IRI,
   uuid: UUID,
   head: OWLAPIOMF#UnreifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ChainRule
  = {
    val rule = ChainRule(iri, name, uuid, head)
    sig.chainRules.add(rule)
    rule.right
  }

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
            \/-(ps.uuid.toString)
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
        ps.position.toString
    }
    u = generateUUID("RuleBodySegment", "chainRule" -> chainRuleUUID, "position" -> positionUUID)
    ruleBodySegment = types.terms.RuleBodySegment(u, chainRule, previousSegment)
    _ = sig.ruleBodySegments.add(ruleBodySegment)
  } yield ruleBodySegment

  def addRuleBodySegment
  (uuid: UUID,
   chainRule: Option[OWLAPIOMF#ChainRule],
   previousSegment: Option[OWLAPIOMF#RuleBodySegment])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#RuleBodySegment
  = {
    val rbs = RuleBodySegment(uuid, chainRule, previousSegment)
    sig.ruleBodySegments.add(rbs)
    rbs.right
  }

  def createAspectPredicate
  (a: SWRLClassAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   aspect: OWLAPIOMF#Aspect)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#AspectPredicate
  = {
    val u = generateUUID(
      "AspectPredicate",
      "aspect" -> aspect.uuid.toString,
      "bodySegment" -> bodySegment.uuid.toString)
    val p = types.terms.AspectPredicate(bodySegment, aspect, u)
    sig.aspectPredicates.add(p)
    p.right
  }

  def addAspectPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   aspect: OWLAPIOMF#Aspect)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#AspectPredicate
  = {
    val p = AspectPredicate(bodySegment, aspect, uuid)
    sig.aspectPredicates.add(p)
    p.right
  }

  def createConceptPredicate
  (a: SWRLClassAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   concept: OWLAPIOMF#Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ConceptPredicate
  = {
    val u = generateUUID(
      "ConceptPredicate",
      "bodySegment" -> bodySegment.uuid.toString,
      "concept" -> concept.uuid.toString)
    val p = types.terms.ConceptPredicate(bodySegment, concept, u)
    sig.conceptPredicates.add(p)
    p.right
  }

  def addConceptPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   concept: OWLAPIOMF#Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ConceptPredicate
  = {
    val p = ConceptPredicate(bodySegment, concept, uuid)
    sig.conceptPredicates.add(p)
    p.right
  }

  def createReifiedRelationshipPredicate
  (a: SWRLClassAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipPredicate
  = {
    val u = generateUUID(
      "ReifiedRelationshipPredicate",
      "bodySegment" -> bodySegment.uuid.toString,
      "reifiedRelationship" -> reifiedRelationship.uuid.toString)
    val p = types.terms.ReifiedRelationshipPredicate(bodySegment, reifiedRelationship, u)
    sig.reifiedRelationshipPredicates.add(p)
    p.right
  }

  def addReifiedRelationshipPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipPredicate
  = {
    val p = ReifiedRelationshipPredicate(bodySegment, reifiedRelationship, uuid)
    sig.reifiedRelationshipPredicates.add(p)
    p.right
  }

  def createReifiedRelationshipPropertyPredicate
  (a: SWRLObjectPropertyAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipPropertyPredicate
  = {
    val u = generateUUID(
      "ReifiedRelationshipPropertyPredicate",
      "bodySegment" -> bodySegment.uuid.toString,
      "reifiedRelationship" -> reifiedRelationship.uuid.toString)
    val p = types.terms.ReifiedRelationshipPropertyPredicate(bodySegment, reifiedRelationship, u)
    sig.reifiedRelationshipPropertyPredicates.add(p)
    p.right
  }

  def addReifiedRelationshipPropertyPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipPropertyPredicate
  = {
    val p = ReifiedRelationshipPropertyPredicate(bodySegment, reifiedRelationship, uuid)
    sig.reifiedRelationshipPropertyPredicates.add(p)
    p.right
  }

  def createReifiedRelationshipInversePropertyPredicate
  (a: SWRLObjectPropertyAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipInversePropertyPredicate
  = {
    val u = generateUUID(
      "ReifiedRelationshipInversePropertyPredicate",
      "bodySegment" -> bodySegment.uuid.toString,
      "reifiedRelationship" -> reifiedRelationship.uuid.toString)
    val p = types.terms.ReifiedRelationshipInversePropertyPredicate(bodySegment, reifiedRelationship, u)
    sig.reifiedRelationshipInversePropertyPredicates.add(p)
    p.right
  }

  def addReifiedRelationshipInversePropertyPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipInversePropertyPredicate
  = {
    val p = ReifiedRelationshipInversePropertyPredicate(bodySegment, reifiedRelationship, uuid)
    sig.reifiedRelationshipInversePropertyPredicates.add(p)
    p.right
  }

  def createReifiedRelationshipSourcePropertyPredicate
  (a: SWRLObjectPropertyAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipSourcePropertyPredicate
  = {
    val u = generateUUID(
      "ReifiedRelationshipSourcePropertyPredicate",
      "bodySegment" -> bodySegment.uuid.toString,
      "reifiedRelationship" -> reifiedRelationship.uuid.toString)
    val p = types.terms.ReifiedRelationshipSourcePropertyPredicate(bodySegment, reifiedRelationship, u)
    sig.reifiedRelationshipSourcePropertyPredicates.add(p)
    p.right
  }

  def addReifiedRelationshipSourcePropertyPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipSourcePropertyPredicate
  = {
    val p = ReifiedRelationshipSourcePropertyPredicate(bodySegment, reifiedRelationship, uuid)
    sig.reifiedRelationshipSourcePropertyPredicates.add(p)
    p.right
  }

  def createReifiedRelationshipSourceInversePropertyPredicate
  (a: SWRLObjectPropertyAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipSourceInversePropertyPredicate
  = {
    val u = generateUUID(
      "ReifiedRelationshipSourceInversePropertyPredicate",
      "bodySegment" -> bodySegment.uuid.toString,
      "reifiedRelationship" -> reifiedRelationship.uuid.toString)
    val p = types.terms.ReifiedRelationshipSourceInversePropertyPredicate(bodySegment, reifiedRelationship, u)
    sig.reifiedRelationshipSourceInversePropertyPredicates.add(p)
    p.right
  }

  def addReifiedRelationshipSourceInversePropertyPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipSourceInversePropertyPredicate
  = {
    val p = ReifiedRelationshipSourceInversePropertyPredicate(bodySegment, reifiedRelationship, uuid)
    sig.reifiedRelationshipSourceInversePropertyPredicates.add(p)
    p.right
  }

  def createReifiedRelationshipTargetPropertyPredicate
  (a: SWRLObjectPropertyAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipTargetPropertyPredicate
  = {
    val u = generateUUID(
      "ReifiedRelationshipTargetPropertyPredicate",
      "bodySegment" -> bodySegment.uuid.toString,
      "reifiedRelationship" -> reifiedRelationship.uuid.toString)
    val p = types.terms.ReifiedRelationshipTargetPropertyPredicate(bodySegment, reifiedRelationship, u)
    sig.reifiedRelationshipTargetPropertyPredicates.add(p)
    p.right
  }

  def addReifiedRelationshipTargetPropertyPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipTargetPropertyPredicate
  = {
    val p = ReifiedRelationshipTargetPropertyPredicate(bodySegment, reifiedRelationship, uuid)
    sig.reifiedRelationshipTargetPropertyPredicates.add(p)
    p.right
  }

  def createReifiedRelationshipTargetInversePropertyPredicate
  (a: SWRLObjectPropertyAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipTargetInversePropertyPredicate
  = {
    val u = generateUUID(
      "ReifiedRelationshipTargetInversePropertyPredicate",
      "bodySegment" -> bodySegment.uuid.toString,
      "reifiedRelationship" -> reifiedRelationship.uuid.toString)
    val p = types.terms.ReifiedRelationshipTargetInversePropertyPredicate(bodySegment, reifiedRelationship, u)
    sig.reifiedRelationshipTargetInversePropertyPredicates.add(p)
    p.right
  }

  def addReifiedRelationshipTargetInversePropertyPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   reifiedRelationship: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipTargetInversePropertyPredicate
  = {
    val p = ReifiedRelationshipTargetInversePropertyPredicate(bodySegment, reifiedRelationship, uuid)
    sig.reifiedRelationshipTargetInversePropertyPredicates.add(p)
    p.right
  }

  def createUnreifiedRelationshipPropertyPredicate
  (a: SWRLObjectPropertyAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   unreifiedRelationship: OWLAPIOMF#UnreifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#UnreifiedRelationshipPropertyPredicate
  = {
    val u = generateUUID(
      "UnreifiedRelationshipPropertyPredicate",
      "bodySegment" -> bodySegment.uuid.toString,
      "unreifiedRelationship" -> unreifiedRelationship.uuid.toString)
    val p = types.terms.UnreifiedRelationshipPropertyPredicate(bodySegment, unreifiedRelationship, u)
    sig.unreifiedRelationshipPropertyPredicates.add(p)
    p.right
  }

  def addUnreifiedRelationshipPropertyPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   unreifiedRelationship: OWLAPIOMF#UnreifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#UnreifiedRelationshipPropertyPredicate
  = {
    val p = UnreifiedRelationshipPropertyPredicate(bodySegment, unreifiedRelationship, uuid)
    sig.unreifiedRelationshipPropertyPredicates.add(p)
    p.right
  }

  def createUnreifiedRelationshipInversePropertyPredicate
  (a: SWRLObjectPropertyAtom,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   unreifiedRelationship: OWLAPIOMF#UnreifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#UnreifiedRelationshipInversePropertyPredicate
  = {
    val u = generateUUID(
      "UnreifiedRelationshipInversePropertyPredicate",
      "bodySegment" -> bodySegment.uuid.toString,
      "unreifiedRelationship" -> unreifiedRelationship.uuid.toString)
    val p = types.terms.UnreifiedRelationshipInversePropertyPredicate(bodySegment, unreifiedRelationship, u)
    sig.unreifiedRelationshipInversePropertyPredicates.add(p)
    p.right
  }

  def addUnreifiedRelationshipInversePropertyPredicate
  (uuid: UUID,
   bodySegment: OWLAPIOMF#RuleBodySegment,
   unreifiedRelationship: OWLAPIOMF#UnreifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#UnreifiedRelationshipInversePropertyPredicate
  = {
    val p = UnreifiedRelationshipInversePropertyPredicate(bodySegment, unreifiedRelationship, uuid)
    sig.unreifiedRelationshipInversePropertyPredicates.add(p)
    p.right
  }

  def createEntityConceptSubClassAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#Concept,
   sup: OWLAPIOMF#Concept)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ConceptSpecializationAxiom
  = sig.axioms
    .find {
      case axiom: ConceptSpecializationAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ OWLAPIOMF#ConceptSpecializationAxiom](
    for {
      axiom <- store
        .registerOMFEntityConceptSubClassAxiomInstance(this,
          ConceptSpecializationAxiom(uuid, sub, sup))
    } yield {
      sig.axioms += axiom
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
  : OMFError.Throwables \/ OWLAPIOMF#ConceptSpecializationAxiom
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
   rel: OWLAPIOMF#EntityRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityUniversalRestrictionAxiom
  = for {
    uuid <- entityUniversalRestrictionAxiomUUID(this, sub, rel, range)
    ax <- addEntityDefinitionUniversalRestrictionAxiom(uuid, sub, rel, range)
  } yield ax

  def addEntityDefinitionUniversalRestrictionAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#Entity,
   rel: OWLAPIOMF#EntityRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityUniversalRestrictionAxiom
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
        op = rel match {
          case rr: OWLAPIOMF#ReifiedRelationship =>
            rr.unreified
          case ur: OWLAPIOMF#UnreifiedRelationship =>
            ur.e
        }
        _ <- applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont,
            owlDataFactory
              .getOWLSubClassOfAxiom(
                subC,
                owlDataFactory.getOWLObjectAllValuesFrom(op, rangeC),
                java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid)))),
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
   rel: OWLAPIOMF#EntityRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityExistentialRestrictionAxiom
  = for {
    uuid <- entityExistentialRestrictionAxiomUUID(this, sub, rel, range)
    ax <- addEntityDefinitionExistentialRestrictionAxiom(uuid, sub, rel, range)
  } yield ax

  def addEntityDefinitionExistentialRestrictionAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#Entity,
   rel: OWLAPIOMF#EntityRelationship,
   range: Entity)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityExistentialRestrictionAxiom
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
        op = rel match {
          case rr: OWLAPIOMF#ReifiedRelationship =>
            rr.unreified
          case ur: OWLAPIOMF#UnreifiedRelationship =>
            ur.e
        }
        _ <- store.applyModelTermAxiomChanges(
          axiom,
          "addEntityDefinitionExistentialRestrictionAxiom",
          Seq(
            new AddAxiom(ont,
              owlDataFactory
                .getOWLSubClassOfAxiom(
                  subC,
                  owlDataFactory.getOWLObjectSomeValuesFrom(op, rangeC),
                  java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid))))
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

  def addEntityScalarDataPropertyExistentialRestrictionAxiom
  (uuid: UUID,
   restrictedEntity: OWLAPIOMF#Entity,
   scalarProperty: OWLAPIOMF#EntityScalarDataProperty,
   range: DataRange)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataPropertyExistentialRestrictionAxiom
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
        sig.axioms += axiom
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
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataPropertyUniversalRestrictionAxiom
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
        sig.axioms += axiom
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
   literalValue: LiteralValue,
   valueType: Option[DataRange])
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#EntityScalarDataPropertyParticularRestrictionAxiom
  = (isTypeTermDefinedRecursively(restrictedEntity),
    isTypeTermDefinedRecursively(scalarProperty)) match {
    case (true, true) =>
      val subC = owlDataFactory.getOWLClass(restrictedEntity.iri)
      for {
        axiom <-
        store
          .registerOMFEntityScalarDataPropertyParticularRestrictionAxiomInstance(this,
            EntityScalarDataPropertyParticularRestrictionAxiom(uuid, restrictedEntity, scalarProperty, literalValue, valueType))
        _ <- applyOntologyChangeOrNoOp(
          ontManager,
          new AddAxiom(ont,
            owlDataFactory
              .getOWLSubClassOfAxiom(
                subC,
                owlDataFactory.getOWLDataHasValue(scalarProperty.e,
                  LiteralConversions.toOWLLiteral(literalValue, owlDataFactory,
                    valueType.map(_.e).orElse(Option.apply(scalarProperty.range.e)))),
                java.util.Collections.singleton(createOMFProvenanceAnnotation(uuid)))),
          ifError = {
            "addEntityScalarDataPropertyParticularRestrictionAxiom Error"
          })
      } yield {
        sig.axioms += axiom
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
  : OMFError.Throwables \/ OWLAPIOMF#AspectSpecializationAxiom
  = sig.axioms
    .find {
      case axiom: AspectSpecializationAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ OWLAPIOMF#AspectSpecializationAxiom](
    for {
      axiom <-
      store
        .registerOMFEntityDefinitionAspectSubClassAxiomInstance(this,
          AspectSpecializationAxiom(uuid, sub, sup))
    } yield {
      sig.axioms += axiom
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
  : OMFError.Throwables \/ OWLAPIOMF#AspectSpecializationAxiom
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
   graph: OWLAPIOMF#MutableTerminologyBox,
   entityConceptDesignation: OWLAPIOMF#Concept,
   designationTerminologyGraph: OWLAPIOMF#TerminologyBox)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ConceptDesignationTerminologyAxiom
  = sig.conceptDesignation
    .find {
      case axiom: ConceptDesignationTerminologyAxiom =>
        axiom.designatedConcept == entityConceptDesignation &&
          axiom.designatedTerminology == designationTerminologyGraph
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ ConceptDesignationTerminologyAxiom]({
    val axInstance = ConceptDesignationTerminologyAxiom(
      uuid, graph.uuid, entityConceptDesignation, designationTerminologyGraph)
    for {
      axiom <- store.registerEntityConceptDesignationTerminologyGraphAxiom(this, axInstance)
    } yield {
      sig.conceptDesignation += axiom
      axiom
    }
  }) { other =>
    Set(
      duplicateTerminologyGraphAxiomException(AxiomExceptionKind.EntityConceptDesignationTerminologyGraphAxiomException, other)
    ).left
  }

  def addEntityConceptDesignationTerminologyGraphAxiom
  (uuid: UUID,
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

  def createEntityReifiedRelationshipSubClassAxiom
  (uuid: UUID,
   sub: OWLAPIOMF#ReifiedRelationship,
   sup: OWLAPIOMF#ReifiedRelationship)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipSpecializationAxiom
  = sig.axioms
    .find {
      case axiom: ReifiedRelationshipSpecializationAxiom =>
        axiom.sub == sub && axiom.sup == sup
      case _ =>
        false
    }
    .fold[OMFError.Throwables \/ ReifiedRelationshipSpecializationAxiom]({
    val axInstance = ReifiedRelationshipSpecializationAxiom(uuid, sub, sup)
    for {
      axiom <- store
        .registerOMFEntityReifiedRelationshipSubClassAxiomInstance(this, axInstance)
    } yield {
      sig.axioms += axiom
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
  : OMFError.Throwables \/ OWLAPIOMF#ReifiedRelationshipSpecializationAxiom
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
