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

package gov.nasa.jpl.omf.scala.binding.owlapi

import gov.nasa.jpl.imce.oml.tables
import org.semanticweb.owlapi.model._
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.TerminologyKind

import scala.collection.immutable._
import scala.{Enumeration, Option}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._
import Scalaz._

/**
  * Used for calculating the declared entities of an OWL ontology
  * to determine whether the result should include or exclude
  * backbone declarations.
  */
object BackboneDeclaractions extends Enumeration {
  type BackboneDeclaractions = Value
  val include, exclude = Value
}

sealed abstract class Backbone( val ont: OWLOntology ) {

  require(null != ont)
}

class NoBackbone( override val ont: OWLOntology ) extends Backbone( ont )

class OMFBackbone
( override val ont: OWLOntology,
  val Thing: IRI,
  val Aspect: IRI,
  val Entity: IRI,
  val StructuredDatatype: IRI,
  val ReifiedObjectProperty: IRI,
  val ReifiedStructuredDataProperty: IRI,
  val topObjectProperty: IRI,
  val topUnreifiedObjectProperty: IRI,
  val topReifiedObjectProperty: IRI,
  val topReifiedObjectPropertySource: IRI,
  val topReifiedObjectPropertyTarget: IRI,
  val topReifiedStructuredDataProperty: IRI,
  val topReifiedStructuredDataPropertySource: IRI,
  val topReifiedStructuredDataPropertyTarget: IRI,
  val topDataProperty: IRI )
  extends Backbone( ont ) {

  require(null != Thing)
  require(null != Aspect)
  require(null != Entity)
  require(null != StructuredDatatype)
  require(null != ReifiedObjectProperty)
  require(null != ReifiedStructuredDataProperty)
  require(null != topObjectProperty)
  require(null != topUnreifiedObjectProperty)
  require(null != topReifiedObjectProperty)
  require(null != topReifiedObjectPropertySource)
  require(null != topReifiedObjectPropertyTarget)
  require(null != topReifiedStructuredDataProperty)
  require(null != topReifiedStructuredDataPropertySource)
  require(null != topReifiedStructuredDataPropertyTarget)
  require(null != topDataProperty)

  val df = ont.getOWLOntologyManager.getOWLDataFactory

  /**
   * The IRI of the OWL Class that is the parent of:
   * - the 3 concrete categories of OMF `ModelEntityDefinition` represented as OWL Classes:
   * -- `ModelEntityAspect`
   *    (direct specializations of the backbone OWL Class `Thing`)
   * -- `ModelEntityConcept`
   *    (indirect specializations via the backbone OWL Class `Entity`)
   * -- `ModelEntityReifiedRelationship`
   *    (indirect specializations via the backbone OWL Class `ReifiedObjectProperty`)
   * - the 2 concrete categories of OMF `ModelDataTypeDefinition` represented as OWL Classes:
   * -- `ModelStructuredDataType`
   *     (indirect specializations via the backbone OWL Class `StructuredDatatype`)
   * -- `ModelStructuredDataRelationship`
   *     (indirect specializations via the backbone OWL Class `ReifiedStructuredDataProperty`)
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityDefinition
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelDataTypeDefinition
   */
  lazy val ThingC = df.getOWLClass( Thing )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `Aspect` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityAspect
   */
  lazy val AspectC = df.getOWLClass( Aspect )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `Concept` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityConcept
   */
  lazy val EntityC = df.getOWLClass( Entity )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `Structure` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataType
   */
  lazy val StructuredDatatypeC = df.getOWLClass( StructuredDatatype )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `ReifiedRelationship` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityReifiedRelationship
   */
  lazy val ReifiedObjectPropertyC = df.getOWLClass( ReifiedObjectProperty )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `StructuredDataProperty` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  lazy val ReifiedStructuredDataPropertyC = df.getOWLClass( ReifiedStructuredDataProperty )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of
   * any category of OMF type represented as an OWL ObjectProperty
   */
  lazy val topObjectPropertyOP = df.getOWLObjectProperty( topObjectProperty )

  /**
    * The IRI of the OWL ObjectProperty that is the parent of
    * any unreified object property chain for an OMF `UnreifiedRelationship` defined
    * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityReifiedRelationship
    */
  lazy val topUnreifiedObjectPropertyOP = df.getOWLObjectProperty( topUnreifiedObjectProperty )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of
   * any unreified object property chain for an OMF `ModelEntityReifiedRelationship` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityReifiedRelationship
   */
  lazy val topReifiedObjectPropertyOP = df.getOWLObjectProperty( topReifiedObjectProperty )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of
   * any unreified object property source for an OMF `ModelEntityReifiedelationship` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityReifiedRelationship
   */
  lazy val topReifiedObjectPropertySourceOP = df.getOWLObjectProperty( topReifiedObjectPropertySource )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of
   * any unreified object property target for an OMF `ModelEntityReifiedRelationship` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityReifiedRelationship
   */
  lazy val topReifiedObjectPropertyTargetOP = df.getOWLObjectProperty( topReifiedObjectPropertyTarget )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of
   * any unreified object property chain for an OMF `ModelStructuredDataRelationship` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  lazy val topReifiedStructuredDataPropertyOP = df.getOWLObjectProperty( topReifiedStructuredDataProperty )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of
   * any unreified object property source for an OMF `ModelStructuredDataRelationship` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  lazy val topReifiedStructuredDataPropertySourceOP = df.getOWLObjectProperty( topReifiedStructuredDataPropertySource )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of
   * any unreified object property target for an OMF `ModelStructuredDataRelationship` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  lazy val topReifiedStructuredDataPropertyTargetOP = df.getOWLObjectProperty( topReifiedStructuredDataPropertyTarget )

  /**
   * The IRI of the OWL DataProperty that is the parent of
   * any data property for an OMF `ModelEntityDataRelationship` defined
   * in the ontology representation of an OMF modeling domain
   */
  lazy val topDataPropertyDP = df.getOWLDataProperty( topDataProperty )

}

object Backbone {

  /**
   * Representing an OMF domain as an OWL ontology involves adopting conventions for
   * distinguishing the OMF role that each OWL entity has.
   * These conventions involve a small vocabulary of OWL entities called
   * the backbone for an OMF domain ontology.
   *
   * @todo needs: annotation:isAbstract, annotation:noMapping
   */
  def createBackbone( ont: OWLOntology, kind: TerminologyKind, isOMLKindAP: OWLAnnotationProperty, ops: OWLAPIOMFOps )
  : Set[java.lang.Throwable] \/ OMFBackbone
  = for {
    b <- createBackbone(ont, ops)
    _ <- applyOntologyChangesOrNoOp(
      ont.getOWLOntologyManager,
      Seq(
        kind match {
          case TerminologyKind.`isOpenWorld` =>
            val defP = b.df.getOWLAnnotationProperty(ops.AnnotationIsTerminologyBoxOpen)
            new AddOntologyAnnotation(ont, b.df.getOWLAnnotation(defP, b.df.getOWLLiteral(true)))

          case TerminologyKind.`isClosedWorld` =>
            val defP = b.df.getOWLAnnotationProperty(ops.AnnotationIsTerminologyBoxOpen)
            new AddOntologyAnnotation(ont, b.df.getOWLAnnotation(defP, b.df.getOWLLiteral(false)))
        },
        new AddOntologyAnnotation(ont, b.df.getOWLAnnotation(isOMLKindAP, b.df.getOWLLiteral(true)))
      ),
      "Error creating backbone ontology")
  } yield b

  def createBackbone( ont: OWLOntology, kind: DescriptionKind, isOMLKindAP: OWLAnnotationProperty, ops: OWLAPIOMFOps )
  : Set[java.lang.Throwable] \/ OMFBackbone
  = for {
    b <- createBackbone(ont, ops)
    _ <- applyOntologyChangesOrNoOp(
      ont.getOWLOntologyManager,
      Seq(
        kind match {
          case DescriptionKind.isPartial =>
            val ap = b.df.getOWLAnnotationProperty(ops.AnnotationIsDescriptionBoxRefinable)
            new AddOntologyAnnotation(ont, b.df.getOWLAnnotation(ap, b.df.getOWLLiteral(true)))

          case DescriptionKind.isFinal =>
            val sp = b.df.getOWLAnnotationProperty(ops.AnnotationIsDescriptionBoxRefinable)
            new AddOntologyAnnotation(ont, b.df.getOWLAnnotation(sp, b.df.getOWLLiteral(false)))
        },
        new AddOntologyAnnotation(ont, b.df.getOWLAnnotation(isOMLKindAP, b.df.getOWLLiteral(true)))
      ),
      "Error creating backbone ontology")
  } yield b

  def createBackbone( ont: OWLOntology, ops: OWLAPIOMFOps )
  : Set[java.lang.Throwable] \/ OMFBackbone = {

    import ops._

    val om = ont.getOWLOntologyManager
    val bIRI = toBackboneIRI( ont.getOntologyID.getOntologyIRI.get )

    for {
      _Thing <- withFragment(bIRI, tables.taggedTypes.localName("Thing"))
      _Aspect <- withFragment(bIRI, tables.taggedTypes.localName("Aspect"))
      _Entity <- withFragment(bIRI, tables.taggedTypes.localName("Entity"))
      _StructuredDatatype <- withFragment(bIRI, tables.taggedTypes.localName("StructuredDatatype"))
      _ReifiedObjectProperty <- withFragment(bIRI, tables.taggedTypes.localName("ReifiedObjectProperty"))
      _ReifiedStructuredDataProperty <- withFragment(bIRI, tables.taggedTypes.localName("ReifiedStructuredDataProperty"))
      _topObjectProperty <- withFragment(bIRI, tables.taggedTypes.localName("topObjectProperty"))
      _topUnreifiedObjectProperty <- withFragment(bIRI, tables.taggedTypes.localName("topUnreifiedObjectProperty"))
      _topReifiedObjectProperty <- withFragment(bIRI, tables.taggedTypes.localName("topReifiedObjectProperty"))
      _topReifiedObjectPropertySource <- withFragment(bIRI, tables.taggedTypes.localName("topReifiedObjectPropertySource"))
      _topReifiedObjectPropertyTarget <- withFragment(bIRI, tables.taggedTypes.localName("topReifiedObjectPropertyTarget"))
      _topReifiedStructuredDataProperty <- withFragment(bIRI, tables.taggedTypes.localName("topReifiedStructuredDataProperty"))
      _topReifiedStructuredDataPropertySource <- withFragment(bIRI, tables.taggedTypes.localName("topReifiedStructuredDataPropertySource"))
      _topReifiedStructuredDataPropertyTarget <- withFragment(bIRI, tables.taggedTypes.localName("topReifiedStructuredDataPropertyTarget"))
      _topDataProperty <- withFragment(bIRI, tables.taggedTypes.localName("topDataProperty"))
      b = new OMFBackbone(ont,
        Thing = _Thing,
        Aspect = _Aspect,
        Entity = _Entity,
        StructuredDatatype = _StructuredDatatype,
        ReifiedObjectProperty = _ReifiedObjectProperty,
        ReifiedStructuredDataProperty = _ReifiedStructuredDataProperty,
        topObjectProperty = _topObjectProperty,
        topUnreifiedObjectProperty = _topUnreifiedObjectProperty,
        topReifiedObjectProperty = _topReifiedObjectProperty,
        topReifiedObjectPropertySource = _topReifiedObjectPropertySource,
        topReifiedObjectPropertyTarget = _topReifiedObjectPropertyTarget,
        topReifiedStructuredDataProperty = _topReifiedStructuredDataProperty,
        topReifiedStructuredDataPropertySource = _topReifiedStructuredDataPropertySource,
        topReifiedStructuredDataPropertyTarget = _topReifiedStructuredDataPropertyTarget,
        topDataProperty = _topDataProperty)
      changes1 =
      Seq(
        new AddAxiom(ont, b.df.getOWLDeclarationAxiom(b.ThingC)),
        new AddAxiom(ont, b.df.getOWLDeclarationAxiom(b.topObjectPropertyOP)),
        new AddAxiom(ont, b.df.getOWLFunctionalObjectPropertyAxiom(b.topReifiedObjectPropertySourceOP)),
        new AddAxiom(ont, b.df.getOWLFunctionalObjectPropertyAxiom(b.topReifiedObjectPropertyTargetOP)),
        new AddAxiom(ont, b.df.getOWLFunctionalObjectPropertyAxiom(b.topReifiedStructuredDataPropertySourceOP)),
        new AddAxiom(ont, b.df.getOWLFunctionalObjectPropertyAxiom(b.topReifiedStructuredDataPropertyTargetOP)),
        new AddAxiom(ont, b.df.getOWLDeclarationAxiom(b.topDataPropertyDP))
      )
      changes2 =
      Seq(
        b.AspectC,
        b.EntityC,
        b.ReifiedObjectPropertyC,
        b.ReifiedStructuredDataPropertyC,
        b.StructuredDatatypeC
      ).flatMap { bc =>
        Seq(
          new AddAxiom(ont, b.df.getOWLDeclarationAxiom(bc)),
          new AddAxiom(ont, b.df.getOWLSubClassOfAxiom(bc, b.ThingC))
        )
      }
      changes3 =
      Seq(
        b.topUnreifiedObjectPropertyOP,
        b.topReifiedObjectPropertyOP,
        b.topReifiedObjectPropertySourceOP,
        b.topReifiedObjectPropertyTargetOP,
        b.topReifiedStructuredDataPropertyOP,
        b.topReifiedStructuredDataPropertySourceOP,
        b.topReifiedStructuredDataPropertyTargetOP
      ).flatMap { bop =>
        Seq(
          new AddAxiom(ont, b.df.getOWLDeclarationAxiom(bop)),
          new AddAxiom(ont, b.df.getOWLSubObjectPropertyOfAxiom(bop, b.topObjectPropertyOP))
        )
      }
      _ <- applyOntologyChangesOrNoOp(om,
        changes1 ++ changes2 ++ changes3,
        "Error creating backbone ontology")
    } yield b
  }

  /**
   * @todo needs: annotation:isAbstract
   */
  def resolveTerminologyBoxBackbone
  ( ont: OWLOntology,
    bCs: Set[OWLClass],
    bOPs: Set[OWLObjectProperty],
    bDPs: Set[OWLDataProperty],
    ops: OWLAPIOMFOps,
    ontOps: OWLOntologyOps)
  : Set[java.lang.Throwable] \/ Backbone = {
    import ops._
    val bIRI = toBackboneIRI(ont.getOntologyID.getOntologyIRI.get)

    def lookup[T <: OWLEntity](fragment: tables.taggedTypes.LocalName, set: Set[T])
    : Set[java.lang.Throwable] \/ Option[T] =
      withFragment(bIRI, fragment)
        .flatMap { iri =>
          set
            .find(_.getIRI == iri)
            .right
        }

    val kind: TerminologyKind = if (ontOps.isOpenWorldDefinitionTerminologyBoxOntology)
      TerminologyKind.isOpenWorld
    else
      TerminologyKind.isClosedWorld

    for {
      _Thing <- lookup(tables.taggedTypes.localName("Thing"), bCs)
      _Aspect <- lookup(tables.taggedTypes.localName("Aspect"), bCs)
      _Entity <- lookup(tables.taggedTypes.localName("Entity"), bCs)
      _StructuredDatatype <- lookup(tables.taggedTypes.localName("StructuredDatatype"), bCs)
      _ReifiedObjectProperty <- lookup(tables.taggedTypes.localName("ReifiedObjectProperty"), bCs)
      _ReifiedStructuredDataProperty <- lookup(tables.taggedTypes.localName("ReifiedStructuredDataProperty"), bCs)
      _topObjectProperty <- lookup(tables.taggedTypes.localName("topObjectProperty"), bOPs)
      _topUnreifiedObjectProperty <- lookup(tables.taggedTypes.localName("topUnreifiedObjectProperty"), bOPs)
      _topReifiedObjectProperty <- lookup(tables.taggedTypes.localName("topReifiedObjectProperty"), bOPs)
      _topReifiedObjectPropertySource <- lookup(tables.taggedTypes.localName("topReifiedObjectPropertySource"), bOPs)
      _topReifiedObjectPropertyTarget <- lookup(tables.taggedTypes.localName("topReifiedObjectPropertyTarget"), bOPs)
      _topReifiedStructuredDataProperty <- lookup(tables.taggedTypes.localName("topReifiedStructuredDataProperty"), bOPs)
      _topReifiedStructuredDataPropertySource <- lookup(tables.taggedTypes.localName("topReifiedStructuredDataPropertySource"), bOPs)
      _topReifiedStructuredDataPropertyTarget <- lookup(tables.taggedTypes.localName("topReifiedStructuredDataPropertyTarget"), bOPs)
      _topDataProperty <- lookup(tables.taggedTypes.localName("topDataProperty"), bDPs)
    } yield {
      val b = for {
        t <- _Thing
        a <- _Aspect
        e <- _Entity
        st <- _StructuredDatatype
        rop <- _ReifiedObjectProperty
        rsdp <- _ReifiedStructuredDataProperty
        tOP <- _topObjectProperty
        tUOP <- _topUnreifiedObjectProperty
        tROP <- _topReifiedObjectProperty
        tROPs <- _topReifiedObjectPropertySource
        tROPt <- _topReifiedObjectPropertyTarget
        tRSDP <- _topReifiedStructuredDataProperty
        tRSDPs <- _topReifiedStructuredDataPropertySource
        tRSDPt <- _topReifiedStructuredDataPropertyTarget
        tDP <- _topDataProperty
      } yield
        new OMFBackbone(ont,
          Thing = t.getIRI,
          Aspect = a.getIRI,
          Entity = e.getIRI,
          StructuredDatatype = st.getIRI,
          ReifiedObjectProperty = rop.getIRI,
          ReifiedStructuredDataProperty = rsdp.getIRI,
          topObjectProperty = tOP.getIRI,
          topUnreifiedObjectProperty = tUOP.getIRI,
          topReifiedObjectProperty = tROP.getIRI,
          topReifiedObjectPropertySource = tROPs.getIRI,
          topReifiedObjectPropertyTarget = tROPt.getIRI,
          topReifiedStructuredDataProperty = tRSDP.getIRI,
          topReifiedStructuredDataPropertySource = tRSDPs.getIRI,
          topReifiedStructuredDataPropertyTarget = tRSDPt.getIRI,
          topDataProperty = tDP.getIRI)

      val r = b.getOrElse(
        new NoBackbone(ont)
      )

      r
    }
  }
}