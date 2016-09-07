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

import java.util.stream.{Stream => JStream}

import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.AddAxiom
import org.semanticweb.owlapi.model.AddOntologyAnnotation
import org.semanticweb.owlapi.model.OWLAnnotation
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLDataProperty
import org.semanticweb.owlapi.model.OWLEntity
import org.semanticweb.owlapi.model.OWLLiteral

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import scala.compat.java8.FunctionConverters._
import scala.collection.immutable._
import scala.{Boolean,Enumeration,Option}
import scala.Predef.{Set=>_,Map=>_,_}
import scalaz._, Scalaz._

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
  val kind: TerminologyKind,
  val Thing: IRI,
  val Aspect: IRI,
  val Entity: IRI,
  val StructuredDatatype: IRI,
  val ReifiedObjectProperty: IRI,
  val ReifiedStructuredDataProperty: IRI,
  val topObjectProperty: IRI,
  val topReifiedObjectProperty: IRI,
  val topReifiedObjectPropertySource: IRI,
  val topReifiedObjectPropertyTarget: IRI,
  val topReifiedStructuredDataProperty: IRI,
  val topReifiedStructuredDataPropertySource: IRI,
  val topReifiedStructuredDataPropertyTarget: IRI,
  val topDataProperty: IRI )
  extends Backbone( ont ) {

  require(null != kind)
  require(null != Thing)
  require(null != Aspect)
  require(null != Entity)
  require(null != StructuredDatatype)
  require(null != ReifiedObjectProperty)
  require(null != ReifiedStructuredDataProperty)
  require(null != topObjectProperty)
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
   * The IRI of the OWL Class that is the parent of any OMF `ModelEntityAspect` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityAspect
   */
  lazy val AspectC = df.getOWLClass( Aspect )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelEntityConcept` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityConcept
   */
  lazy val EntityC = df.getOWLClass( Entity )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelStructuredDataType` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataType
   */
  lazy val StructuredDatatypeC = df.getOWLClass( StructuredDatatype )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelEntityReifiedRelationship` defined
   * in the ontology representing an OMF modeling domain
    *
    * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityReifiedRelationship
   */
  lazy val ReifiedObjectPropertyC = df.getOWLClass( ReifiedObjectProperty )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelStructuredDataRelationship` defined
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
  def createBackbone( ont: OWLOntology, kind: TerminologyKind, ops: OWLAPIOMFOps )
  : Set[java.lang.Throwable] \/ OMFBackbone = {

    import ops._

    val om = ont.getOWLOntologyManager
    val bIRI = toBackboneIRI( ont.getOntologyID.getOntologyIRI.get )

    for {
      _Thing <- withFragment( bIRI, "Thing" )
      _Aspect <- withFragment( bIRI, "Aspect" )
      _Entity <- withFragment( bIRI, "Entity" )
      _StructuredDatatype <- withFragment( bIRI, "StructuredDatatype" )
      _ReifiedObjectProperty <- withFragment( bIRI, "ReifiedObjectProperty" )
      _ReifiedStructuredDataProperty <- withFragment( bIRI, "ReifiedStructuredDataProperty" )
      _topObjectProperty <- withFragment( bIRI, "topObjectProperty" )
      _topReifiedObjectProperty <- withFragment( bIRI, "topReifiedObjectProperty" )
      _topReifiedObjectPropertySource <- withFragment( bIRI, "topReifiedObjectPropertySource" )
      _topReifiedObjectPropertyTarget <- withFragment( bIRI, "topReifiedObjectPropertyTarget" )
      _topReifiedStructuredDataProperty <- withFragment( bIRI, "topReifiedStructuredDataProperty" )
      _topReifiedStructuredDataPropertySource <- withFragment( bIRI, "topReifiedStructuredDataPropertySource" )
      _topReifiedStructuredDataPropertyTarget <- withFragment( bIRI, "topReifiedStructuredDataPropertyTarget" )
      _topDataProperty <- withFragment( bIRI, "topDataProperty" )
    } yield new OMFBackbone( ont, kind,
      Thing = _Thing,
      Aspect = _Aspect,
      Entity = _Entity,
      StructuredDatatype = _StructuredDatatype,
      ReifiedObjectProperty = _ReifiedObjectProperty,
      ReifiedStructuredDataProperty = _ReifiedStructuredDataProperty,
      topObjectProperty = _topObjectProperty,
      topReifiedObjectProperty = _topReifiedObjectProperty,
      topReifiedObjectPropertySource = _topReifiedObjectPropertySource,
      topReifiedObjectPropertyTarget = _topReifiedObjectPropertyTarget,
      topReifiedStructuredDataProperty = _topReifiedStructuredDataProperty,
      topReifiedStructuredDataPropertySource = _topReifiedStructuredDataPropertySource,
      topReifiedStructuredDataPropertyTarget = _topReifiedStructuredDataPropertyTarget,
      topDataProperty = _topDataProperty ) {

      kind match {
        case _ @ ( `isDefinition` | `isToplevelDefinition` ) =>
          val defP = df.getOWLAnnotationProperty( ops.AnnotationIsDefinition )
          om.applyChange( new AddOntologyAnnotation( ont, df.getOWLAnnotation( defP, df.getOWLLiteral( true ) ) ) )

        case _ @ ( `isDesignation` | `isToplevelDesignation` ) =>
          val desP = df.getOWLAnnotationProperty( ops.AnnotationIsDesignation )
          om.applyChange( new AddOntologyAnnotation( ont, df.getOWLAnnotation( desP, df.getOWLLiteral( true ) ) ) )
      }

      om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( ThingC ) ) )

      for {
        c <- Seq( AspectC, EntityC, ReifiedObjectPropertyC, ReifiedStructuredDataPropertyC, StructuredDatatypeC )
      } {
        om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( c ) ) )
        om.applyChange( new AddAxiom( ont, df.getOWLSubClassOfAxiom( c, ThingC ) ) )
      }

      om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( topObjectPropertyOP ) ) )

      for {
        op <- Seq(
          topReifiedObjectPropertyOP,
          topReifiedObjectPropertySourceOP,
          topReifiedObjectPropertyTargetOP,
          topReifiedStructuredDataPropertyOP,
          topReifiedStructuredDataPropertySourceOP,
          topReifiedStructuredDataPropertyTargetOP )
      } {
        om.applyChange( new AddAxiom( ont,
          df.getOWLDeclarationAxiom( op ) ) )
        om.applyChange( new AddAxiom( ont,
          df.getOWLSubObjectPropertyOfAxiom( op, topObjectPropertyOP ) ) )
      }

      om.applyChange( new AddAxiom( ont,
        df.getOWLFunctionalObjectPropertyAxiom( topReifiedObjectPropertySourceOP ) ) )
      om.applyChange( new AddAxiom( ont,
        df.getOWLFunctionalObjectPropertyAxiom( topReifiedObjectPropertyTargetOP ) ) )
      om.applyChange( new AddAxiom( ont,
        df.getOWLFunctionalObjectPropertyAxiom( topReifiedStructuredDataPropertySourceOP ) ) )
      om.applyChange( new AddAxiom( ont,
        df.getOWLFunctionalObjectPropertyAxiom( topReifiedStructuredDataPropertyTargetOP ) ) )

      om.applyChange( new AddAxiom( ont,
        df.getOWLDeclarationAxiom( topDataPropertyDP ) ) )
    }
  }

  /**
   * @todo needs: annotation:isAbstract
   */
  def resolveBackbone(
    ont: OWLOntology,
    bCs: Set[OWLClass],
    bOPs: Set[OWLObjectProperty],
    bDPs: Set[OWLDataProperty],
    ops: OWLAPIOMFOps )
  : Set[java.lang.Throwable] \/ Backbone = {
    import ops._
    val bIRI = toBackboneIRI(ont.getOntologyID.getOntologyIRI.get)

    def lookup[T <: OWLEntity](fragment: String, set: Set[T])
    : Set[java.lang.Throwable] \/ Option[T] =
      withFragment(bIRI, fragment)
        .flatMap { iri =>
          set
            .find(_.getIRI == iri)
            .right
        }

    val aFilter = (a: OWLAnnotation) => a.getProperty.getIRI == ops.AnnotationIsDesignation

    val mapper = (a: OWLAnnotation) =>
      a.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          JStream.of[Boolean](l.parseBoolean)
        case _ =>
          JStream.of[Boolean]()
      }

    val hasIsDesignation = ont.annotations().
      filter(aFilter.asJava).
      flatMap(mapper.asJava).findFirst()

    val kind: TerminologyKind =
      if (hasIsDesignation.orElse(false)) isDesignation else isDefinition

    for {
      _Thing <- lookup("Thing", bCs)
      _Aspect <- lookup("Aspect", bCs)
      _Entity <- lookup("Entity", bCs)
      _StructuredDatatype <- lookup("StructuredDatatype", bCs)
      _ReifiedObjectProperty <- lookup("ReifiedObjectProperty", bCs)
      _ReifiedStructuredDataProperty <- lookup("ReifiedStructuredDataProperty", bCs)
      _topObjectProperty <- lookup("topObjectProperty", bOPs)
      _topReifiedObjectProperty <- lookup("topReifiedObjectProperty", bOPs)
      _topReifiedObjectPropertySource <- lookup("topReifiedObjectPropertySource", bOPs)
      _topReifiedObjectPropertyTarget <- lookup("topReifiedObjectPropertyTarget", bOPs)
      _topReifiedStructuredDataProperty <- lookup("topReifiedStructuredDataProperty", bOPs)
      _topReifiedStructuredDataPropertySource <- lookup("topReifiedStructuredDataPropertySource", bOPs)
      _topReifiedStructuredDataPropertyTarget <- lookup("topReifiedStructuredDataPropertyTarget", bOPs)
      _topDataProperty <- lookup("topDataProperty", bDPs)
    } yield {
      val b = for {
        t <- _Thing
        a <- _Aspect
        e <- _Entity
        st <- _StructuredDatatype
        rop <- _ReifiedObjectProperty
        rsdp <- _ReifiedStructuredDataProperty
        tOP <- _topObjectProperty
        tROP <- _topReifiedObjectProperty
        tROPs <- _topReifiedObjectPropertySource
        tROPt <- _topReifiedObjectPropertyTarget
        tRSDP <- _topReifiedStructuredDataProperty
        tRSDPs <- _topReifiedStructuredDataPropertySource
        tRSDPt <- _topReifiedStructuredDataPropertyTarget
        tDP <- _topDataProperty
      } yield
        new OMFBackbone(ont, kind,
          Thing = t.getIRI,
          Aspect = a.getIRI,
          Entity = e.getIRI,
          StructuredDatatype = st.getIRI,
          ReifiedObjectProperty = rop.getIRI,
          ReifiedStructuredDataProperty = rsdp.getIRI,
          topObjectProperty = tOP.getIRI,
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