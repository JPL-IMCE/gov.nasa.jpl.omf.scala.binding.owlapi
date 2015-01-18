/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.binding.owlapi

import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.IRI
import scala.util.Try
import scala.util.Success
import org.semanticweb.owlapi.model.AddAxiom
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLDataProperty
import scala.util.Failure
import org.semanticweb.owlapi.model.OWLEntity

class Backbone( val ont: OWLOntology ) {

  val df = ont.getOWLOntologyManager.getOWLDataFactory
  
  /**
   * The IRI of the OWL Class that is the parent of:
   * - the 3 concrete categories of OMF `ModelEntityDefinition` represented as OWL Classes:
   * -- `ModelEntityAspect` (direct specializations of the backbone OWL Class `Thing`)
   * -- `ModelEntityConcept` (indirect specializations via the backbone OWL Class `Entity`)
   * -- `ModelEntityRelationship` (indirect specializations via the backbone OWL Class `ReifiedObjectProperty`)
   * - the 2 concrete categories of OMF `ModelDataTypeDefinition` represented as OWL Classes:
   * -- `ModelStructuredDataType` (indirect specializations via the backbone OWL Class `StructuredDatatype`)
   * -- `ModelStructuredDataRelationship` (indirect specializations via the backbone OWL Class `ReifiedStructuredDataProperty`)
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityDefinition
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelDataTypeDefinition
   */
  val Thing: Option[IRI] = None

  lazy val ThingC = for { iri <- Thing } yield df.getOWLClass( iri )
  
  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelEntityConcept` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityConcept
   */
  val Entity: Option[IRI] = None

  lazy val EntityC = for { iri <- Entity } yield df.getOWLClass( iri )
  
  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelStructuredDataType` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataType
   */
  val StructuredDatatype: Option[IRI] = None
  
  lazy val StructuredDatatypeC = for { iri <- StructuredDatatype } yield df.getOWLClass( iri )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelEntityRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityRelationship
   */
  val ReifiedObjectProperty: Option[IRI] = None
  
  lazy val ReifiedObjectPropertyC = for { iri <- ReifiedObjectProperty } yield df.getOWLClass( iri )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelStructuredDataRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  val ReifiedStructuredDataProperty: Option[IRI] = None
  
  lazy val ReifiedStructuredDataPropertyC = for { iri <- ReifiedStructuredDataProperty } yield df.getOWLClass( iri )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any category of OMF type represented as an OWL ObjectProperty
   */
  val topObjectProperty: Option[IRI] = None
  
  lazy val topObjectPropertyOP = for { iri <- topObjectProperty } yield df.getOWLObjectProperty( iri )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property chain for an OMF `ModelEntityRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityRelationship
   */
  val topReifiedObjectProperty: Option[IRI] = None
  
  lazy val topReifiedObjectPropertyOP = for { iri <- topReifiedObjectProperty } yield df.getOWLObjectProperty( iri )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property source for an OMF `ModelEntityRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityRelationship
   */
  val topReifiedObjectPropertySource: Option[IRI] = None
  
  lazy val topReifiedObjectPropertySourceOP = for { iri <- topReifiedObjectPropertySource } yield df.getOWLObjectProperty( iri )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property target for an OMF `ModelEntityRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityRelationship
   */
  val topReifiedObjectPropertyTarget: Option[IRI] = None
  
  lazy val topReifiedObjectPropertyTargetOP = for { iri <- topReifiedObjectPropertyTarget } yield df.getOWLObjectProperty( iri )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property chain for an OMF `ModelStructuredDataRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  val topReifiedStructuredDataProperty: Option[IRI] = None
  
  lazy val topReifiedStructuredDataPropertyOP = for { iri <- topReifiedStructuredDataProperty } yield df.getOWLObjectProperty( iri )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property source for an OMF `ModelStructuredDataRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  val topReifiedStructuredDataPropertySource: Option[IRI] = None
  
  lazy val topReifiedStructuredDataPropertySourceOP = for { iri <- topReifiedStructuredDataPropertySource } yield df.getOWLObjectProperty( iri )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property target for an OMF `ModelStructuredDataRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  val topReifiedStructuredDataPropertyTarget: Option[IRI] = None
  
  lazy val topReifiedStructuredDataPropertyTargetOP = for { iri <- topReifiedStructuredDataPropertyTarget } yield df.getOWLObjectProperty( iri )

  /**
   * The IRI of the OWL DataProperty that is the parent of any data property for an OMF `ModelEntityDataRelationship` defined in the ontology representation of an OMF modeling domain
   */
  val topDataProperty: Option[IRI] = None
  
  lazy val topDataPropertyDP = for { iri <- topDataProperty } yield df.getOWLDataProperty( iri )

  lazy val isBackboneDefined: Boolean = Seq(
    Thing, Entity, StructuredDatatype, ReifiedObjectProperty, ReifiedStructuredDataProperty,
    topDataProperty, topObjectProperty,
    topReifiedObjectProperty, topReifiedObjectPropertySource, topReifiedObjectPropertyTarget,
    topReifiedStructuredDataProperty, topReifiedStructuredDataPropertySource, topReifiedStructuredDataPropertyTarget ) forall ( _.isDefined )
}

object Backbone {

  /**
   * Representing an OMF domain as an OWL ontology involves adopting conventions for distinguishing the OMF role that each OWL entity has.
   * These conventions involve a small vocabulary of OWL entities called the backbone for an OMF domain ontology.
   *
   * @TODO needs: annotation:isAbstract, annotation:noMapping
   */
  def createBackbone( ont: OWLOntology, ops: OWLAPIOMFOps ): Try[Backbone] = {

    import ops._

    val om = ont.getOWLOntologyManager
    val df = om.getOWLDataFactory
    val bIRI = toBackboneIRI( ont.getOntologyID.getOntologyIRI.get )

    for {
      _Thing <- withFragment( bIRI, "Thing" )
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
    } yield new Backbone( ont ) {
      override val Thing = Some( _Thing )
      override val Entity = Some( _Entity )
      override val StructuredDatatype = Some( _StructuredDatatype )
      override val ReifiedObjectProperty = Some( _ReifiedObjectProperty )
      override val ReifiedStructuredDataProperty = Some( _ReifiedStructuredDataProperty )
      override val topObjectProperty = Some( _topObjectProperty )
      override val topReifiedObjectProperty = Some( _topReifiedObjectProperty )
      override val topReifiedObjectPropertySource = Some( _topReifiedObjectPropertySource )
      override val topReifiedObjectPropertyTarget = Some( _topReifiedObjectPropertyTarget )
      override val topReifiedStructuredDataProperty = Some( _topReifiedStructuredDataProperty )
      override val topReifiedStructuredDataPropertySource = Some( _topReifiedStructuredDataPropertySource )
      override val topReifiedStructuredDataPropertyTarget = Some( _topReifiedStructuredDataPropertyTarget )
      override val topDataProperty = Some( _topDataProperty )

      val _ThingC = df.getOWLClass( _Thing )
      val _EntityC = df.getOWLClass( _Entity )
      val _ReifiedObjectPropertyC = df.getOWLClass( _ReifiedObjectProperty )
      val _ReifiedStructuredDataPropertyC = df.getOWLClass( _ReifiedStructuredDataProperty )
      val _StructuredDatatypeC = df.getOWLClass( _StructuredDatatype )

      val _topObjectPropertyOP = df.getOWLObjectProperty( _topObjectProperty )
      val _topReifiedObjectPropertyOP = df.getOWLObjectProperty( _topReifiedObjectProperty )
      val _topReifiedObjectPropertySourceOP = df.getOWLObjectProperty( _topReifiedObjectPropertySource )
      val _topReifiedObjectPropertyTargetOP = df.getOWLObjectProperty( _topReifiedObjectPropertyTarget )
      val _topReifiedStructuredDataPropertyOP = df.getOWLObjectProperty( _topReifiedStructuredDataProperty )
      val _topReifiedStructuredDataPropertySourceOP = df.getOWLObjectProperty( _topReifiedStructuredDataPropertySource )
      val _topReifiedStructuredDataPropertyTargetOP = df.getOWLObjectProperty( _topReifiedStructuredDataPropertyTarget )

      val _topDataPropertyDP = df.getOWLDataProperty( _topDataProperty )

      om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( _ThingC ) ) )

      for {
        c <- Seq( _EntityC, _ReifiedObjectPropertyC, _ReifiedStructuredDataPropertyC, _StructuredDatatypeC )
      } {
        om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( c ) ) )
        om.applyChange( new AddAxiom( ont, df.getOWLSubClassOfAxiom( c, _ThingC ) ) )
      }

      om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( _topObjectPropertyOP ) ) )

      for {
        op <- Seq(
          _topReifiedObjectPropertyOP, _topReifiedObjectPropertySourceOP, _topReifiedObjectPropertyTargetOP,
          _topReifiedStructuredDataPropertyOP, _topReifiedStructuredDataPropertySourceOP, _topReifiedStructuredDataPropertyTargetOP )
      } {
        om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( op ) ) )
        om.applyChange( new AddAxiom( ont, df.getOWLSubObjectPropertyOfAxiom( op, _topObjectPropertyOP ) ) )
      }

      om.applyChange( new AddAxiom( ont, df.getOWLFunctionalObjectPropertyAxiom( _topReifiedObjectPropertySourceOP ) ) )
      om.applyChange( new AddAxiom( ont, df.getOWLFunctionalObjectPropertyAxiom( _topReifiedObjectPropertyTargetOP ) ) )
      om.applyChange( new AddAxiom( ont, df.getOWLFunctionalObjectPropertyAxiom( _topReifiedStructuredDataPropertySourceOP ) ) )
      om.applyChange( new AddAxiom( ont, df.getOWLFunctionalObjectPropertyAxiom( _topReifiedStructuredDataPropertyTargetOP ) ) )

      om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( _topDataPropertyDP ) ) )
    }

  }

  /**
   * @TODO needs: annotation:isAbstract
   */
  def resolveBackbone(
    ont: OWLOntology,
    bCs: Set[OWLClass],
    bOPs: Set[OWLObjectProperty],
    bDPs: Set[OWLDataProperty],
    ops: OWLAPIOMFOps ): Try[Backbone] = {
    import ops._
    val bIRI = toBackboneIRI( ont.getOntologyID.getOntologyIRI.get )
    
    def lookup[T <: OWLEntity]( fragment: String, set: Set[T]): Option[T] = {    
      val iri = withFragment( bIRI, fragment ).get
      set.find( _.getIRI == iri ) 
    }
    
    val b = for {
      _Thing <- lookup( "Thing", bCs )
      _Entity <- lookup( "Entity", bCs )
      _StructuredDatatype <- lookup( "StructuredDatatype", bCs )
      _ReifiedObjectProperty <- lookup( "ReifiedObjectProperty", bCs )
      _ReifiedStructuredDataProperty <- lookup( "ReifiedStructuredDataProperty", bCs )
      _topObjectProperty <- lookup( "topObjectProperty" , bOPs )
      _topReifiedObjectProperty <- lookup( "topReifiedObjectProperty", bOPs )
      _topReifiedObjectPropertySource <- lookup( "topReifiedObjectPropertySource", bOPs )
      _topReifiedObjectPropertyTarget <- lookup( "topReifiedObjectPropertyTarget", bOPs )
      _topReifiedStructuredDataProperty <- lookup( "topReifiedStructuredDataProperty", bOPs )
      _topReifiedStructuredDataPropertySource <- lookup( "topReifiedStructuredDataPropertySource", bOPs )
      _topReifiedStructuredDataPropertyTarget <- lookup( "topReifiedStructuredDataPropertyTarget", bOPs )
      _topDataProperty <- lookup( "topDataProperty", bDPs )
    } yield new Backbone( ont ) {
      override val Thing = Some( _Thing.getIRI )
      override val Entity = Some( _Entity.getIRI )
      override val StructuredDatatype = Some( _StructuredDatatype.getIRI )
      override val ReifiedObjectProperty = Some( _ReifiedObjectProperty.getIRI )
      override val ReifiedStructuredDataProperty = Some( _ReifiedStructuredDataProperty.getIRI )
      override val topObjectProperty = Some( _topObjectProperty.getIRI )
      override val topReifiedObjectProperty = Some( _topReifiedObjectProperty.getIRI )
      override val topReifiedObjectPropertySource = Some( _topReifiedObjectPropertySource.getIRI )
      override val topReifiedObjectPropertyTarget = Some( _topReifiedObjectPropertyTarget.getIRI )
      override val topReifiedStructuredDataProperty = Some( _topReifiedStructuredDataProperty.getIRI )
      override val topReifiedStructuredDataPropertySource = Some( _topReifiedStructuredDataPropertySource.getIRI )
      override val topReifiedStructuredDataPropertyTarget = Some( _topReifiedStructuredDataPropertyTarget.getIRI )
      override val topDataProperty = Some( _topDataProperty.getIRI )
    }
    b match {
      case Some( backbone ) => Success( backbone )
      case None             => Success( new Backbone( ont ) )
    }
  }
}