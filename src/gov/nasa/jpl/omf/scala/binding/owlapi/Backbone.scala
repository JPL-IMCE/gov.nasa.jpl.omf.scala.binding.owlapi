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

sealed abstract class Backbone( val ont: OWLOntology )

class NoBackbone( override val ont: OWLOntology ) extends Backbone( ont )

class OMFBackbone( 
    override val ont: OWLOntology,
    val Thing: IRI,
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
    val topDataProperty: IRI
    ) extends Backbone( ont ) {

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
  lazy val ThingC = df.getOWLClass( Thing )
  
  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelEntityConcept` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityConcept
   */
  lazy val EntityC = df.getOWLClass( Entity )
  
  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelStructuredDataType` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataType
   */
  lazy val StructuredDatatypeC = df.getOWLClass( StructuredDatatype )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelEntityRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityRelationship
   */
  lazy val ReifiedObjectPropertyC = df.getOWLClass( ReifiedObjectProperty )

  /**
   * The IRI of the OWL Class that is the parent of any OMF `ModelStructuredDataRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  lazy val ReifiedStructuredDataPropertyC = df.getOWLClass( ReifiedStructuredDataProperty )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any category of OMF type represented as an OWL ObjectProperty
   */
  lazy val topObjectPropertyOP = df.getOWLObjectProperty( topObjectProperty )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property chain for an OMF `ModelEntityRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityRelationship
   */
  lazy val topReifiedObjectPropertyOP = df.getOWLObjectProperty( topReifiedObjectProperty )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property source for an OMF `ModelEntityRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityRelationship
   */
  lazy val topReifiedObjectPropertySourceOP = df.getOWLObjectProperty( topReifiedObjectPropertySource )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property target for an OMF `ModelEntityRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelEntityRelationship
   */
  lazy val topReifiedObjectPropertyTargetOP = df.getOWLObjectProperty( topReifiedObjectPropertyTarget )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property chain for an OMF `ModelStructuredDataRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  lazy val topReifiedStructuredDataPropertyOP = df.getOWLObjectProperty( topReifiedStructuredDataProperty )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property source for an OMF `ModelStructuredDataRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  lazy val topReifiedStructuredDataPropertySourceOP = df.getOWLObjectProperty( topReifiedStructuredDataPropertySource )

  /**
   * The IRI of the OWL ObjectProperty that is the parent of any unreified object property target for an OMF `ModelStructuredDataRelationship` defined in the ontology representing an OMF modeling domain
   * @see gov.nasa.jpl.omf.scala.core.OMF#ModelStructuredDataRelationship
   */
  lazy val topReifiedStructuredDataPropertyTargetOP = df.getOWLObjectProperty( topReifiedStructuredDataPropertyTarget )

  /**
   * The IRI of the OWL DataProperty that is the parent of any data property for an OMF `ModelEntityDataRelationship` defined in the ontology representation of an OMF modeling domain
   */
  lazy val topDataPropertyDP = df.getOWLDataProperty( topDataProperty )

}

object Backbone {

  /**
   * Representing an OMF domain as an OWL ontology involves adopting conventions for distinguishing the OMF role that each OWL entity has.
   * These conventions involve a small vocabulary of OWL entities called the backbone for an OMF domain ontology.
   *
   * @TODO needs: annotation:isAbstract, annotation:noMapping
   */
  def createBackbone( ont: OWLOntology, ops: OWLAPIOMFOps ): Try[OMFBackbone] = {

    import ops._

    val om = ont.getOWLOntologyManager
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
    } yield new OMFBackbone( ont,
      Thing = _Thing,
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

      om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( ThingC ) ) )

      for {
        c <- Seq( EntityC, ReifiedObjectPropertyC, ReifiedStructuredDataPropertyC, StructuredDatatypeC )
      } {
        om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( c ) ) )
        om.applyChange( new AddAxiom( ont, df.getOWLSubClassOfAxiom( c, ThingC ) ) )
      }

      om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( topObjectPropertyOP ) ) )

      for {
        op <- Seq(
          topReifiedObjectPropertyOP, topReifiedObjectPropertySourceOP, topReifiedObjectPropertyTargetOP,
          topReifiedStructuredDataPropertyOP, topReifiedStructuredDataPropertySourceOP, topReifiedStructuredDataPropertyTargetOP )
      } {
        om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( op ) ) )
        om.applyChange( new AddAxiom( ont, df.getOWLSubObjectPropertyOfAxiom( op, topObjectPropertyOP ) ) )
      }

      om.applyChange( new AddAxiom( ont, df.getOWLFunctionalObjectPropertyAxiom( topReifiedObjectPropertySourceOP ) ) )
      om.applyChange( new AddAxiom( ont, df.getOWLFunctionalObjectPropertyAxiom( topReifiedObjectPropertyTargetOP ) ) )
      om.applyChange( new AddAxiom( ont, df.getOWLFunctionalObjectPropertyAxiom( topReifiedStructuredDataPropertySourceOP ) ) )
      om.applyChange( new AddAxiom( ont, df.getOWLFunctionalObjectPropertyAxiom( topReifiedStructuredDataPropertyTargetOP ) ) )

      om.applyChange( new AddAxiom( ont, df.getOWLDeclarationAxiom( topDataPropertyDP ) ) )
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
    } yield new OMFBackbone( ont,
      Thing = _Thing.getIRI,
      Entity = _Entity.getIRI,
      StructuredDatatype = _StructuredDatatype.getIRI,
      ReifiedObjectProperty = _ReifiedObjectProperty.getIRI,
      ReifiedStructuredDataProperty = _ReifiedStructuredDataProperty.getIRI,
      topObjectProperty = _topObjectProperty.getIRI,
      topReifiedObjectProperty = _topReifiedObjectProperty.getIRI,
      topReifiedObjectPropertySource = _topReifiedObjectPropertySource.getIRI,
      topReifiedObjectPropertyTarget = _topReifiedObjectPropertyTarget.getIRI,
      topReifiedStructuredDataProperty = _topReifiedStructuredDataProperty.getIRI,
      topReifiedStructuredDataPropertySource = _topReifiedStructuredDataPropertySource.getIRI,
      topReifiedStructuredDataPropertyTarget = _topReifiedStructuredDataPropertyTarget.getIRI,
      topDataProperty = _topDataProperty.getIRI )
    
    b match {
      case Some( backbone ) => Success( backbone )
      case None             => Success( new NoBackbone( ont ) )
    }
  }
}