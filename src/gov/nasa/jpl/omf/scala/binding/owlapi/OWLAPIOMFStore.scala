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

import scala.Iterable
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.semanticweb.owlapi.model.AddImport
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyAlreadyExistsException
import org.semanticweb.owlapi.model.OWLOntologyCreationException
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper
import org.semanticweb.owlapi.model.OWLOntologyManager

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._

case class OWLAPIOMFStore( val omfModule: OWLAPIOMFModule, val ontManager: OWLOntologyManager ) {

  val catalogIRIMapper: Option[CatalogIRIMapper] =
    for {
      catalogManager <- omfModule.catalogManager
    } yield {
      val mapper = new CatalogIRIMapper( catalogManager )
      ontManager.getIRIMappers.add( Iterable[OWLOntologyIRIMapper]( mapper ).asJava )
      mapper
    }

  val tboxGraphs = scala.collection.mutable.HashMap[IRI, types.ModelTerminologyGraph]()

  protected def registerImmutableOntologyAsTerminologyGraph(
    o: OWLOntology,
    extendedTGraphs: Iterable[types.ModelTerminologyGraph] = Nil )( implicit ops: OWLAPIOMFOps ): Try[types.ModelTerminologyGraph] = {
    val iri = o.getOntologyID.getOntologyIRI
    if ( !iri.isPresent )
      Failure( new IllegalArgumentException( "An ontology must have an OntologyID with an Ontology IRI" ) )
    else
      tboxGraphs.get( iri.get ) match {
        case Some( g ) =>
          // already registered.
          Success( g )

        case None =>
          // not yet registered.

          var importedOrExtendedTGraphs = scala.collection.mutable.HashSet[types.ModelTerminologyGraph]()

          o.getDirectImports foreach ( registerImmutableOntologyAsTerminologyGraph( _ ) match {
            case Failure( t ) =>
              return Failure( t )

            case Success( ig ) =>
              importedOrExtendedTGraphs.add( ig )
          } )

          extendedTGraphs.foreach { tg =>
            tboxGraphs.get( tg.iri ) match {
              case None => return Failure( new IllegalArgumentException(
                s"Cannot create an ontology with iri='${iri.get}' extending a foreign terminology graph, '${tg.iri}' not managed by this ontology manager" ) )
              case Some( eg ) =>
                importedOrExtendedTGraphs.add( eg )
                val decl = ontManager.getOWLDataFactory.getOWLImportsDeclaration( tg.iri )
                ontManager.applyChange( new AddImport( o, decl ) )
            }
          }

          for {
            g <- types.ImmutableModelTerminologyGraph.resolve(importedOrExtendedTGraphs, o)
          } yield {
            tboxGraphs.put( iri.get, g )
            g
          }
      }
  }
  
  protected def registerMutableOntologyAsTerminologyGraph(
    o: OWLOntology,
    extendedTGraphs: Iterable[types.ModelTerminologyGraph] = Nil )( implicit ops: OWLAPIOMFOps ): Try[types.ModelTerminologyGraph] = {
    val iri = o.getOntologyID.getOntologyIRI
    if ( !iri.isPresent )
      Failure( new IllegalArgumentException( "An ontology must have an OntologyID with an Ontology IRI" ) )
    else
      tboxGraphs.get( iri.get ) match {
        case Some( g ) =>
          // already registered.
          Success( g )

        case None =>
          // not yet registered.

          var importedOrExtendedTGraphs = scala.collection.mutable.HashSet[types.ModelTerminologyGraph]()

          extendedTGraphs.foreach { tg =>
            tboxGraphs.get( tg.iri ) match {
              case None => return Failure( new IllegalArgumentException(
                s"Cannot create an ontology with iri='${iri.get}' extending a foreign terminology graph, '${tg.iri}' not managed by this ontology manager" ) )
              case Some( eg ) =>
                importedOrExtendedTGraphs.add( eg )
                val decl = ontManager.getOWLDataFactory.getOWLImportsDeclaration( tg.iri )
                ontManager.applyChange( new AddImport( o, decl ) )
            }
          }

          val g = new types.MutableModelTerminologyGraph( importedOrExtendedTGraphs, o )
          tboxGraphs.put( iri.get, g )
          Success( g )
      }
  }
  
  def saveTerminologyGraph( g: types.ModelTerminologyGraph )( implicit ops: OWLAPIOMFOps ): Try[Unit] = 
    catalogIRIMapper match {
    case None => Failure( new IllegalArgumentException( s"Cannot save a terminology graph without a catalog IRI mapper"))
    case Some( iriMapper ) =>
      val iri = iriMapper.resolveIRI( g.iri, iriMapper.saveResolutionStrategy(_) )
      g.save( iri )
  }
  
  def loadTerminologyGraph( iri: IRI )( implicit ops: OWLAPIOMFOps ): Try[types.ModelTerminologyGraph] =
    try {
      val o = ontManager.loadOntology( iri )
      registerImmutableOntologyAsTerminologyGraph( o )
    }
    catch {
      case t: OWLOntologyCreationException =>
        Failure( t.fillInStackTrace )
    }

  def makeTerminologyGraph(
    iri: IRI,
    extendedTGraphs: Iterable[types.ModelTerminologyGraph] )( implicit ops: OWLAPIOMFOps ): Try[types.ModelTerminologyGraph] =
    if ( ontManager.contains( iri ) )
      Failure( new IllegalArgumentException( s"An ontology with iri='${iri}' already exists" ) )
    else
      for {
        b <- Backbone.createBackbone( ontManager.createOntology( iri ), omfModule.ops )
        g <- registerMutableOntologyAsTerminologyGraph( b.ont, extendedTGraphs )
      } yield g

    
  def loadInstanceGraph( iri: IRI ): Try[instances.ModelInstanceGraph] = ???

  def makeInstanceGraph(
    iri: IRI,
    instantiatedTGraphs: Iterable[types.ModelTerminologyGraph],
    extendedIGraphs: Iterable[instances.ModelInstanceGraph] ): Try[instances.ModelInstanceGraph] = ???

}