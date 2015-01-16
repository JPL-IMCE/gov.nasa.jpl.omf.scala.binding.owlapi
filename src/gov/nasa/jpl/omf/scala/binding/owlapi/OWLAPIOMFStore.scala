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

  protected def registerOntologyAsTerminologyGraph(
    o: OWLOntology,
    extendedTGraphs: Iterable[types.ModelTerminologyGraph] = Nil ): Try[types.ModelTerminologyGraph] = {
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

          o.getDirectImports foreach ( registerOntologyAsTerminologyGraph( _ ) match {
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

          val g = new types.ModelTerminologyGraph( importedOrExtendedTGraphs, o )
          tboxGraphs.put( iri.get, g )
          Success( g )
      }
  }

  def loadTerminologyGraph( iri: IRI ): Try[types.ModelTerminologyGraph] =
    try {
      val o = ontManager.loadOntology( iri )
      registerOntologyAsTerminologyGraph( o )
    }
    catch {
      case t: OWLOntologyCreationException =>
        Failure( t.fillInStackTrace )
    }

  def makeTerminologyGraph(
    iri: IRI,
    extendedTGraphs: Iterable[types.ModelTerminologyGraph] ): Try[types.ModelTerminologyGraph] =
    if ( ontManager.contains( iri ) )
      Failure( new IllegalArgumentException( s"An ontology with iri='${iri}' already exists" ) )
    else
      for {
        b <- Backbone.createBackbone( ontManager.createOntology( iri ), omfModule.ops )
        g <- registerOntologyAsTerminologyGraph( b.ont, extendedTGraphs )
      } yield g

  def addStructuredDataType(
    graph: types.ModelTerminologyGraph,
    fragment: String ): Try[types.ModelStructuredDataType] = ???

  def addEntityConceptRestrictionAxiom(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelEntityConcept,
    rel: types.ModelEntityRelationship,
    range: types.ModelEntityDefinition ): Try[types.EntityConceptRestrictionAxiom] = ???

  def addEntityRelationshipSubClassAxiom(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelEntityRelationship,
    sup: types.ModelEntityRelationship ): Try[types.EntityRelationshipSubClassAxiom] = ???

  def addScalarDataTypeFacetRestriction(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelScalarDataType,
    sup: types.ModelScalarDataType,
    restrictions: Iterable[ConstrainingFacet] ): Try[types.ScalarDataTypeFacetRestriction] = ???

  def loadInstanceGraph( iri: IRI ): Try[instances.ModelInstanceGraph] = ???

  def makeInstanceGraph(
    iri: IRI,
    instantiatedTGraphs: Iterable[types.ModelTerminologyGraph],
    extendedIGraphs: Iterable[instances.ModelInstanceGraph] ): Try[instances.ModelInstanceGraph] = ???

  def addInstanceObject(
    graph: instances.ModelInstanceGraph,
    conceptType: types.ModelEntityConcept,
    fragment: String ): Try[instances.ModelInstanceObject] = ???

  def addInstanceRelation(
    graph: instances.ModelInstanceGraph,
    relationshipType: types.ModelEntityRelationship,
    source: instances.ModelEntityInstance,
    target: instances.ModelEntityInstance,
    fragment: String ): Try[instances.ModelInstanceRelation] = ???

  def addDataLiteral(
    graph: instances.ModelInstanceGraph,
    datatype: types.ModelScalarDataType,
    lexicalForm: String ): Try[instances.ModelInstanceDataLiteral] = ???

  def addDataStructure(
    graph: instances.ModelInstanceGraph,
    datatype: types.ModelStructuredDataType,
    fragment: String ): Try[instances.ModelInstanceDataStructure] = ???

}