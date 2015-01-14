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

import java.net.URI

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.binding._
import gov.nasa.jpl.omf.scala.binding.owlapi._

import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntologyManager

import scala.util.Try
import scala.util.Success
import scala.util.Failure

class OWLAPIOMFOps extends OMFOps[OWLAPIOMF] {

  // IRI

  override def makeIRI( s: String ) = org.semanticweb.owlapi.model.IRI.create( s )

  override def withFragment( iri: IRI, fragment: String ) = {
    val u = iri.toURI
    val f = u.getFragment
    if ( f != null || f.nonEmpty ) Failure( new IllegalArgumentException( s" withFragment(iri, fragment) -- the iri must not have a fragment: ${iri}" ) )
    else Success( org.semanticweb.owlapi.model.IRI.create( iri.toURI.resolve( "#" + fragment ) ) )
  }

  override def splitIRI( iri: IRI ) = {
    val u = iri.toURI
    val f = u.getFragment
    if ( f == null || f.isEmpty() ) ( iri, None )
    else ( org.semanticweb.owlapi.model.IRI.create( new URI( u.getScheme, u.getSchemeSpecificPart, null ) ), Some( f ) )
  }

  override def fromIRI( iri: IRI ) = iri.toString

  override def toBackboneIRI( iri: IRI ) = {
    val u = iri.toURI
    import u._
    org.semanticweb.owlapi.model.IRI.create( new URI( getScheme, getUserInfo, getHost, getPort, "/backbone" + getPath, getQuery, null ) )
  }

  override def toObjectPropertyIRI( iri: IRI ) = {
    val u = iri.toURI
    val f = u.getFragment
    require( f != null && f.nonEmpty )
    val fragment = f( 0 ).toLower + f.drop( 1 )
    import u._
    org.semanticweb.owlapi.model.IRI.create( new URI( getScheme, getUserInfo, getHost, getPort, getPath, getQuery, fragment ) )
  }

  override def toSourceIRI( iri: IRI ) = {
    val u = iri.toURI
    val f = u.getFragment
    require( f != null && f.nonEmpty )
    val fragment = s"has${f}Source"
    import u._
    org.semanticweb.owlapi.model.IRI.create( new URI( getScheme, getUserInfo, getHost, getPort, getPath, getQuery, fragment ) )
  }

  override def toTargetIRI( iri: IRI ) = {
    val u = iri.toURI
    val f = u.getFragment
    require( f != null && f.nonEmpty )
    val fragment = s"has${f}Target"
    import u._
    org.semanticweb.owlapi.model.IRI.create( new URI( getScheme, getUserInfo, getHost, getPort, getPath, getQuery, fragment ) )
  }

  // terminology graph

  override def loadTerminologyGraph( iri: IRI )( implicit store: OWLAPIOMFStore ) =
    store.loadTerminologyGraph( iri )

  override def makeTerminologyGraph(
    iri: IRI,
    extendedTGraphs: Iterable[types.ModelTerminologyGraph] )( implicit store: OWLAPIOMFStore ) =
    store.makeTerminologyGraph( iri, extendedTGraphs )

  override def getTerminologyGraphIRI( graph: types.ModelTerminologyGraph ) = graph.iri

  override def fromTerminologyGraph( graph: types.ModelTerminologyGraph ) =
    graph.fromTerminologyGraph

  override def lookupTypeTerm( graph: types.ModelTerminologyGraph, iri: IRI ) =
    graph.lookupTypeTerm( iri )

  override def lookupEntityDefinition( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelEntityDefinition ) => Some( t )
      case _                                      => None
    }

  override def lookupEntityConcept( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelEntityConcept ) => Some( t )
      case _                                   => None
    }

  override def lookupEntityRelationship( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelEntityRelationship ) => Some( t )
      case _                                        => None
    }

  override def lookupScalarDataType( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelScalarDataType ) => Some( t )
      case _                                    => None
    }

  override def lookupStructuredDataType( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelStructuredDataType ) => Some( t )
      case _                                        => None
    }

  override def lookupStructuredDataRelationship( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelStructuredDataRelationship ) => Some( t )
      case _ => None
    }

  override def lookupEntityDataRelationship( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelEntityDataRelationship ) => Some( t )
      case _ => None
    }

  override def getTerms( graph: types.ModelTerminologyGraph ) =
    graph.getTerms

  def foldTerm[T]( t: types.ModelTypeTerm )(
    funEntityConcept: types.ModelEntityConcept => T,
    funEntityRelationship: types.ModelEntityRelationship => T,
    funScalarDataType: types.ModelScalarDataType => T,
    funStructuredDataType: types.ModelStructuredDataType => T,
    funStructuredDataRelationship: types.ModelStructuredDataRelationship => T,
    funEntityDataRelationship: types.ModelEntityDataRelationship => T ): T = t match {
    case et: types.ModelEntityConcept               => funEntityConcept( et )
    case et: types.ModelEntityRelationship          => funEntityRelationship( et )
    case ed: types.ModelScalarDataType              => funScalarDataType( ed )
    case ed: types.ModelStructuredDataType          => funStructuredDataType( ed )
    case sdr: types.ModelStructuredDataRelationship => funStructuredDataRelationship( sdr )
    case edr: types.ModelEntityDataRelationship     => funEntityDataRelationship( edr )
  }

  override def fromEntityDefinition( e: types.ModelEntityDefinition ) = e match {
    case ec: types.ModelEntityConcept      => fromEntityConcept( ec )
    case er: types.ModelEntityRelationship => fromEntityRelationship( er )._1
  }

  // type facet
  
  override def addEntityAspect(graph: types.ModelTerminologyGraph, facetName: String)(implicit store: OWLAPIOMFStore): Try[types.ModelEntityAspect] =
    store.addEntityAspect( graph, facetName )
    
  override def fromEntityAspect( f: types.ModelEntityAspect ): IRI = 
    f.iri
  
  // entity concept

  override def addEntityConcept( graph: types.ModelTerminologyGraph, conceptName: String, isAbstract: Boolean = false )( implicit store: OWLAPIOMFStore ) =
    store.addEntityConcept( graph, conceptName, isAbstract )

  override def fromEntityConcept( c: types.ModelEntityConcept ) =
    c.iri

  // entity relationship

  override def addEntityRelationship(
    graph: types.ModelTerminologyGraph,
    source: types.ModelEntityDefinition,
    target: types.ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics],
    reifiedRelationshipName: String,
    unreifiedRelationshipName: String,
    unreifiedInverseRelationshipName: Option[String],
    isAbstract: Boolean = false )( implicit store: OWLAPIOMFStore ) =
    store.addEntityRelationship( graph, source, target, characteristics, reifiedRelationshipName, unreifiedRelationshipName, unreifiedInverseRelationshipName, isAbstract )

  override def fromEntityRelationship( r: types.ModelEntityRelationship ) = {
    import r._
    ( iri, source, target, characteristics )
  }

  override def fromDataTypeDefinition( dt: types.ModelDataTypeDefinition ): IRI = dt match {
    case d: types.ModelScalarDataType     => fromScalarDataType( d )
    case d: types.ModelStructuredDataType => fromStructuredDataType( d )
  }

  // scalar datatype

  override def addScalarDataType(
    graph: types.ModelTerminologyGraph,
    fragment: String )( implicit store: OWLAPIOMFStore ) =
    store.addScalarDataType( graph, fragment )

  override def fromScalarDataType( dt: types.ModelScalarDataType ) = dt.iri

  // structured datatype

  override def addStructuredDataType(
    graph: types.ModelTerminologyGraph,
    fragment: String )( implicit store: OWLAPIOMFStore ) =
    store.addStructuredDataType( graph, fragment )

  override def fromStructuredDataType( dt: types.ModelStructuredDataType ): IRI = dt.iri

  // structured data relationship

  override def addStructuredDataRelationship(
    graph: types.ModelTerminologyGraph,
    source: types.ModelStructuredDataType,
    target: types.ModelDataTypeDefinition,
    fragment: String )( implicit store: OWLAPIOMFStore ) =
    store.addStructuredDataRelationship( graph, source, target, fragment )

  override def fromStructuredDataRelationship( sd: types.ModelStructuredDataRelationship ) = {
    import sd._
    ( iri, source, target )
  }

  // entity data relationship

  override def addEntityDataRelationship(
    graph: types.ModelTerminologyGraph,
    source: types.ModelEntityDefinition,
    target: types.ModelDataTypeDefinition,
    fragment: String )( implicit store: OWLAPIOMFStore ) =
    store.addEntityDataRelationship( graph, source, target, fragment )

  override def fromEntityDataRelationship( ed: types.ModelEntityDataRelationship ) = {
    import ed._
    ( iri, source, target )
  }

  // model term axioms
  
  // entity definition aspect subclass axiom
  
  override def addEntityDefinitionAspectSubClassAxiom(
      graph: types.ModelTerminologyGraph,
      sub: types.ModelEntityDefinition,
      sup: types.ModelEntityAspect)(implicit store: OWLAPIOMFStore): Try[types.EntityDefinitionAspectSubClassAxiom] = 
    store.addEntityDefinitionAspectSubClassAxiom( graph, sub, sup )
  
  override def fromEntityDefinitionAspectSubClassAxiom(ax: types.EntityDefinitionAspectSubClassAxiom): (types.ModelEntityDefinition, types.ModelEntityAspect) = {
    import ax._
    ( sub, sup )
  }
      
  // entity concept subclass axiom

  override def addEntityConceptSubClassAxiom(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelEntityConcept,
    sup: types.ModelEntityConcept )( implicit store: OWLAPIOMFStore ) =
    store.addEntityConceptSubClassAxiom( graph, sub, sup )

  override def fromEntityConceptSubClassAxiom( ax: types.EntityConceptSubClassAxiom ) = {
    import ax._
    ( sub, sup )
  }

  // entity concept restriction axiom

  override def addEntityConceptRestrictionAxiom(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelEntityConcept,
    rel: types.ModelEntityRelationship,
    range: types.ModelEntityDefinition )( implicit store: OWLAPIOMFStore ) =
    store.addEntityConceptRestrictionAxiom( graph, sub, rel, range )

  override def fromEntityConceptRestrictionAxiom( ax: types.EntityConceptRestrictionAxiom ) = {
    import ax._
    ( sub, rel, range )
  }

  // entity relationship subclass axiom

  override def addEntityRelationshipSubClassAxiom(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelEntityRelationship,
    sup: types.ModelEntityRelationship )( implicit store: OWLAPIOMFStore ) =
    store.addEntityRelationshipSubClassAxiom( graph, sub, sup )

  override def fromEntityRelationshipSubClassAxiom( ax: types.EntityRelationshipSubClassAxiom ) = {
    import ax._
    ( sub, sup )
  }

  // scalar datatype facet restriction axiom

  override def addScalarDataTypeFacetRestriction(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelScalarDataType,
    sup: types.ModelScalarDataType,
    restrictions: Iterable[ConstrainingFacet] )( implicit store: OWLAPIOMFStore ) =
    store.addScalarDataTypeFacetRestriction( graph, sub, sup, restrictions )

  override def fromScalarDataTypeFacetRestriction( ax: types.ScalarDataTypeFacetRestriction ) = {
    import ax._
    ( sub, sup, restrictions )
  }

  override def foldTermAxiom[T]( t: types.ModelTermAxiom )(
    funEntityConceptSubClassAxiom: types.EntityConceptSubClassAxiom => T,
    funEntityConceptRestrictionAxiom: types.EntityConceptRestrictionAxiom => T,
    funEntityRelationshipSubClassAxiom: types.EntityRelationshipSubClassAxiom => T,
    funScalarDataTypeFacetRestriction: types.ScalarDataTypeFacetRestriction => T ): T = t match {
    case ax: types.EntityConceptSubClassAxiom      => funEntityConceptSubClassAxiom( ax )
    case ax: types.EntityConceptRestrictionAxiom   => funEntityConceptRestrictionAxiom( ax )
    case ax: types.EntityRelationshipSubClassAxiom => funEntityRelationshipSubClassAxiom( ax )
    case ax: types.ScalarDataTypeFacetRestriction  => funScalarDataTypeFacetRestriction( ax )
  }

  // instance graph
  
  override def loadInstanceGraph( iri: IRI)(implicit store: OWLAPIOMFStore): Try[instances.ModelInstanceGraph] =
    store.loadInstanceGraph(iri)

  override def makeInstanceGraph(
    iri: IRI,
    instantiatedTGraphs: Iterable[types.ModelTerminologyGraph],
    extendedIGraphs: Iterable[instances.ModelInstanceGraph] )( implicit store: OWLAPIOMFStore ) =
    store.makeInstanceGraph( iri, instantiatedTGraphs, extendedIGraphs )

  override def getInstanceGraphIRI( graph: instances.ModelInstanceGraph ) = graph.iri

  override def fromInstanceGraph( graph: instances.ModelInstanceGraph ) = {
    import graph._
    ( iri, t, i, c, r, dl, ic, sdp, edp )
  }

  // instance object

  override def addInstanceObject(
    graph: instances.ModelInstanceGraph,
    conceptType: types.ModelEntityConcept,
    fragment: String )( implicit store: OWLAPIOMFStore ) =
    store.addInstanceObject( graph, conceptType, fragment )

  override def fromInstanceObject( o: instances.ModelInstanceObject ) = {
    import o._
    ( iri, conceptType )
  }

  // instance relation

  override def addInstanceRelation(
    graph: instances.ModelInstanceGraph,
    relationshipType: types.ModelEntityRelationship,
    source: instances.ModelEntityInstance,
    target: instances.ModelEntityInstance,
    fragment: String )( implicit store: OWLAPIOMFStore ) =
    store.addInstanceRelation( graph, relationshipType, source, target, fragment )

  override def fromInstanceRelation( r: instances.ModelInstanceRelation ) = {
    import r._
    ( iri, relationshipType, source, target )
  }

  // data literal

  override def addDataLiteral(
    graph: instances.ModelInstanceGraph,
    datatype: types.ModelScalarDataType,
    lexicalForm: String )( implicit store: OWLAPIOMFStore ) =
    store.addDataLiteral( graph, datatype, lexicalForm )

  override def fromDataLiteral( dl: instances.ModelInstanceDataLiteral ) = {
    import dl._
    ( lexicalForm, datatype )
  }

  // data structure

  override def addDataStructure(
    graph: instances.ModelInstanceGraph,
    datatype: types.ModelStructuredDataType,
    fragment: String )( implicit store: OWLAPIOMFStore ) =
    store.addDataStructure( graph, datatype, fragment )

  override def fromDataStructure( ds: instances.ModelInstanceDataStructure ) = {
    import ds._
    ( iri, datatype )
  }

  // structured data property

  override def addStructuredDataProperty(
    graph: instances.ModelInstanceGraph,
    ds: instances.ModelInstanceDataStructure,
    structuredDataRelationshipType: types.ModelStructuredDataRelationship,
    value: instances.ModelDataInstance )( implicit store: OWLAPIOMFStore ) =
    store.addStructuredDataProperty( graph, ds, structuredDataRelationshipType, value )

  override def fromStructuredDataProperty( sdp: instances.ModelStructuredDataProperty ) = {
    import sdp._
    ( ds, structuredDataRelationshipType, value )
  }

  // entity data property

  override def addEntityDataProperty(
    graph: instances.ModelInstanceGraph,
    e: instances.ModelEntityInstance,
    entityDataRelationshipType: types.ModelEntityDataRelationship,
    value: instances.ModelDataInstance )( implicit store: OWLAPIOMFStore ) =
    store.addEntityDataProperty( graph, e, entityDataRelationshipType, value )

  override def fromEntityDataProperty( edp: instances.ModelEntityDataProperty ) = {
    import edp._
    ( e, entityDataRelationshipType, value )
  }
}
