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
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.binding._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntologyManager
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.io.OutputStream
import org.semanticweb.owlapi.model.OWLOntology

trait OWLAPIIRIOps
  extends IRIOps[OWLAPIOMF] {

  // IRI

  override def makeIRI( s: String ) = org.semanticweb.owlapi.model.IRI.create( s )

  def withFragment( iri: IRI, fragment: Option[String] ): Try[Option[IRI]] =
    fragment match {
      case None => Success( None )
      case Some( _fragment ) => withFragment( iri, _fragment ) match {
        case Success( _iri ) => Success( Some( _iri ) )
        case Failure( t )    => Failure( t )
      }
    }

  override def withFragment( iri: IRI, fragment: String ) = {
    val u = iri.toURI
    u.getFragment match {
      case f: String if ( f.nonEmpty ) => Failure( IRIFragmentException( iri ) )
      case _                           => Success( org.semanticweb.owlapi.model.IRI.create( u.resolve( "#"+fragment ) ) )
    }
  }

  override def splitIRI( iri: IRI ) = {
    val u = iri.toURI
    u.getFragment match {
      case f: String if ( f.nonEmpty ) => ( org.semanticweb.owlapi.model.IRI.create( new URI( u.getScheme, u.getSchemeSpecificPart, null ) ), Some( f ) )
      case _                           => ( iri, None )
    }
  }

  override def toAbbreviatedName( iri: IRI, lowercaseFragmentInitial: Boolean ) =
    splitIRI( iri ) match {
    case ( _, None ) => None
    case ( i, Some( fragment ) ) => 
      val path = i.toURI.getSchemeSpecificPart
      val slash = path.lastIndexOf('/')
      val last = path.substring(slash+1)
      val fragmentInitial = if (lowercaseFragmentInitial) fragment.head.toLower else fragment.head
      val fragmentTail = fragment.tail
      Some( last+":"+fragmentInitial+fragmentTail )
  }
  
  override def fromIRI( iri: IRI ) = iri.toString

  override def isBackboneIRI( iri: IRI ) = {
    val u = iri.toURI
    import u._
    getHost == "imce.jpl.nasa.gov" && getPath.startsWith( "/backbone" )
  }

  override def toBackboneIRI( iri: IRI ) = {
    val u = iri.toURI
    import u._
    org.semanticweb.owlapi.model.IRI.create( new URI( getScheme, getUserInfo, "imce.jpl.nasa.gov", getPort, "/backbone/"+getHost + getPath, getQuery, getFragment ) )
  }

  override def toSourceIRI( iri: IRI ) =
    splitIRI( iri ) match {
      case ( iri, Some( f ) ) =>
        val fragment = s"has${f}Source"
        org.semanticweb.owlapi.model.IRI.create( iri.toURI.resolve( "#"+fragment ) )
      case ( iri, None ) =>
        throw IRISourcePropertyException( iri )
    }

  override def toTargetIRI( iri: IRI ) =
    splitIRI( iri ) match {
      case ( iri, Some( f ) ) =>
        val fragment = s"has${f}Target"
        org.semanticweb.owlapi.model.IRI.create( iri.toURI.resolve( "#"+fragment ) )
      case ( iri, None ) =>
        throw IRIargetPropertyException( iri )
    }

}

trait OWLAPIImmutableTerminologyGraphOps
  extends ImmutableTerminologyGraphOps[OWLAPIOMF] { self: OWLAPIOMFOps =>

  override def loadTerminologyGraph( iri: IRI )( implicit store: OWLAPIOMFGraphStore ) =
    store.loadTerminologyGraph( iri )( this )

  override def getTerminologyGraphIRI( graph: types.ModelTerminologyGraph ) = graph.iri

  override def getTerminologyGraphKind( graph: types.ModelTerminologyGraph ) = graph.kind

  override def fromTerminologyGraph( graph: types.ModelTerminologyGraph ) = graph.fromTerminologyGraph

  override def lookupTypeTerm( graph: types.ModelTerminologyGraph, iri: IRI ) =
    graph.lookupTypeTerm( iri )

  override def lookupEntityDefinition( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelEntityDefinition ) => Some( t )
      case _                                      => None
    }

  override def lookupEntityAspect( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelEntityAspect ) => Some( t )
      case _                                  => None
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

  override def lookupEntityDataRelationshipFromEntityToScalar( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelDataRelationshipFromEntityToScalar ) => Some( t )
      case _ => None
    }

  override def lookupEntityDataRelationshipFromEntityToStructure( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelDataRelationshipFromEntityToStructure ) => Some( t )
      case _ => None
    }

  override def lookupEntityDataRelationshipFromStructureToScalar( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelDataRelationshipFromStructureToScalar ) => Some( t )
      case _ => None
    }

  override def lookupEntityDataRelationshipFromStructureToStructure( graph: types.ModelTerminologyGraph, iri: IRI ) =
    lookupTypeTerm( graph, iri ) match {
      case Some( t: types.ModelDataRelationshipFromStructureToStructure ) => Some( t )
      case _ => None
    }

  override def getTerms( graph: types.ModelTerminologyGraph ) = graph.getTerms

  def foldTerm[T]( t: types.ModelTypeTerm )(
    funEntityConcept: types.ModelEntityConcept => T,
    funEntityRelationship: types.ModelEntityRelationship => T,
    funScalarDataType: types.ModelScalarDataType => T,
    funStructuredDataType: types.ModelStructuredDataType => T,
    funDataRelationshipFromEntityToScalar: types.ModelDataRelationshipFromEntityToScalar => T,
    funDataRelationshipFromEntityToStructure: types.ModelDataRelationshipFromEntityToStructure => T,
    funDataRelationshipFromStructureToScalar: types.ModelDataRelationshipFromStructureToScalar => T,
    funDataRelationshipFromStructureToStructure: types.ModelDataRelationshipFromStructureToStructure => T ): T = t match {
    case et: types.ModelEntityConcept                             => funEntityConcept( et )
    case et: types.ModelEntityRelationship                        => funEntityRelationship( et )
    case ed: types.ModelScalarDataType                            => funScalarDataType( ed )
    case ed: types.ModelStructuredDataType                        => funStructuredDataType( ed )
    case esc: types.ModelDataRelationshipFromEntityToScalar       => funDataRelationshipFromEntityToScalar( esc )
    case est: types.ModelDataRelationshipFromEntityToStructure    => funDataRelationshipFromEntityToStructure( est )
    case ssc: types.ModelDataRelationshipFromStructureToScalar    => funDataRelationshipFromStructureToScalar( ssc )
    case sst: types.ModelDataRelationshipFromStructureToStructure => funDataRelationshipFromStructureToStructure( sst )
  }

  override def fromEntityDefinition( e: types.ModelEntityDefinition ) = e match {
    case ec: types.ModelEntityConcept      => fromEntityConcept( ec )._1
    case er: types.ModelEntityRelationship => fromEntityRelationship( er )._1
  }

  // entity facet

  override def fromEntityAspect( f: types.ModelEntityAspect ): IRI = f.iri

  // entity concept

  override def fromEntityConcept( c: types.ModelEntityConcept ) = ( c.iri, c.eg, c.isAbstract )

  // entity relationship

  override def fromEntityRelationship( r: types.ModelEntityRelationship ) = {
    import r._
    ( iri, eg, source, target, characteristics, isAbstract )
  }

  // datatype definition

  override def fromDataTypeDefinition( dt: types.ModelDataTypeDefinition ): IRI = dt match {
    case d: types.ModelScalarDataType     => fromScalarDataType( d )
    case d: types.ModelStructuredDataType => fromStructuredDataType( d )
  }

  // scalar datatype

  override def fromScalarDataType( dt: types.ModelScalarDataType ) = dt.iri

  // structured datatype

  override def fromStructuredDataType( dt: types.ModelStructuredDataType ): IRI = dt.iri

  // data relationship from entity to scalar

  override def fromDataRelationshipFromEntityToScalar( esc: types.ModelDataRelationshipFromEntityToScalar ) = {
    import esc._
    ( iri, source, target )
  }

  // data relationship from entity to structure

  override def fromDataRelationshipFromEntityToStructure( est: types.ModelDataRelationshipFromEntityToStructure ) = {
    import est._
    ( iri, source, target )
  }

  // data relationship from structure to scalar

  override def fromDataRelationshipFromStructureToScalar( ssc: types.ModelDataRelationshipFromStructureToScalar ) = {
    import ssc._
    ( iri, source, target )
  }

  // data relationship from structure to structure

  override def fromDataRelationshipFromStructureToStructure( sst: types.ModelDataRelationshipFromStructureToStructure ) = {
    import sst._
    ( iri, source, target )
  }

  // model term axioms

  override def foldTermAxiom[T]( t: types.ModelTermAxiom )(
    funEntityDefinitionAspectSubClassAxiom: types.EntityDefinitionAspectSubClassAxiom => T,
    funEntityConceptSubClassAxiom: types.EntityConceptSubClassAxiom => T,
    funEntityConceptRestrictionAxiom: types.EntityConceptRestrictionAxiom => T,
    funEntityRelationshipSubClassAxiom: types.EntityRelationshipSubClassAxiom => T,
    funScalarDataTypeFacetRestriction: types.ScalarDataTypeFacetRestriction => T ): T = t match {
    case ax: types.EntityDefinitionAspectSubClassAxiom => funEntityDefinitionAspectSubClassAxiom( ax )
    case ax: types.EntityConceptSubClassAxiom          => funEntityConceptSubClassAxiom( ax )
    case ax: types.EntityConceptRestrictionAxiom       => funEntityConceptRestrictionAxiom( ax )
    case ax: types.EntityRelationshipSubClassAxiom     => funEntityRelationshipSubClassAxiom( ax )
    case ax: types.ScalarDataTypeFacetRestriction      => funScalarDataTypeFacetRestriction( ax )
  }

  // entity definition aspect subclass axiom

  override def fromEntityDefinitionAspectSubClassAxiom( ax: types.EntityDefinitionAspectSubClassAxiom ): ( types.ModelEntityDefinition, types.ModelEntityAspect ) = {
    import ax._
    ( sub, sup )
  }

  // entity concept subclass axiom

  override def fromEntityConceptSubClassAxiom( ax: types.EntityConceptSubClassAxiom ) = {
    import ax._
    ( sub, sup )
  }

  // entity concept restriction axiom

  override def fromEntityConceptRestrictionAxiom( ax: types.EntityConceptRestrictionAxiom ) = {
    import ax._
    ( sub, rel, range )
  }

  // entity relationship subclass axiom

  override def fromEntityRelationshipSubClassAxiom( ax: types.EntityRelationshipSubClassAxiom ) = {
    import ax._
    ( sub, sup )
  }

  // scalar datatype facet restriction axiom

  override def fromScalarDataTypeFacetRestriction( ax: types.ScalarDataTypeFacetRestriction ) = {
    import ax._
    ( sub, sup, restrictions )
  }
}

trait OWLAPIMutableTerminologyGraphOps
  extends MutableTerminologyGraphOps[OWLAPIOMF]
  with OWLAPIImmutableTerminologyGraphOps { self: OWLAPIOMFOps =>

  override def asImmutableTerminologyGraph(
    g: types.MutableModelTerminologyGraph )( implicit store: OWLAPIOMFGraphStore ) =
    store.asImmutableTerminologyGraph( g )

  override def makeTerminologyGraph(
    iri: IRI, kind: TerminologyKind )( implicit store: OWLAPIOMFGraphStore ) =
    store.makeTerminologyGraph( iri, kind, None )( this )

  override def addTerminologyGraphExtension(
    extendingG: types.MutableModelTerminologyGraph,
    extendedG: types.ModelTerminologyGraph )( implicit store: OWLAPIOMFGraphStore ): Try[Unit] =
    extendingG.addTerminologyGraphExtension( extendedG )

  override def saveTerminologyGraph(
    g: types.MutableModelTerminologyGraph )( implicit store: OWLAPIOMFGraphStore ) =
    store.saveTerminologyGraph( g )( this )

  override def saveTerminologyGraph(
    g: types.MutableModelTerminologyGraph,
    os: OutputStream )( implicit store: OWLAPIOMFGraphStore ) =
    store.saveTerminologyGraph( g, os )( this )

  // entity facet

  override def addEntityAspect(
    graph: types.MutableModelTerminologyGraph,
    aspectName: String )( implicit store: OWLAPIOMFGraphStore ): Try[types.ModelEntityAspect] =
    for {
      aspectIRI <- withFragment( graph.iri, aspectName )
      aspect <- graph.addEntityAspect( aspectIRI )
    } yield aspect

  // entity concept

  /**
   * Wrapper
   */
  def addEntityConcept(
    o: OWLOntology,
    graph: types.MutableModelTerminologyGraph,
    conceptName: String,
    conceptGraphIRI: Option[IRI],
    isAbstract: Boolean,
    hasName: String,
    hasQualifiedName: String,
    hasUUID: String )( implicit store: OWLAPIOMFGraphStore ): Try[( types.ModelEntityConcept, Option[types.MutableModelTerminologyGraph] )] = 
      for {
        result <- addEntityConcept( graph, conceptName, conceptGraphIRI, isAbstract)
      } yield {
        store.createOMFModelEntityConceptInstance( o, result._1.iri, hasName, hasQualifiedName, hasUUID, isAbstract )
        result
      }
      
  override def addEntityConcept(
    graph: types.MutableModelTerminologyGraph,
    conceptName: String,
    conceptGraphIRI: Option[IRI],
    isAbstract: Boolean = false )( implicit store: OWLAPIOMFGraphStore ) =
    for {
      conceptIRI <- withFragment( graph.iri, conceptName )
      c <- graph.addEntityConcept( conceptIRI, conceptGraphIRI, isAbstract )
    } yield c

  // entity relationship

  override def addEntityRelationship(
    graph: types.MutableModelTerminologyGraph,
    source: types.ModelEntityDefinition,
    target: types.ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics],
    reifiedRelationshipName: String,
    relationshipGraphIRI: Option[IRI],
    unreifiedRelationshipName: String,
    unreifiedInverseRelationshipName: Option[String],
    isAbstract: Boolean = false )( implicit store: OWLAPIOMFGraphStore ) =
    for {
      rIRI <- withFragment( graph.iri, reifiedRelationshipName )
      rIRISource = toSourceIRI( rIRI )
      rIRITarget = toTargetIRI( rIRI )
      uIRI <- withFragment( graph.iri, unreifiedRelationshipName )
      uiIRI <- withFragment( graph.iri, unreifiedInverseRelationshipName )
      r <- graph.addEntityRelationship(
        rIRI, relationshipGraphIRI,
        rIRISource, rIRITarget,
        uIRI, uiIRI,
        source, target,
        characteristics, isAbstract )
    } yield r

  // scalar datatype

  override def addScalarDataType(
    graph: types.MutableModelTerminologyGraph,
    scalarName: String )( implicit store: OWLAPIOMFGraphStore ) =
    for {
      scalarIRI <- withFragment( graph.iri, scalarName )
      scalar <- graph.addScalarDataType( scalarIRI )
    } yield scalar

  // structured datatype

  override def addStructuredDataType(
    graph: types.MutableModelTerminologyGraph,
    fragment: String )( implicit store: OWLAPIOMFGraphStore ) = ???

  // data relationship from entity to scalar

  override def addDataRelationshipFromEntityToScalar(
    graph: types.MutableModelTerminologyGraph,
    source: types.ModelEntityDefinition,
    target: types.ModelScalarDataType,
    dataRelationshipName: String )( implicit store: OWLAPIOMFGraphStore ) =
    for {
      dIRI <- withFragment( graph.iri, dataRelationshipName )
      d <- graph.addDataRelationshipFromEntityToScalar( dIRI, source, target )
    } yield d

  // data relationship from entity to structure

  override def addDataRelationshipFromEntityToStructure(
    graph: types.MutableModelTerminologyGraph,
    source: types.ModelEntityDefinition,
    target: types.ModelStructuredDataType,
    dataRelationshipName: String )( implicit store: OWLAPIOMFGraphStore ) =
    for {
      dIRI <- withFragment( graph.iri, dataRelationshipName )
      d <- graph.addDataRelationshipFromEntityToStructure( dIRI, source, target )
    } yield d

  // data relationship from structure to scalar

  override def addDataRelationshipFromStructureToScalar(
    graph: types.MutableModelTerminologyGraph,
    source: types.ModelStructuredDataType,
    target: types.ModelScalarDataType,
    dataRelationshipName: String )( implicit store: OWLAPIOMFGraphStore ) =
    for {
      dIRI <- withFragment( graph.iri, dataRelationshipName )
      d <- graph.addDataRelationshipFromStructureToScalar( dIRI, source, target )
    } yield d

  // data relationship from structure to structure

  override def addDataRelationshipFromStructureToStructure(
    graph: types.MutableModelTerminologyGraph,
    source: types.ModelStructuredDataType,
    target: types.ModelStructuredDataType,
    dataRelationshipName: String )( implicit store: OWLAPIOMFGraphStore ) =
    for {
      dIRI <- withFragment( graph.iri, dataRelationshipName )
      d <- graph.addDataRelationshipFromStructureToStructure( dIRI, source, target )
    } yield d

  // model term axioms

  // entity definition aspect subclass axiom

  override def addEntityDefinitionAspectSubClassAxiom(
    graph: types.MutableModelTerminologyGraph,
    sub: types.ModelEntityDefinition,
    sup: types.ModelEntityAspect )( implicit store: OWLAPIOMFGraphStore ) =
    graph.addEntityDefinitionAspectSubClassAxiom( sub, sup )

  // entity concept subclass axiom

  override def addEntityConceptSubClassAxiom(
    graph: types.MutableModelTerminologyGraph,
    sub: types.ModelEntityConcept,
    sup: types.ModelEntityConcept )( implicit store: OWLAPIOMFGraphStore ) =
    graph.addEntityConceptSubClassAxiom( sub, sup )

  // entity concept restriction axiom

  override def addEntityConceptRestrictionAxiom(
    graph: types.MutableModelTerminologyGraph,
    sub: types.ModelEntityConcept,
    rel: types.ModelEntityRelationship,
    range: types.ModelEntityDefinition )( implicit store: OWLAPIOMFGraphStore ) = ???

  // entity relationship subclass axiom

  override def addEntityRelationshipSubClassAxiom(
    graph: types.MutableModelTerminologyGraph,
    sub: types.ModelEntityRelationship,
    sup: types.ModelEntityRelationship )( implicit store: OWLAPIOMFGraphStore ) =
      graph.addEntityRelationshipSubClassAxiom( sub, sup )

  // scalar datatype facet restriction axiom

  override def addScalarDataTypeFacetRestriction(
    graph: types.MutableModelTerminologyGraph,
    sub: types.ModelScalarDataType,
    sup: types.ModelScalarDataType,
    restrictions: Iterable[ConstrainingFacet] )( implicit store: OWLAPIOMFGraphStore ) = ???

}

trait OWLAPIImmutableInstanceGraphOps
  extends ImmutableInstanceGraphOps[OWLAPIOMF] {

  override def loadInstanceGraph( iri: IRI )( implicit store: OWLAPIOMFGraphStore ) = store.loadInstanceGraph( iri )

  override def getInstanceGraphIRI( graph: instances.ModelInstanceGraph ) = graph.iri

  override def fromInstanceGraph( graph: instances.ModelInstanceGraph ) = graph.fromInstanceGraph

  // instance object

  override def fromInstanceObject( o: instances.ModelInstanceObject ) = {
    import o._
    ( iri, conceptType )
  }

  // instance relation

  override def fromInstanceRelation( r: instances.ModelInstanceRelation ) = {
    import r._
    ( iri, relationshipType, source, target )
  }

  // data literal

  override def fromDataLiteral( dl: instances.ModelInstanceDataLiteral ) = {
    import dl._
    ( lexicalForm, datatype )
  }

  // data structure

  override def fromDataStructure( ds: instances.ModelInstanceDataStructure ) = {
    import ds._
    ( iri, datatype )
  }

  // data property from entity to scalar

  override def fromInstanceDataRelationshipFromEntityToScalar( e2sc: instances.ModelInstanceDataRelationshipFromEntityToScalar ) = {
    import e2sc._
    ( ei, dataRelationship, value )
  }

  // data property from entity to structure

  override def fromInstanceDataRelationshipFromEntityToStructure( e2st: instances.ModelInstanceDataRelationshipFromEntityToStructure ) = {
    import e2st._
    ( ei, dataRelationship, value )
  }

  // data property from structure to scalar

  override def fromInstanceDataRelationshipFromStructureToScalar( s2sc: instances.ModelInstanceDataRelationshipFromStructureToScalar ) = {
    import s2sc._
    ( di, dataRelationship, value )
  }

  // data property from structure to structure

  override def fromInstanceDataRelationshipFromStructureToStructure( s2st: instances.ModelInstanceDataRelationshipFromStructureToStructure ) = {
    import s2st._
    ( di, dataRelationship, value )
  }
}

trait OWLAPIMutableInstanceGraphOps
  extends MutableInstanceGraphOps[OWLAPIOMF]
  with OWLAPIImmutableInstanceGraphOps {

  override def asImmutableInstanceGraph( g: instances.MutableModelInstanceGraph )( implicit store: OWLAPIOMFGraphStore ) =
    store.asImmutableInstanceGraph( g )

  override def makeInstanceGraph(
    iri: IRI,
    instantiatedTGraphs: Iterable[types.ImmutableModelTerminologyGraph],
    extendedIGraphs: Iterable[instances.ImmutableModelInstanceGraph] )( implicit store: OWLAPIOMFGraphStore ) =
    store.makeInstanceGraph( iri, instantiatedTGraphs, extendedIGraphs )

  override def saveInstanceGraph( g: instances.MutableModelInstanceGraph )( implicit store: OWLAPIOMFGraphStore ) =
    g.save

  override def saveInstanceGraph( g: instances.MutableModelInstanceGraph, os: OutputStream )( implicit store: OWLAPIOMFGraphStore ) =
    g.save( os )

  // instance object

  override def addInstanceObject(
    graph: instances.MutableModelInstanceGraph,
    conceptType: types.ModelEntityConcept,
    fragment: String )( implicit store: OWLAPIOMFGraphStore ) = ???

  // instance relation

  override def addInstanceRelation(
    graph: instances.MutableModelInstanceGraph,
    relationshipType: types.ModelEntityRelationship,
    source: instances.ModelEntityInstance,
    target: instances.ModelEntityInstance,
    fragment: String )( implicit store: OWLAPIOMFGraphStore ) = ???

  // data literal

  override def addDataLiteral(
    graph: instances.MutableModelInstanceGraph,
    datatype: types.ModelScalarDataType,
    lexicalForm: String )( implicit store: OWLAPIOMFGraphStore ) = ???

  // data structure

  override def addDataStructure(
    graph: instances.MutableModelInstanceGraph,
    datatype: types.ModelStructuredDataType,
    fragment: String )( implicit store: OWLAPIOMFGraphStore ) = ???

  // data property from entity to scalar

  override def addInstanceDataRelationshipFromEntityToScalar(
    graph: instances.MutableModelInstanceGraph,
    ei: instances.ModelEntityInstance,
    e2sc: types.ModelDataRelationshipFromEntityToScalar,
    value: instances.ModelInstanceDataLiteral )( implicit store: OWLAPIOMFGraphStore ) = ???

  // data property from entity to structure

  override def addInstanceDataRelationshipFromEntityToStructure(
    graph: instances.MutableModelInstanceGraph,
    ei: instances.ModelEntityInstance,
    e2st: types.ModelDataRelationshipFromEntityToStructure,
    value: instances.ModelInstanceDataStructure )( implicit store: OWLAPIOMFGraphStore ) = ???

  // data property from structure to scalar

  override def addInstanceDataRelationshipFromStructureToScalar(
    graph: instances.MutableModelInstanceGraph,
    di: instances.ModelInstanceDataStructure,
    e2sc: types.ModelDataRelationshipFromStructureToScalar,
    value: instances.ModelInstanceDataLiteral )( implicit store: OWLAPIOMFGraphStore ) = ???

  // data property from structure to structure

  override def addInstanceDataRelationshipFromStructureToStructure(
    graph: instances.MutableModelInstanceGraph,
    di: instances.ModelInstanceDataStructure,
    e2st: types.ModelDataRelationshipFromStructureToStructure,
    value: instances.ModelInstanceDataStructure )( implicit store: OWLAPIOMFGraphStore ) = ???
}

class OWLAPIOMFOps
  extends OWLAPIIRIOps
  with OWLAPIMutableTerminologyGraphOps
  with OWLAPIMutableInstanceGraphOps
  with OMFOps[OWLAPIOMF] {

  val rdfs_label = makeIRI( "http://www.w3.org/2000/01/rdf-schema#label" )
  val AnnotationIsAbstract = makeIRI( "http://imce.jpl.nasa.gov/foundation/annotation/annotation#isAbstract" )
  val AnnotationIsDerived = makeIRI( "http://imce.jpl.nasa.gov/foundation/annotation/annotation#isDerived" )
  val AnnotationIsDefinition = makeIRI( "http://imce.jpl.nasa.gov/foundation/annotation/annotation#isDefinition" )
  val AnnotationIsDesignation = makeIRI( "http://imce.jpl.nasa.gov/foundation/annotation/annotation#isDesignation" )

  /**
   * Used for an entity concept or relationship to indicate the IRI of the corresponding graph.
   */
  val AnnotationEntityGraphIRI = makeIRI( "http://imce.jpl.nasa.gov/foundation/annotation/annotation#entityGraphIRI" )

  /**
   * Used for a terminology graph to indicate the IRI of the corresponding entity
   */
  val AnnotationGraphForEntityIRI = makeIRI( "http://imce.jpl.nasa.gov/foundation/annotation/annotation#graphForEntityIRI" )

}

sealed abstract class IRIArgumentException( val message: String ) extends IllegalArgumentException( message )

case class IRIFragmentException( val iri: IRI ) extends IRIArgumentException( s"withFragment(iri=${iri}) -- the IRI already has a fragment" )

case class IRIObjectPropertyException( val iri: IRI ) extends IRIArgumentException( s"toObjectProperty(iri=${iri}) -- the IRI must have a fragment" )

case class IRISourcePropertyException( val iri: IRI ) extends IRIArgumentException( s"toSourceIRI(iri=${iri}) -- the IRI must have a fragment" )

case class IRIargetPropertyException( val iri: IRI ) extends IRIArgumentException( s"toTargetIRI(iri=${iri}) -- the IRI must have a fragment" )
