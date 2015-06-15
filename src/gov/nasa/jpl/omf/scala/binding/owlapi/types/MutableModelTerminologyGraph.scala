/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2015, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.binding.owlapi.types

import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.parameters.Imports
import scala.collection.JavaConversions._
import scala.language.postfixOps
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import org.semanticweb.owlapi.model.AddAxiom
import org.semanticweb.owlapi.model.OWLDatatype
import org.semanticweb.owlapi.model.OWLClass
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.OWLDataProperty
import org.semanticweb.owlapi.model.AddImport
import java.io.OutputStream

object EntityExceptionKind extends Enumeration {
  type EntityExceptionKind = Value
  val EntityAspect, EntityConcept, EntityRelationship, DataRelationshipFromEntityToScalar, ScalarDataType = Value
}

import EntityExceptionKind._

object RelationshipScopeAccessKind extends Enumeration {
  type RelationshipScopeAccessKind = Value
  val Source, Target = Value
}

import RelationshipScopeAccessKind._

object AxiomExceptionKind extends Enumeration {
  type AxiomExceptionKind = Value
  val AspectSubclassAxiom, ConceptSubclassAxiom, ConceptRestrictionAxiom, RelationshipSubclassAxiom = Value
}

import AxiomExceptionKind._

object AxiomScopeAccessKind extends Enumeration {
  type AxiomScopeAccessKind = Value
  val Sub, Sup, Rel, Range = Value
}

import AxiomScopeAccessKind._

sealed abstract class MutableModelTerminologyGraphException( val message: String ) extends IllegalArgumentException( message ) 

case class EntityConflictException( kind: EntityExceptionKind, iri: IRI, conflictingTerm: ModelTypeTerm )
  extends MutableModelTerminologyGraphException( s"Cannot create ${kind} with IRI='${iri}' because this IRI refers to: ${conflictingTerm}" )

case class EntityScopeException( kind: EntityExceptionKind, iri: IRI, unaccessibleTerms: Map[RelationshipScopeAccessKind, ModelTypeTerm] )
  extends MutableModelTerminologyGraphException(
    s"Cannot create ${kind} with IRI='${iri}' because there are ${unaccessibleTerms.size} terms out of scope of the graph: "+
      (( unaccessibleTerms.map { case ( kind, term ) => s"${kind}: ${term}" } ) mkString ( ", ") ) )

case class AxiomScopeException( kind: AxiomExceptionKind, unaccessibleTerms: Map[AxiomScopeAccessKind, ModelTypeTerm] )
  extends MutableModelTerminologyGraphException(
    s"Cannot create ${kind} because there are ${unaccessibleTerms.size} terms out of scope of the graph: "+
     ( ( unaccessibleTerms.map { case ( kind, term ) => s"${kind}: ${term}" } ) mkString ( ", " ) ) )

case class MutableModelTerminologyGraph(
  override val kind: TerminologyKind,
  override val ont: OWLOntology,
  override val entityG: Option[IRI] )( override implicit val ops: OWLAPIOMFOps )
  extends ModelTerminologyGraph( kind, ont, entityG )( ops ) {

  override val isImmutableModelTerminologyGraph = true
  override val isMutableModelTerminologyGraph = false
  override val imports = scala.collection.mutable.ListBuffer[ModelTerminologyGraph]()

  import ops._
  import EntityConflictException._

  val rdfs_labelAP = owlDataFactory.getOWLAnnotationProperty( rdfs_label )
  val isAbstractAP = owlDataFactory.getOWLAnnotationProperty( AnnotationIsAbstract )
  val isDerivedAP = owlDataFactory.getOWLAnnotationProperty( AnnotationIsDerived )
  val entityGraphIRIAP = owlDataFactory.getOWLAnnotationProperty( AnnotationEntityGraphIRI )
  val graphForEntityIRIAP = owlDataFactory.getOWLAnnotationProperty( AnnotationGraphForEntityIRI )

  override protected val aspects = scala.collection.mutable.ListBuffer[ModelEntityAspect]()
  override protected val concepts = scala.collection.mutable.ListBuffer[ModelEntityConcept]()
  override protected val relationships = scala.collection.mutable.ListBuffer[ModelEntityReifiedRelationship]()
  override protected val sc = scala.collection.mutable.ListBuffer[ModelScalarDataType]()
  override protected val st = scala.collection.mutable.ListBuffer[ModelStructuredDataType]()
  override protected val e2sc = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromEntityToScalar]()
  override protected val e2st = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromEntityToStructure]()
  override protected val s2sc = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromStructureToScalar]()
  override protected val s2st = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromStructureToStructure]()
  override protected val ax = scala.collection.mutable.ListBuffer[ModelTermAxiom]()

  override def getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition] =
    ( ( aspects map ( a => ( a.e -> a ) ) ) ++
      ( concepts map ( c => ( c.e -> c ) ) ) ++
      ( relationships map ( r => ( r.e -> r ) ) ) ) toMap

  override protected val iri2typeTerm = scala.collection.mutable.HashMap[IRI, ModelTypeTerm]()

  def save( saveIRI: IRI ): Try[Unit] = Try {
    ontManager.saveOntology( ont, saveIRI )
  }

  def save( os: OutputStream ): Try[Unit] = Try {
    ontManager.saveOntology( ont, os )
  }

  val backbone = Backbone.createBackbone( ont, kind, ops ).get

  def addTerminologyGraphExtension( extendedG: ModelTerminologyGraph ): Try[Unit] = {
    if ( imports.contains( extendedG ) ) Failure( new IllegalArgumentException( "..." ) )
    else {
      imports.add( extendedG )
      val decl = ontManager.getOWLDataFactory.getOWLImportsDeclaration( extendedG.iri )
      val changeApplied = ontManager.applyChange( new AddImport( ont, decl ) )
      Success( Unit )
    }
  }

  protected def createModelEntityAspect( a: OWLClass ): ModelEntityAspect = {
    val _a = ModelEntityAspect( a )
    aspects += _a
    iri2typeTerm += a.getIRI -> _a
    _a
  }

  def addEntityAspect( aspectIRI: IRI ): Try[types.ModelEntityAspect] = iri2typeTerm get aspectIRI match {
    case None =>
      val aspectC = owlDataFactory.getOWLClass( aspectIRI )
      val aspectTerm = createModelEntityAspect( aspectC )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( aspectC ) ) )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubClassOfAxiom( aspectC, backbone.ThingC ) ) )
      Success( aspectTerm )

    case Some( term ) =>
      Failure( EntityConflictException( EntityAspect, aspectIRI, term ) )
  }

  protected def createModelEntityConcept(
    c: OWLClass,
    isAbstract: Boolean,
    cgIRI: Option[IRI] ): ModelEntityConcept = {
    val _c = ModelEntityConcept( c, cgIRI, isAbstract )
    concepts += _c
    iri2typeTerm += c.getIRI -> _c
    _c
  }

  def addEntityConcept(
    conceptIRI: IRI,
    conceptGraphIRI: Option[IRI],
    isAbstract: Boolean )( implicit store: OWLAPIOMFGraphStore ): Try[( types.ModelEntityConcept, Option[MutableModelTerminologyGraph] )] =

    iri2typeTerm get conceptIRI match {
      case None =>

        val conceptC = owlDataFactory.getOWLClass( conceptIRI )
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( conceptC ) ) )
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLAnnotationAssertionAxiom( isAbstractAP, conceptIRI, owlDataFactory.getOWLLiteral( isAbstract ) ) ) )
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubClassOfAxiom( conceptC, backbone.EntityC ) ) )

        conceptGraphIRI match {
          case None =>
            Success(
              Tuple2(
                createModelEntityConcept( conceptC, isAbstract, None ),
                None ) )

          case Some( cgIRI ) =>
            makeTerminologyGraph( cgIRI, kind ) match {
              case Failure( t ) =>
                Failure( t )

              case Success( cg ) =>
                ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLAnnotationAssertionAxiom( entityGraphIRIAP, conceptIRI, cgIRI ) ) )
                ontManager.applyChange( new AddAxiom( cg.ont, owlDataFactory.getOWLAnnotationAssertionAxiom( graphForEntityIRIAP, cgIRI, conceptIRI ) ) )
                Success(
                  Tuple2(
                    createModelEntityConcept( conceptC, isAbstract, Some( cgIRI ) ),
                    Some( cg ) ) )
            }
        }

      case Some( term ) =>
        Failure( EntityConflictException( EntityConcept, conceptIRI, term ) )
    }

  protected def createEntityRelationship(
    r: OWLClass, rg: Option[IRI],
    u: OWLObjectProperty, ui: Option[OWLObjectProperty],
    source: ModelEntityDefinition, rSource: OWLObjectProperty,
    target: ModelEntityDefinition, rTarget: OWLObjectProperty,
    characteristics: Iterable[RelationshipCharacteristics], isAbstract: Boolean ): types.ModelEntityReifiedRelationship = {
    val _term = ModelEntityReifiedRelationship(
      r, rg,
      u, ui,
      source, rSource,
      target, rTarget,
      characteristics, isAbstract )
    relationships += _term
    iri2typeTerm += r.getIRI -> _term
    _term
  }

  protected def makeEntityRelationship(
    rIRI: IRI, rg: Option[IRI],
    rIRISource: IRI, rIRITarget: IRI,
    uIRI: IRI, uiIRI: Option[IRI],
    source: ModelEntityDefinition, target: ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics],
    isAbstract: Boolean )( implicit store: OWLAPIOMFGraphStore ): Try[( types.ModelEntityReifiedRelationship, Option[MutableModelTerminologyGraph] )] = {

    val sourceC = owlDataFactory.getOWLClass( source.iri )
    val targetC = owlDataFactory.getOWLClass( target.iri )
    val r = owlDataFactory.getOWLClass( rIRI )
    val rSource = owlDataFactory.getOWLObjectProperty( rIRISource )
    val rTarget = owlDataFactory.getOWLObjectProperty( rIRITarget )
    val u = owlDataFactory.getOWLObjectProperty( uIRI )
    val ui = if ( uiIRI.isEmpty ) None else Some( owlDataFactory.getOWLObjectProperty( uiIRI.get ) )
    val term = createEntityRelationship( r, rg, u, ui, source, rSource, target, rTarget, characteristics, isAbstract )

    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( r ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLAnnotationAssertionAxiom( isAbstractAP, rIRI, owlDataFactory.getOWLLiteral( isAbstract ) ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubClassOfAxiom( r, backbone.ReifiedObjectPropertyC ) ) )

    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( rSource ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom( rSource, backbone.topReifiedObjectPropertySourceOP ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLObjectPropertyDomainAxiom( rSource, r ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLObjectPropertyRangeAxiom( rSource, sourceC ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom( rSource ) ) )

    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( rTarget ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom( rTarget, backbone.topReifiedObjectPropertyTargetOP ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLObjectPropertyDomainAxiom( rTarget, r ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLObjectPropertyRangeAxiom( rTarget, targetC ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLFunctionalObjectPropertyAxiom( rTarget ) ) )

    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( u ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom( u, backbone.topReifiedObjectPropertyOP ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLObjectPropertyDomainAxiom( u, sourceC ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLObjectPropertyRangeAxiom( u, targetC ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubPropertyChainOfAxiom( List( owlDataFactory.getOWLObjectInverseOf( rSource ), rTarget ), u ) ) )

    if ( ui.isDefined ) {
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( ui.get ) ) )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLAnnotationAssertionAxiom( isDerivedAP, ui.get.getIRI, owlDataFactory.getOWLLiteral( true ) ) ) )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom( ui.get, backbone.topReifiedObjectPropertyOP ) ) )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLObjectPropertyDomainAxiom( ui.get, targetC ) ) )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLObjectPropertyRangeAxiom( ui.get, sourceC ) ) )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubPropertyChainOfAxiom( List( owlDataFactory.getOWLObjectInverseOf( rTarget ), rSource ), ui.get ) ) )
    }

    rg match {
      case None =>
        Success( Tuple2( term, None ) )
      case Some( rgIRI ) =>

        makeTerminologyGraph( rgIRI, kind ) match {
          case Failure( t ) =>
            Failure( t )
          case Success( relationshipGraph ) =>
            Success( Tuple2( term, Some( relationshipGraph ) ) )
        }
    }

  }

  def addEntityRelationship(
    rIRI: IRI, relationshipGraphIRI: Option[IRI],
    rIRISource: IRI, rIRITarget: IRI,
    uIRI: IRI, uiIRI: Option[IRI],
    source: ModelEntityDefinition, target: ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics],
    isAbstract: Boolean )( implicit store: OWLAPIOMFGraphStore ): Try[( types.ModelEntityReifiedRelationship, Option[MutableModelTerminologyGraph] )] =
    ( lookupTypeTerm( rIRI ),
      lookupTypeTerm( rIRISource ),
      lookupTypeTerm( rIRITarget ),
      lookupTypeTerm( uIRI ),
      lookupTypeTerm( uiIRI ) ) match {
        case ( None, None, None, None, None ) =>
          ( isTypeTermDefinedRecursively( source ), isTypeTermDefinedRecursively( target ) ) match {
            case ( true, true ) =>
              makeEntityRelationship( rIRI, relationshipGraphIRI, rIRISource, rIRITarget, uIRI, uiIRI, source, target, characteristics, isAbstract )
            case ( false, true ) =>
              Failure( EntityScopeException( EntityRelationship, rIRI, Map( Source -> source ) ) )

            case ( true, false ) =>
              Failure( EntityScopeException( EntityRelationship, rIRI, Map( Target -> target ) ) )

            case ( false, false ) =>
              Failure( EntityScopeException( EntityRelationship, rIRI, Map( Source -> source, Target -> target ) ) )
          }

        case ( Some( t ), _, _, _, _ ) =>
          Failure( EntityConflictException( EntityRelationship, rIRI, t ) )

        case ( _, Some( t ), _, _, _ ) =>
          Failure( EntityConflictException( EntityRelationship, rIRISource, t ) )

        case ( _, _, Some( t ), _, _ ) =>
          Failure( EntityConflictException( EntityRelationship, rIRITarget, t ) )

        case ( _, _, _, Some( t ), _ ) =>
          Failure( EntityConflictException( EntityRelationship, uIRI, t ) )

        case ( _, _, _, _, Some( t ) ) =>
          require( uiIRI.isDefined )
          Failure( EntityConflictException( EntityRelationship, uiIRI.get, t ) )
      }

  protected def createModelScalarDataType( dt: OWLDatatype ): ModelScalarDataType = {
    val _dt = ModelScalarDataType( dt.getIRI )
    sc += _dt
    iri2typeTerm += dt.getIRI -> _dt
    _dt
  }

  def addScalarDataType( scalarIRI: IRI ): Try[types.ModelScalarDataType] = iri2typeTerm get scalarIRI match {
    case None =>
      val scalarDT = owlDataFactory.getOWLDatatype( scalarIRI )
      val scalarTerm = createModelScalarDataType( scalarDT )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( scalarDT ) ) )
      Success( scalarTerm )

    case Some( term ) =>
      Failure( EntityConflictException( ScalarDataType, scalarIRI, term ) )
  }

  def createDataRelationshipFromEntityToScalar( esc: OWLDataProperty, source: ModelEntityDefinition, target: ModelScalarDataType ): ModelDataRelationshipFromEntityToScalar = {
    val _esc = ModelDataRelationshipFromEntityToScalar( esc, source, target )
    e2sc += _esc
    iri2typeTerm += esc.getIRI -> _esc
    _esc
  }

  protected def makeDataRelationshipFromEntityToScalar(
    dIRI: IRI,
    source: types.ModelEntityDefinition,
    target: types.ModelScalarDataType ): Try[types.ModelDataRelationshipFromEntityToScalar] = {
    val escDP = owlDataFactory.getOWLDataProperty( dIRI )
    val escTerm = createDataRelationshipFromEntityToScalar( escDP, source, target )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( escDP ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubDataPropertyOfAxiom( escDP, backbone.topDataPropertyDP ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDataPropertyDomainAxiom( escDP, source.e ) ) )
    ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDataPropertyRangeAxiom( escDP, owlDataFactory.getOWLDatatype( target.iri ) ) ) )
    Success( escTerm )
  }

  def addDataRelationshipFromEntityToScalar(
    dIRI: IRI,
    source: types.ModelEntityDefinition,
    target: types.ModelScalarDataType ): Try[types.ModelDataRelationshipFromEntityToScalar] = iri2typeTerm get dIRI match {
    case None =>
      ( isTypeTermDefinedRecursively( source ), isTypeTermDefinedRecursively( target ) ) match {
        case ( true, true ) =>
          makeDataRelationshipFromEntityToScalar( dIRI, source, target )
        case ( false, true ) =>
          Failure( EntityScopeException( DataRelationshipFromEntityToScalar, dIRI, Map( Source -> source ) ) )

        case ( true, false ) =>
          Failure( EntityScopeException( DataRelationshipFromEntityToScalar, dIRI, Map( Target -> target ) ) )

        case ( false, false ) =>
          Failure( EntityScopeException( DataRelationshipFromEntityToScalar, dIRI, Map( Source -> source, Target -> target ) ) )
      }

    case Some( term ) =>
      Failure( EntityConflictException( DataRelationshipFromEntityToScalar, dIRI, term ) )
  }

  def addDataRelationshipFromEntityToStructure(
    dIRI: IRI,
    source: types.ModelEntityDefinition,
    target: types.ModelStructuredDataType ): Try[types.ModelDataRelationshipFromEntityToStructure] = ???

  def addDataRelationshipFromStructureToScalar(
    dIRI: IRI,
    source: types.ModelStructuredDataType,
    target: types.ModelScalarDataType ): Try[types.ModelDataRelationshipFromStructureToScalar] = ???

  def addDataRelationshipFromStructureToStructure(
    dIRI: IRI,
    source: types.ModelStructuredDataType,
    target: types.ModelStructuredDataType ): Try[types.ModelDataRelationshipFromStructureToStructure] = ???

  def addEntityConceptSubClassAxiom(
    sub: types.ModelEntityConcept,
    sup: types.ModelEntityConcept ): Try[types.EntityConceptSubClassAxiom] =
    ( isTypeTermDefinedRecursively( sub ), isTypeTermDefinedRecursively( sup ) ) match {
      case ( true, true ) =>
        val subC = owlDataFactory.getOWLClass( sub.iri )
        val supC = owlDataFactory.getOWLClass( sup.iri )
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubClassOfAxiom( subC, supC ) ) )
        val ax = EntityConceptSubClassAxiom( sub, sup )
        Success( ax )

      case ( false, true ) =>
        Failure( AxiomScopeException( ConceptSubclassAxiom, Map( Sub -> sub ) ) )

      case ( true, false ) =>
        Failure( AxiomScopeException( ConceptSubclassAxiom, Map( Sup -> sup ) ) )

      case ( false, false ) =>
        Failure( AxiomScopeException( ConceptSubclassAxiom, Map( Sub -> sub, Sup -> sup ) ) )
    }

  def addEntityConceptUniversalRestrictionAxiom(
    sub: types.ModelEntityConcept,
    rel: types.ModelEntityReifiedRelationship,
    range: types.ModelEntityDefinition ): Try[types.EntityConceptUniversalRestrictionAxiom] =
    ( isTypeTermDefinedRecursively( sub ), isTypeTermDefinedRecursively( rel ), isTypeTermDefinedRecursively( range ) ) match {
      case ( true, true, true ) =>
        val subC = owlDataFactory.getOWLClass( sub.iri )
        val rangeC = owlDataFactory.getOWLClass( range.iri )
        ontManager.applyChange( 
            new AddAxiom( ont, 
                owlDataFactory.getOWLSubClassOfAxiom( 
                    subC, 
                    owlDataFactory.getOWLObjectAllValuesFrom( rel.unreified, rangeC ) ) ) )
        val ax = EntityConceptUniversalRestrictionAxiom( sub, rel, range )
        Success( ax )

      case ( false, _, _ ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Sub -> sub ) ) )

      case ( _, false, _ ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Rel -> rel ) ) )

      case ( _, _, false ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Range -> range ) ) )

    }
  
  def addEntityConceptExistentialRestrictionAxiom(
    sub: types.ModelEntityConcept,
    rel: types.ModelEntityReifiedRelationship,
    range: types.ModelEntityDefinition ): Try[types.EntityConceptExistentialRestrictionAxiom] =
    ( isTypeTermDefinedRecursively( sub ), isTypeTermDefinedRecursively( rel ), isTypeTermDefinedRecursively( range ) ) match {
      case ( true, true, true ) =>
        val subC = owlDataFactory.getOWLClass( sub.iri )
        val rangeC = owlDataFactory.getOWLClass( range.iri )
        ontManager.applyChange( 
            new AddAxiom( ont, 
                owlDataFactory.getOWLSubClassOfAxiom( 
                    subC, 
                    owlDataFactory.getOWLObjectSomeValuesFrom( rel.unreified, rangeC ) ) ) )
        val ax = EntityConceptExistentialRestrictionAxiom( sub, rel, range )
        Success( ax )

      case ( false, _, _ ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Sub -> sub ) ) )

      case ( _, false, _ ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Rel -> rel ) ) )

      case ( _, _, false ) =>
        Failure( AxiomScopeException( ConceptRestrictionAxiom, Map( Range -> range ) ) )
    }
  
  def addEntityDefinitionAspectSubClassAxiom(
    sub: types.ModelEntityDefinition,
    sup: types.ModelEntityAspect ): Try[types.EntityDefinitionAspectSubClassAxiom] =
    ( isTypeTermDefinedRecursively( sub ), isTypeTermDefinedRecursively( sup ) ) match {
      case ( true, true ) =>
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubClassOfAxiom( sub.e, sup.e ) ) )
        val ax = EntityDefinitionAspectSubClassAxiom( sub, sup )
        Success( ax )

      case ( false, true ) =>
        Failure( AxiomScopeException( AspectSubclassAxiom, Map( Sub -> sub ) ) )

      case ( true, false ) =>
        Failure( AxiomScopeException( AspectSubclassAxiom, Map( Sup -> sub ) ) )

      case ( false, false ) =>
        Failure( AxiomScopeException( AspectSubclassAxiom, Map( Sub -> sub, Sup -> sub ) ) )
    }

  def addEntityRelationshipSubClassAxiom(
    sub: types.ModelEntityReifiedRelationship,
    sup: types.ModelEntityReifiedRelationship ): Try[types.EntityRelationshipSubClassAxiom] =
    ( isTypeTermDefinedRecursively( sub ), isTypeTermDefinedRecursively( sup ) ) match {
      case ( true, true ) =>
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubClassOfAxiom( sub.e, sup.e ) ) )
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom( sub.rSource, sup.rSource ) ) )
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubObjectPropertyOfAxiom( sub.rTarget, sup.rTarget) ) )
        val ax = EntityRelationshipSubClassAxiom( sub, sup )
        Success( ax )

      case ( false, true ) =>
        Failure( AxiomScopeException( RelationshipSubclassAxiom, Map( Sub -> sub ) ) )

      case ( true, false ) =>
        Failure( AxiomScopeException( RelationshipSubclassAxiom, Map( Sup -> sup ) ) )

      case ( false, false ) =>
        Failure( AxiomScopeException( RelationshipSubclassAxiom, Map( Sub -> sub, Sup -> sup ) ) )
    }
}