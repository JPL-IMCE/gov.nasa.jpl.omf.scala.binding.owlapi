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
package gov.nasa.jpl.omf.scala.binding.owlapi.types

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

case class ModelTerminologyGraph( val imports: Iterable[ModelTerminologyGraph], protected val ont: OWLOntology ) {

  val ontManager = ont.getOWLOntologyManager
  val owlDataFactory = ontManager.getOWLDataFactory

  protected val aspects = scala.collection.mutable.ListBuffer[ModelEntityAspect]()
  protected val concepts = scala.collection.mutable.ListBuffer[ModelEntityConcept]()
  protected val relationships = scala.collection.mutable.ListBuffer[ModelEntityRelationship]()
  protected val sc = scala.collection.mutable.ListBuffer[ModelScalarDataType]()
  protected val st = scala.collection.mutable.ListBuffer[ModelStructuredDataType]()
  protected val e2sc = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromEntityToScalar]()
  protected val e2st = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromEntityToStructure]()
  protected val s2sc = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromStructureToScalar]()
  protected val s2st = scala.collection.mutable.ListBuffer[ModelDataRelationshipFromStructureToStructure]()
  protected val ax = scala.collection.mutable.ListBuffer[ModelTermAxiom]()

  protected val iri2typeTerm = scala.collection.mutable.HashMap[IRI, ModelTypeTerm]()

  def isTypeTermDefined( t: ModelTypeTerm ): Boolean = iri2typeTerm.values.contains( t )

  def isTypeTermDefinedRecursively( t: ModelTypeTerm ): Boolean =
    isTypeTermDefined( t ) ||
      { ( ( imports.view flatMap { case i if ( i.isTypeTermDefinedRecursively( t ) ) => Some( true ) } ) ++ Some( false ) ) head }

  def lookupTypeTerm( iri: IRI ): Option[ModelTypeTerm] = iri2typeTerm.get( iri )

  def lookupTypeTerm( iri: Option[IRI] ): Option[ModelTypeTerm] =
    for {
      _iri <- iri
      _t <- lookupTypeTerm( _iri )
    } yield _t

  def lookupTypeTermRecursively( iri: IRI ): Option[ModelTypeTerm] =
    lookupTypeTerm( iri ).orElse( { imports.view flatMap { _.lookupTypeTermRecursively( iri ) } headOption } )

  def lookupTypeTermRecursively( iri: Option[IRI] ): Option[ModelTypeTerm] =
    for {
      _iri <- iri
      _t <- lookupTypeTermRecursively( _iri )
    } yield _t

  ont.getDatatypesInSignature( Imports.EXCLUDED ) foreach ( createModelScalarDataType( _ ) )

  val iri = ont.getOntologyID.getOntologyIRI.get

  def getTerms: ( IRI, Iterable[ModelTypeTerm] ) = ( iri, iri2typeTerm.values )

  def fromTerminologyGraph: ( IRI, Iterable[ModelTerminologyGraph], Iterable[ModelEntityAspect], Iterable[ModelEntityConcept], Iterable[ModelEntityRelationship], Iterable[ModelScalarDataType], Iterable[ModelStructuredDataType], Iterable[ModelDataRelationshipFromEntityToScalar], Iterable[ModelDataRelationshipFromEntityToStructure], Iterable[ModelDataRelationshipFromStructureToScalar], Iterable[ModelDataRelationshipFromStructureToStructure], Iterable[ModelTermAxiom] ) =
    ( iri, imports, aspects, concepts, relationships, sc, st, e2sc, e2st, s2sc, s2st, ax )

  protected def createModelEntityAspect( a: OWLClass ): ModelEntityAspect = {
    val _a = ModelEntityAspect( a.getIRI )
    aspects += _a
    iri2typeTerm += a.getIRI -> _a
    _a
  }

  def addEntityAspect( aspectIRI: IRI ): Try[types.ModelEntityAspect] = iri2typeTerm get aspectIRI match {
    case None =>
      val aspectC = owlDataFactory.getOWLClass( aspectIRI )
      val aspectTerm = createModelEntityAspect( aspectC )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( aspectC ) ) )
      Success( aspectTerm )

    case Some( term ) =>
      Failure( new IllegalArgumentException( s"Cannot create an entity aspect with IRI='${aspectIRI}' because this IRI is a ${term}" ) )
  }

  protected def createModelEntityConcept( c: OWLClass, isAbstract: Boolean ): ModelEntityConcept = {
    val _c = ModelEntityConcept( c.getIRI, isAbstract )
    concepts += _c
    iri2typeTerm += c.getIRI -> _c
    _c
  }

  def addEntityConcept( conceptIRI: IRI, isAbstract: Boolean ): Try[types.ModelEntityConcept] = iri2typeTerm get conceptIRI match {
    case None =>
      val conceptC = owlDataFactory.getOWLClass( conceptIRI )
      val conceptTerm = createModelEntityConcept( conceptC, isAbstract )
      ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( conceptC ) ) )
      Success( conceptTerm )

    case Some( term ) =>
      Failure( new IllegalArgumentException( s"Cannot create an entity concept with IRI='${conceptIRI}' because this IRI is a ${term}" ) )
  }

  protected def createEntityRelationship(
    r: OWLClass, u: OWLObjectProperty, ui: Option[OWLObjectProperty],
    source: ModelEntityDefinition, target: ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics], isAbstract: Boolean ): types.ModelEntityRelationship = {
    val _term = ModelEntityRelationship(
      r.getIRI, u.getIRI, if ( ui.isEmpty ) None else Some( ui.get.getIRI ),
      source, target, characteristics, isAbstract )
    relationships += _term
    iri2typeTerm += r.getIRI -> _term
    _term
  }

  def addEntityRelationship(
    rIRI: IRI, uIRI: IRI, uiIRI: Option[IRI],
    source: ModelEntityDefinition, target: ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics], isAbstract: Boolean ): Try[types.ModelEntityRelationship] =
    ( lookupTypeTerm( rIRI ),
      lookupTypeTerm( uIRI ),
      lookupTypeTerm( uiIRI ) ) match {
        case ( None, None, None ) =>
          ( isTypeTermDefinedRecursively( source ), isTypeTermDefinedRecursively( target ) ) match {
            case ( true, true ) =>
              val r = owlDataFactory.getOWLClass( rIRI )
              val u = owlDataFactory.getOWLObjectProperty( uIRI )
              val ui = if ( uiIRI.isEmpty ) None else Some( owlDataFactory.getOWLObjectProperty( uiIRI.get ) )
              val term = createEntityRelationship( r, u, ui, source, target, characteristics, isAbstract )
              ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( r ) ) )
              ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( u ) ) )
              if ( ui.isDefined ) ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( ui.get ) ) )
              Success( term )

            case ( false, true ) =>
              Failure( new IllegalArgumentException( s"Cannot create a entity relationship with IRI='${rIRI}' because the source, '${source}', is out of scope of the graph" ) )

            case ( true, false ) =>
              Failure( new IllegalArgumentException( s"Cannot create a entity relationship with IRI='${rIRI}' because the target, '${target}', is out of scope of the graph" ) )

            case ( false, false ) =>
              Failure( new IllegalArgumentException( s"Cannot create a entity relationship with IRI='${rIRI}' because the source, '${source}', and target, '${target}', are out of scope of the graph" ) )
          }

        case ( Some( t ), _, _ ) =>
          Failure( new IllegalArgumentException( s"Cannot create a entity relationship with IRI='${rIRI}' because this IRI is a ${t}" ) )

        case ( _, Some( t ), _ ) =>
          Failure( new IllegalArgumentException( s"Cannot create a entity relationship with IRI='${uIRI}' because this IRI is a ${t}" ) )

        case ( _, _, Some( t ) ) =>
          Failure( new IllegalArgumentException( s"Cannot create a entity relationship with IRI='${uiIRI.get}' because this IRI is a ${t}" ) )
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
      Failure( new IllegalArgumentException( s"Cannot create a scalar datatype with IRI='${scalarIRI}' because this IRI is a ${term}" ) )
  }

  def createDataRelationshipFromEntityToScalar( esc: OWLDatatype, source: ModelEntityDefinition, target: ModelScalarDataType ): ModelDataRelationshipFromEntityToScalar = {
    val _esc = ModelDataRelationshipFromEntityToScalar( esc.getIRI, source, target )
    e2sc += _esc
    iri2typeTerm += esc.getIRI -> _esc
    _esc
  }

  def addDataRelationshipFromEntityToScalar(
    dIRI: IRI,
    source: types.ModelEntityDefinition,
    target: types.ModelScalarDataType ): Try[types.ModelDataRelationshipFromEntityToScalar] = iri2typeTerm get dIRI match {
    case None =>
      ( isTypeTermDefinedRecursively( source ), isTypeTermDefinedRecursively( target ) ) match {
        case ( true, true ) =>
          val escDT = owlDataFactory.getOWLDatatype( dIRI )
          val escTerm = createDataRelationshipFromEntityToScalar( escDT, source, target )
          ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLDeclarationAxiom( escDT ) ) )
          Success( escTerm )

        case ( false, true ) =>
          Failure( new IllegalArgumentException( s"Cannot create a data relationship from entity to scalar with IRI='${dIRI}' because the source, '${source}', is out of scope of the graph" ) )

        case ( true, false ) =>
          Failure( new IllegalArgumentException( s"Cannot create a data relationship from entity to scalar with IRI='${dIRI}' because the target, '${target}', is out of scope of the graph" ) )

        case ( false, false ) =>
          Failure( new IllegalArgumentException( s"Cannot create a data relationship from entity to scalar with IRI='${dIRI}' because the source, '${source}', and target, '${target}', are out of scope of the graph" ) )
      }

    case Some( term ) =>
      Failure( new IllegalArgumentException( s"Cannot create a data relationship from entity to scalar with IRI='${dIRI}' because this IRI is a ${term}" ) )
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
        Failure( new IllegalArgumentException( s"Cannot create an entity concept subclass axiom because the sub, '${sub}', is out of scope of the graph" ) )

      case ( true, false ) =>
        Failure( new IllegalArgumentException( s"Cannot create an entity concept subclass axiom because the sup, '${sup}', is out of scope of the graph" ) )

      case ( false, false ) =>
        Failure( new IllegalArgumentException( s"Cannot create an entity concept subclass axiom because the sub, '${sub}', and sup, '${sup}', are out of scope of the graph" ) )
    }

  def addEntityDefinitionAspectSubClassAxiom(
    sub: types.ModelEntityDefinition,
    sup: types.ModelEntityAspect ): Try[types.EntityDefinitionAspectSubClassAxiom] =
    ( isTypeTermDefinedRecursively( sub ), isTypeTermDefinedRecursively( sup ) ) match {
      case ( true, true ) =>
        val subC = owlDataFactory.getOWLClass( sub.iri )
        val supC = owlDataFactory.getOWLClass( sup.iri )
        ontManager.applyChange( new AddAxiom( ont, owlDataFactory.getOWLSubClassOfAxiom( subC, supC ) ) )
        val ax = EntityDefinitionAspectSubClassAxiom( sub, sup )
        Success( ax )

      case ( false, true ) =>
        Failure( new IllegalArgumentException( s"Cannot create an entity definition aspect subclass axiom because the sub, '${sub}', is out of scope of the graph" ) )

      case ( true, false ) =>
        Failure( new IllegalArgumentException( s"Cannot create an entity definition aspect subclass axiom because the sup, '${sup}', is out of scope of the graph" ) )

      case ( false, false ) =>
        Failure( new IllegalArgumentException( s"Cannot create an entity definition aspect subclass axiom because the sub, '${sub}', and sup, '${sup}', are out of scope of the graph" ) )
    }

}