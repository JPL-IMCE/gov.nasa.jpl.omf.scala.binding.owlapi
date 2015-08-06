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

import java.io.OutputStream

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import scala.collection.JavaConversions._
import scala.language.postfixOps
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._

abstract class ModelTerminologyGraph
( val kind: TerminologyKind,
  val ont: OWLOntology )
( implicit val ops: OWLAPIOMFOps ) {

  require(null != kind)
  require(null != ont)
  require(null != ops)

  val isImmutableModelTerminologyGraph: Boolean
  val isMutableModelTerminologyGraph: Boolean
  
  import ops._

  val ontManager = ont.getOWLOntologyManager
  val owlDataFactory = ontManager.getOWLDataFactory

  protected val aspects: scala.collection.Seq[ModelEntityAspect]
  protected val concepts: scala.collection.Seq[ModelEntityConcept]
  protected val reifiedRelationships: scala.collection.Seq[ModelEntityReifiedRelationship]
  protected val unreifiedRelationships: scala.collection.Seq[ModelEntityUnreifiedRelationship]
  protected val sc: scala.collection.Seq[ModelScalarDataType]
  protected val st: scala.collection.Seq[ModelStructuredDataType]
  protected val e2sc: scala.collection.Seq[ModelDataRelationshipFromEntityToScalar]
  protected val e2st: scala.collection.Seq[ModelDataRelationshipFromEntityToStructure]
  protected val s2sc: scala.collection.Seq[ModelDataRelationshipFromStructureToScalar]
  protected val s2st: scala.collection.Seq[ModelDataRelationshipFromStructureToStructure]
  protected val ax: scala.collection.Seq[ModelTermAxiom]

  protected val iri2typeTerm: scala.collection.Map[IRI, ModelTypeTerm]

  def isTypeTermDefined
  ( t: ModelTypeTerm )
  : Boolean =
    iri2typeTerm.values.contains( t )

  def isTypeTermDefinedRecursively
  ( t: ModelTypeTerm )
  ( implicit store: OWLAPIOMFGraphStore )
  : Boolean =
    terminologyGraphImportClosure[OWLAPIOMF, ModelTerminologyGraph](this, onlyCompatibleKind = true).
      exists ( _.isTypeTermDefined( t ) )

  def lookupTypeTerm
  ( iri: IRI, recursively: Boolean )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ModelTypeTerm] =
  if (recursively)
    lookupTypeTermRecursively(iri)
  else
    iri2typeTerm.get( iri )

  def lookupTypeTerm
  ( iri: Option[IRI], recursively: Boolean )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ModelTypeTerm] =
    for {
      _iri <- iri
      _t <- lookupTypeTerm( _iri, recursively )
    } yield _t

  def lookupTypeTermRecursively
  ( iri: IRI )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ModelTypeTerm] =
    terminologyGraphImportClosure[OWLAPIOMF, ModelTerminologyGraph](this, onlyCompatibleKind = true).
      flatMap(_.lookupTypeTerm( iri, recursively=false )).headOption

  def lookupTypeTermRecursively
  ( iri: Option[IRI] )
  ( implicit store: OWLAPIOMFGraphStore )
  : Option[ModelTypeTerm] =
    for {
      _iri <- iri
      _t <- lookupTypeTermRecursively( _iri )
    } yield _t

  val iri: IRI = ont.getOntologyID.getOntologyIRI.get

  val kindIRI: IRI

  protected def makeKindIRI(kind: String): IRI =
    iri.resolve(iri.getRemainder.or("")+"?kind="+kind)

  def getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition]

  def getScalarDatatypeDefinitionMap: Map[OWLDatatype, ModelScalarDataType]

  def getTerms: ( IRI, Iterable[ModelTypeTerm] ) = ( iri, iri2typeTerm.values )

  def fromTerminologyGraph
  ( nesting: Option[ModelTerminologyGraph],
    nested: Iterable[ModelTerminologyGraph],
    extended: Iterable[ModelTerminologyGraph] )
  : OWLAPITerminologyGraphSignature =
    OWLAPITerminologyGraphSignature(
      iri, kind, nesting, nested,
      extended, aspects, concepts,
      reifiedRelationships, unreifiedRelationships,
      sc, st, e2sc, e2st, s2sc, s2st,
      ax )

  def getTerminologyGraphShortNameAnnotation
  : Option[OWLAnnotation] =
    ont.getAnnotations.find( _.getProperty.getIRI == ops.OMF_TBox_DataProperty_HasShortName )

  def getTerminologyGraphShortName
  : Option[String] =
    getTerminologyGraphShortNameAnnotation.
      flatMap ( _.getValue match {
      case l: OWLLiteral =>
        Some( l.getLiteral )
      case _  =>
        None
  } )


  def getTerminologyGraphUUIDAnnotation
  : Option[OWLAnnotation] =
    ont.getAnnotations.find( _.getProperty.getIRI == ops.OMF_TBox_DataProperty_HasUUID )

  def getTerminologyGraphUUID
  : Option[String] =
    getTerminologyGraphUUIDAnnotation.
      flatMap ( _.getValue match {
      case l: OWLLiteral =>
        Some( l.getLiteral )
      case _  =>
        None
    } )

  def getTermShortNameAnnotationAssertionAxiom
  ( term: types.ModelTypeTerm )
  : Option[OWLAnnotationAssertionAxiom] =
    ont.getAnnotationAssertionAxioms( term.iri ).
      find( _.getProperty.getIRI == ops.rdfs_label )

  def getTermUUIDAnnotationAssertionAxiom
  ( term: types.ModelTypeTerm )
  : Option[OWLAnnotationAssertionAxiom] =
    ont.getAnnotationAssertionAxioms( term.iri ).
      find( _.getProperty.getIRI == ops.AnnotationHasUUID )


  def save( saveIRI: IRI ): Try[Unit] = Try {
    ontManager.saveOntology( ont, saveIRI )
  }

  def save( os: OutputStream ): Try[Unit] = Try {
    ontManager.saveOntology( ont, os )
  }
}