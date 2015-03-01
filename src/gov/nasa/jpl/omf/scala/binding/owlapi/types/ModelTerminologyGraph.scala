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

abstract class ModelTerminologyGraph(
  val kind: TerminologyKind,
  val ont: OWLOntology,
  val entityG: Option[IRI] )( implicit val ops: OWLAPIOMFOps ) {

  val isImmutableModelTerminologyGraph: Boolean
  val isMutableModelTerminologyGraph: Boolean  
  val imports: Iterable[ModelTerminologyGraph]
  
  import ops._

  val ontManager = ont.getOWLOntologyManager
  val owlDataFactory = ontManager.getOWLDataFactory

  protected val aspects: scala.collection.Seq[ModelEntityAspect]
  protected val concepts: scala.collection.Seq[ModelEntityConcept]
  protected val relationships: scala.collection.Seq[ModelEntityRelationship]
  protected val sc: scala.collection.Seq[ModelScalarDataType]
  protected val st: scala.collection.Seq[ModelStructuredDataType]
  protected val e2sc: scala.collection.Seq[ModelDataRelationshipFromEntityToScalar]
  protected val e2st: scala.collection.Seq[ModelDataRelationshipFromEntityToStructure]
  protected val s2sc: scala.collection.Seq[ModelDataRelationshipFromStructureToScalar]
  protected val s2st: scala.collection.Seq[ModelDataRelationshipFromStructureToStructure]
  protected val ax: scala.collection.Seq[ModelTermAxiom]

  protected val iri2typeTerm: scala.collection.Map[IRI, ModelTypeTerm]

  def isTypeTermDefined( t: ModelTypeTerm ): Boolean = iri2typeTerm.values.contains( t )

  def isTypeTermDefinedRecursively( t: ModelTypeTerm ): Boolean =
    isTypeTermDefined( t ) || imports.exists ( _.isTypeTermDefinedRecursively( t ) )

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

  val iri = ont.getOntologyID.getOntologyIRI.get

  def getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition]

  def getTerms: ( IRI, Iterable[ModelTypeTerm] ) = ( iri, iri2typeTerm.values )

  def fromTerminologyGraph: ( IRI, Option[IRI], TerminologyKind, Iterable[ModelTerminologyGraph], Iterable[ModelEntityAspect], Iterable[ModelEntityConcept], Iterable[ModelEntityRelationship], Iterable[ModelScalarDataType], Iterable[ModelStructuredDataType], Iterable[ModelDataRelationshipFromEntityToScalar], Iterable[ModelDataRelationshipFromEntityToStructure], Iterable[ModelDataRelationshipFromStructureToScalar], Iterable[ModelDataRelationshipFromStructureToStructure], Iterable[ModelTermAxiom] ) =
    ( iri, entityG, kind, imports, aspects, concepts, relationships, sc, st, e2sc, e2st, s2sc, s2st, ax )

}