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

case class ModelTerminologyGraph( protected val ont: OWLOntology ) {
  
  protected val i=scala.collection.mutable.ListBuffer[ModelTerminologyGraph]()
  protected val a=scala.collection.mutable.ListBuffer[ModelEntityAspect]()
  protected val c=scala.collection.mutable.ListBuffer[ModelEntityConcept]()
  protected val r=scala.collection.mutable.ListBuffer[ModelEntityRelationship]()
  protected val sc=scala.collection.mutable.ListBuffer[ModelScalarDataType]()
  protected val st=scala.collection.mutable.ListBuffer[ModelStructuredDataType]()
  protected val sdr=scala.collection.mutable.ListBuffer[ModelStructuredDataRelationship]()
  protected val edr=scala.collection.mutable.ListBuffer[ModelEntityDataRelationship]()
  protected val ax=scala.collection.mutable.ListBuffer[ModelTermAxiom]()
  
  protected val iri2typeTerm=scala.collection.mutable.HashMap[IRI, ModelTypeTerm]()
  
  val iri = ont.getOntologyID.getOntologyIRI.get
  
  def lookupTypeTerm( iri: IRI ): Option[ModelTypeTerm] = iri2typeTerm.get(iri)
  
  def getTerms: ( IRI, Iterable[ModelTypeTerm] ) = ( iri, iri2typeTerm.values )
  
  def fromTerminologyGraph: 
  ( 
      IRI, 
      Iterable[ModelTerminologyGraph], 
      Iterable[ModelEntityAspect], 
      Iterable[ModelEntityConcept], 
      Iterable[ModelEntityRelationship], 
      Iterable[ModelScalarDataType], 
      Iterable[ModelStructuredDataType], 
      Iterable[ModelStructuredDataRelationship], 
      Iterable[ModelEntityDataRelationship], 
      Iterable[ModelTermAxiom] ) =
        ( iri, i, a, c, r, sc, st, sdr, edr, ax)

}