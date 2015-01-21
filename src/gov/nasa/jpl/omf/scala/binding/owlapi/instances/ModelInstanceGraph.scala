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
package gov.nasa.jpl.omf.scala.binding.owlapi.instances

import gov.nasa.jpl.omf.scala.binding._
import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.IRI
import scala.util.Try
import gov.nasa.jpl.omf.scala.binding.owlapi.types.ModelTerminologyGraph
import gov.nasa.jpl.omf.scala.binding.owlapi.types.ImmutableModelTerminologyGraph

abstract class ModelInstanceGraph(
    val tboxes: Iterable[types.ImmutableModelTerminologyGraph],
    val imports: Iterable[ImmutableModelInstanceGraph], 
    protected val ont: OWLOntology ) {
    
  protected val objects: scala.collection.Seq[ModelInstanceObject]
  protected val relations: scala.collection.Seq[ModelInstanceRelation]
  protected val dataLiterals: scala.collection.Seq[ModelInstanceDataLiteral]
  protected val dataObjects: scala.collection.Seq[ModelInstanceDataStructure]
  protected val e2sc: scala.collection.Seq[ModelInstanceDataRelationshipFromEntityToScalar]
  protected val e2st: scala.collection.Seq[ModelInstanceDataRelationshipFromEntityToStructure]
  protected val s2sc: scala.collection.Seq[ModelInstanceDataRelationshipFromStructureToScalar]
  protected val s2st: scala.collection.Seq[ModelInstanceDataRelationshipFromStructureToStructure]
    
  val ontManager = ont.getOWLOntologyManager
  val owlDataFactory = ontManager.getOWLDataFactory
    
  val iri = ont.getOntologyID.getOntologyIRI.get
  
  def fromInstanceGraph: ( 
      IRI, 
      Iterable[ImmutableModelTerminologyGraph], 
      Iterable[ImmutableModelInstanceGraph], 
      Iterable[ModelInstanceObject], 
      Iterable[ModelInstanceRelation], 
      Iterable[ModelInstanceDataLiteral], 
      Iterable[ModelInstanceDataStructure], 
      Iterable[ModelInstanceDataRelationshipFromEntityToScalar],
      Iterable[ModelInstanceDataRelationshipFromEntityToStructure],
      Iterable[ModelInstanceDataRelationshipFromStructureToScalar],
      Iterable[ModelInstanceDataRelationshipFromStructureToStructure]) =
    ( iri, tboxes, imports,
        objects, relations, dataLiterals, dataObjects, 
        e2sc, e2st, s2sc, s2st )

}