/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
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
package gov.nasa.jpl.omf.scala.binding.owlapi.instances

import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.IRI
import scala.collection.immutable._

case class MutableModelInstanceGraph(
    override val tboxes: Iterable[types.ImmutableModelTerminologyGraph],
    override val imports: Iterable[ImmutableModelInstanceGraph], 
    override protected val ont: OWLOntology ) 
    extends ModelInstanceGraph( tboxes, imports, ont ) {
    
  override protected val objects = scala.collection.mutable.ListBuffer[ModelInstanceObject]()
   override protected val relations = scala.collection.mutable.ListBuffer[ModelInstanceRelation]()
   override protected val dataLiterals = scala.collection.mutable.ListBuffer[ModelInstanceDataLiteral]()
   override protected val dataObjects = scala.collection.mutable.ListBuffer[ModelInstanceDataStructure]()
   override protected val e2sc = scala.collection.mutable.ListBuffer[ModelInstanceDataRelationshipFromEntityToScalar]()
   override protected val e2st = scala.collection.mutable.ListBuffer[ModelInstanceDataRelationshipFromEntityToStructure]()
   override protected val s2sc = scala.collection.mutable.ListBuffer[ModelInstanceDataRelationshipFromStructureToScalar]()
   override protected val s2st = scala.collection.mutable.ListBuffer[ModelInstanceDataRelationshipFromStructureToStructure]()
    
  protected val iri2namedIndividual = scala.collection.mutable.HashMap[IRI, ModelNamedIndividual]()

}