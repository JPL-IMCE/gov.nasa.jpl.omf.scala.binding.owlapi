/*
 * Copyright 2015 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
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