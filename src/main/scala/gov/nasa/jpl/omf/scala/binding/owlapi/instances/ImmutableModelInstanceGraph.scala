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

import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.ImmutableTerminologyGraph
import org.semanticweb.owlapi.model.OWLOntology

import scala.collection.immutable._

case class ImmutableModelInstanceGraph(
                                        override val tboxes: Iterable[ImmutableTerminologyGraph],
                                        override val imports: Iterable[ImmutableModelInstanceGraph],
                                        override protected val ont: OWLOntology,
                                        override protected val objects: Vector[ModelInstanceObject],
                                        override protected val relations: Vector[ModelInstanceRelation],
                                        override protected val dataLiterals: Vector[ModelInstanceDataLiteral],
                                        override protected val dataObjects: Vector[ModelInstanceDataStructure],
                                        override protected val e2sc: Vector[ModelInstanceDataRelationshipFromEntityToScalar],
                                        override protected val e2st: Vector[ModelInstanceDataRelationshipFromEntityToStructure],
                                        override protected val s2sc: Vector[ModelInstanceDataRelationshipFromStructureToScalar],
                                        override protected val s2st: Vector[ModelInstanceDataRelationshipFromStructureToStructure] )
  extends ModelInstanceGraph( tboxes, imports, ont )