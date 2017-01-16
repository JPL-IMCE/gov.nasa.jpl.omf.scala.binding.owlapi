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

import java.io.OutputStream

import gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies.ImmutableTerminologyGraph
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.IRI

import scala.collection.immutable._
import scala.{StringContext, Unit}
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import gov.nasa.jpl.omf.scala.core.OMFError

abstract class ModelInstanceGraph
(val tboxes: scala.collection.Iterable[ImmutableTerminologyGraph],
 val imports: scala.collection.Iterable[ImmutableModelInstanceGraph],
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
      Iterable[ImmutableTerminologyGraph],
      Iterable[ImmutableModelInstanceGraph], 
      Iterable[ModelInstanceObject], 
      Iterable[ModelInstanceRelation], 
      Iterable[ModelInstanceDataLiteral], 
      Iterable[ModelInstanceDataStructure], 
      Iterable[ModelInstanceDataRelationshipFromEntityToScalar],
      Iterable[ModelInstanceDataRelationshipFromEntityToStructure],
      Iterable[ModelInstanceDataRelationshipFromStructureToScalar],
      Iterable[ModelInstanceDataRelationshipFromStructureToStructure]) =
    ( iri,
      tboxes.to[Iterable],
      imports.to[Iterable],
      objects.to[Iterable],
      relations.to[Iterable],
      dataLiterals.to[Iterable],
      dataObjects.to[Iterable],
      e2sc.to[Iterable],
      e2st.to[Iterable],
      s2sc.to[Iterable],
      s2st.to[Iterable] )

  def save
  : Set[java.lang.Throwable] \/ Unit =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            OMFError.omfException(
              s"saving ModelInstanceGraph failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        ontManager.saveOntology(ont).right
      })

  def save( os: OutputStream )
  : Set[java.lang.Throwable] \/ Unit =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            OMFError.omfException(
              s"saving ModelInstanceGraph failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        ontManager.saveOntology(ont, os).right
      })

}