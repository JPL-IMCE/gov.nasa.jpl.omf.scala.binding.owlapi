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
package gov.nasa.jpl.omf.scala.binding.owlapi.instances

import java.io.OutputStream

import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.IRI
import scala.collection.immutable._
import scala.{StringContext,Unit}
import scala.util.control.Exception._
import scalaz._, Scalaz._

import gov.nasa.jpl.omf.scala.core.OMFError
import gov.nasa.jpl.omf.scala.binding.owlapi.types.ImmutableModelTerminologyGraph

abstract class ModelInstanceGraph
( val tboxes: scala.collection.Iterable[gov.nasa.jpl.omf.scala.binding.owlapi.types.ImmutableModelTerminologyGraph],
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
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          NonEmptyList(
            OMFError.omfException(
              s"saving ModelInstanceGraph failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        ontManager.saveOntology(ont).right
      })

  def save( os: OutputStream )
  : NonEmptyList[java.lang.Throwable] \/ Unit =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          NonEmptyList(
            OMFError.omfException(
              s"saving ModelInstanceGraph failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        ontManager.saveOntology(ont, os).right
      })

}