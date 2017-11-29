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

package gov.nasa.jpl.omf.scala.binding.owlapi.descriptions

import java.io.OutputStream

import gov.nasa.jpl.imce.oml.resolver.api.taggedTypes.DescriptionBoxUUID
import gov.nasa.jpl.omf.scala.binding.owlapi.{OWLAPIOMF, OWLAPIOMFOps}
import gov.nasa.jpl.omf.scala.binding.owlapi.common.Module

import scala.collection.immutable._
import scala.{Any, Boolean, StringContext, Unit}
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import gov.nasa.jpl.omf.scala.core.{DescriptionBoxSignature, OMFError}
import org.semanticweb.owlapi.model.IRI

trait DescriptionBox extends Module {

  override type MS <: DescriptionBoxSignature[OWLAPIOMF, scala.collection.Iterable]

  override val sig: MS

  override val uuid: DescriptionBoxUUID = sig.uuid.asInstanceOf[DescriptionBoxUUID]

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: DescriptionBox => true
    case _ => false
  }

  implicit val ops: OWLAPIOMFOps

  def save(saveIRI: IRI)
  : Set[java.lang.Throwable] \/ Unit
  = nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            OMFError.omfException(
              s"saving DescriptionBox failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        ontManager.saveOntology(ont, saveIRI).right
      })

  def save( os: OutputStream )
  : Set[java.lang.Throwable] \/ Unit =
    nonFatalCatch[Unit]
      .withApply {
        (cause: java.lang.Throwable) =>
          Set(
            OMFError.omfException(
              s"saving DescriptionBox failed: ${cause.getMessage}",
              cause)
          ).left
      }
      .apply({
        ontManager.saveOntology(ont, os).right
      })

}