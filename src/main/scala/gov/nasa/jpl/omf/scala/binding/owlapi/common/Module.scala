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

package gov.nasa.jpl.omf.scala.binding.owlapi.common

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue}
import gov.nasa.jpl.omf.scala.binding.owlapi.{OMFBackbone, OWLAPIOMF}
import gov.nasa.jpl.omf.scala.core.OMLString.{LocalName, NamespacePrefix}
import gov.nasa.jpl.omf.scala.core.{ModuleSignature, generateUUID}
import org.semanticweb.owlapi.model.{IRI, OWLOntology}

import scala.collection.immutable.Set
import scala.{Any, Boolean}
import scala.Predef.{require}

trait Module
  extends Element with Resource {

  type MS <: ModuleSignature[OWLAPIOMF]

  val sig: MS
  val ont: OWLOntology
  val ontManager = ont.getOWLOntologyManager
  val owlDataFactory = ontManager.getOWLDataFactory

  override val iri: IRI = sig.iri

  require(ont.getOntologyID.getOntologyIRI.isPresent())
  require(sig.iri == ont.getOntologyID.getOntologyIRI.get)

  val backbone: OMFBackbone

  val nsPrefix: NamespacePrefix = NamespacePrefix.apply(iri.getShortForm)

  override val name: LocalName = LocalName.apply(NamespacePrefix.unwrap(nsPrefix))

  override val uuid: UUID = generateUUID(iri.toString)

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: Module => true
    case _ => false
  }

  final def annotationProperties
  ()
  : Set[AnnotationProperty]
  = sig.annotationProperties.to[Set]

  final def annotations
  ()
  : Set[AnnotationPropertyValue]
  = sig.annotations.to[Set]

}
