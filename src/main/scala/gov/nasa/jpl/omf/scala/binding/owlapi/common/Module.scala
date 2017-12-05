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

import gov.nasa.jpl.imce.oml.resolver.api.taggedTypes.ModuleUUID
import gov.nasa.jpl.imce.oml.tables.{AnnotationProperty, AnnotationPropertyValue}
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.omf.scala.binding.owlapi.{OMFBackbone, OWLAPIOMF}
import gov.nasa.jpl.omf.scala.core.ModuleSignature
import org.semanticweb.owlapi.model.{IRI, OWLOntology}

import scala.collection.immutable.Set
import scala.{Any, Boolean}
import scala.Predef.{require}

trait Module
  extends LogicalElement with Resource {

  type MS <: ModuleSignature[OWLAPIOMF]

  val sig: MS
  val ont: OWLOntology
  val ontManager = ont.getOWLOntologyManager
  val owlDataFactory = ontManager.getOWLDataFactory

  override val iri: IRI = sig.iri

  val builtInVocabulary: Boolean = {
    val iriString = iri.toString
    iriString.startsWith("http://www.w3.org/") || iriString.startsWith("http://purl.org")
  }

  /**
    * If true, this means that this Module is a representation of a vocabulary that should not be serialized to OWL.
    */
  val owlVocabularyNotToBeSerialized: Boolean = {
    val iriString = iri.toString
    iriString.startsWith("http://www.w3.org/") || iriString.startsWith("http://purl.org")
  }

  require(ont.getOntologyID.getOntologyIRI.isPresent())
  require(sig.iri == ont.getOntologyID.getOntologyIRI.get)

  val backbone: OMFBackbone

  val nsPrefix: tables.taggedTypes.NamespacePrefix = tables.taggedTypes.namespacePrefix(iri.getShortForm)

  override val name: tables.taggedTypes.LocalName = tables.taggedTypes.localName(nsPrefix)

  override val uuid: ModuleUUID

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
  = sig.annotationPropertyValues.to[Set]

}
