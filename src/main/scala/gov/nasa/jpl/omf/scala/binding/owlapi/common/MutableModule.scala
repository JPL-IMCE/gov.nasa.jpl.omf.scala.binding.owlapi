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
import gov.nasa.jpl.omf.scala.binding.owlapi.{OWLAPIOMF, OWLAPIOMFGraphStore}
import gov.nasa.jpl.omf.scala.core.OMFError
import org.semanticweb.owlapi.model.OWLAnnotation

import scala.Predef.String
import scalaz.\/

trait MutableModule extends Module {

  def addAnnotationProperty
  (ap: AnnotationProperty)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ AnnotationProperty

  def addAnnotation
  (subject: OWLAPIOMF#Element,
   property: AnnotationProperty,
   value: String)
  (implicit store: OWLAPIOMFGraphStore)
  : OMFError.Throwables \/ AnnotationPropertyValue

  def createOMLProvenanceAnnotation
  (uuid: String)
  (implicit store: OWLAPIOMFGraphStore)
  : OWLAnnotation
  = owlDataFactory.getOWLAnnotation(store.ANNOTATION_HAS_UUID, owlDataFactory.getOWLLiteral(uuid))

  def createOMLProvenanceAnnotation
  (uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : OWLAnnotation
  = owlDataFactory.getOWLAnnotation(store.ANNOTATION_HAS_UUID, owlDataFactory.getOWLLiteral(uuid.toString))

  def createOMLProvenanceAnnotations
  (uuid: String)
  (implicit store: OWLAPIOMFGraphStore)
  : java.util.Collection[OWLAnnotation]
  = java.util.Collections.singleton(createOMLProvenanceAnnotation(uuid))

  def createOMLProvenanceAnnotations
  (uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : java.util.Collection[OWLAnnotation]
  = java.util.Collections.singleton(createOMLProvenanceAnnotation(uuid))

  def createOMLProvenanceAnnotationsWithLabel
  (name: String, uuid: UUID)
  (implicit store: OWLAPIOMFGraphStore)
  : java.util.Collection[OWLAnnotation]
  = {
    val anns = new java.util.ArrayList[OWLAnnotation]()
    anns.add(owlDataFactory.getOWLAnnotation(
      owlDataFactory.getRDFSLabel(),
      owlDataFactory.getOWLLiteral(name, owlDataFactory.getStringOWLDatatype)))
    anns.add(createOMLProvenanceAnnotation(uuid))
    anns
  }
}
