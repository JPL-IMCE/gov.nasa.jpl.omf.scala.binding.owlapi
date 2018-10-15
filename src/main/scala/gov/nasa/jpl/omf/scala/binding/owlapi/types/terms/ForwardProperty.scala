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

package gov.nasa.jpl.omf.scala.binding.owlapi.types.terms

import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.imce.oml.tables
import gov.nasa.jpl.omf.scala.binding.owlapi.common.LogicalElement
import gov.nasa.jpl.omf.scala.binding.owlapi.{OWLAPIOMF, OWLAPIOMFGraphStore}
import org.semanticweb.owlapi.model.OWLObjectProperty

import scala.{Any, Boolean, Int}

case class ForwardProperty
(override val e: OWLObjectProperty,
 override val uuid: resolver.api.taggedTypes.ForwardPropertyUUID,
 override val iri: OWLAPIOMF#IRI,
 override val name: tables.taggedTypes.LocalName)
  extends RestrictableRelationship with LogicalElement{

  override def domain
  ()(implicit store: OWLAPIOMFGraphStore)
  : Entity
  = store.relation(this).source

  override def range
  ()(implicit store: OWLAPIOMFGraphStore)
  : Entity
  = store.relation(this).target

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ForwardProperty => true
    case _ => false
  }

  override val hashCode
  : Int
  = (uuid, name, iri).##

  override def equals(other: Any): Boolean = other match {
    case that: ForwardProperty =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.iri == that.iri)
    case _ =>
      false
  }
}
