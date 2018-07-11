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

import gov.nasa.jpl.imce.oml.resolver.api.taggedTypes.ConceptualRelationshipUUID
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFGraphStore
import org.semanticweb.owlapi.model.OWLClass

import scala.collection.immutable.Set
import scala.{Any, Boolean, None, Some}

trait ConceptualRelationship extends ConceptualEntity with EntityRelationship {
  override val e: OWLClass

  def rootCharacterizedEntityRelationships
  ()(implicit store: OWLAPIOMFGraphStore)
  : Set[_ <: CharacterizedEntityRelationship]

  def rootReifiedRelationships
  ()(implicit store: OWLAPIOMFGraphStore)
  : Set[ReifiedRelationship]
  = rootCharacterizedEntityRelationships().flatMap {
    case rr: ReifiedRelationship =>
      Some(rr)
    case _ =>
      None
  }

  override val uuid: ConceptualRelationshipUUID

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ConceptualRelationship => true
    case _ => false
  }
}
