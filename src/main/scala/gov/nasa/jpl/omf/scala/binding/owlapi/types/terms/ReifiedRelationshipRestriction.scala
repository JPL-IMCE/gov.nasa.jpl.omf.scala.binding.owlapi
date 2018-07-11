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

import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables.taggedTypes.LocalName
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFGraphStore
import org.semanticweb.owlapi.model.{IRI, OWLClass}

import scala.collection.immutable.Set
import scala.{Any, Boolean, Int}
import scala.Predef.require

case class ReifiedRelationshipRestriction
( override val e: OWLClass,
  override val iri: IRI,
  override val name: LocalName,
  override val uuid: api.taggedTypes.ReifiedRelationshipRestrictionUUID,
  override val source: Entity,
  override val target: Entity)
  extends ConceptualRelationship {

  require(null != e)
  require(null != source)
  require(null != target)

  override def rootCharacterizedEntityRelationships
  ()(implicit store: OWLAPIOMFGraphStore)
  : Set[_ <: CharacterizedEntityRelationship]
  = store.rootCharacterizedEntityRelationships(this)

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ReifiedRelationshipRestriction => true
    case _ => false
  }

  override val hashCode
  : Int
  = (uuid, name, source, target, e).##

  override def equals(other: Any): Boolean = other match {
    case that: ReifiedRelationshipRestriction =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.source == that.source) &&
        (this.target == that.target) &&
        (this.e == that.e)
    case _ =>
      false
  }
}