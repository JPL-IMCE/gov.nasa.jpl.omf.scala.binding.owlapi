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

package gov.nasa.jpl.omf.scala.binding.owlapi.types

import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.model.IRI

import scala.{Any, Boolean, Int}
import scala.collection.immutable._
import scala.Predef.require

case class ModelEntityUnreifiedRelationship
( e: OWLObjectProperty,
  override val name: LocalName,
  override val uuid: UUID,
  source: ModelEntityDefinition,
  rSource: OWLObjectProperty,
  target: ModelEntityDefinition,
  rTarget: OWLObjectProperty,
  characteristics: Iterable[RelationshipCharacteristics] )
  extends ModelTypeTerm {

  require(null != e)
  require(null != source)
  require(null != rSource)
  require(null != target)
  require(null != rTarget)
  require(null != characteristics)

  override val iri: IRI = e.getIRI

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ModelEntityUnreifiedRelationship => true
    case _ => false
  }

  override val hashCode
  : Int
  = (uuid, name, source, target, e, rSource, rTarget, characteristics).##

  override def equals(other: Any): Boolean = other match {
    case that: ModelEntityUnreifiedRelationship =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.source == that.source) &&
        (this.target == that.target) &&
        (this.e == that.e) &&
        (this.rSource == that.rSource) &&
        (this.rTarget == that.rTarget) &&
        (this.characteristics.to[Set] == that.characteristics.to[Set])
    case _ =>
      false
  }
}