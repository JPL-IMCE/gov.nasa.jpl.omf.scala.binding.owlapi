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
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLObjectProperty

import scala.collection.immutable._
import scala.{Any, Boolean, Int, Option}
import scala.Predef.require

case class ModelEntityReifiedRelationship
(override val e: OWLClass,
 override val name: LocalName,
 override val uuid: UUID,
 unreified: OWLObjectProperty,
 inverse: Option[OWLObjectProperty],
 source: ModelEntityDefinition, rSource: OWLObjectProperty,
 target: ModelEntityDefinition, rTarget: OWLObjectProperty,
 characteristics: Iterable[RelationshipCharacteristics],
 isAbstract: Boolean)
  extends ModelEntityDefinition(e, name, uuid) {

  require(null != e)
  require(null != inverse)
  require(null != source)
  require(null != target)
  require(null != characteristics)

  override val iri = e.getIRI

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ModelEntityReifiedRelationship => true
    case _ => false
  }

  override val hashCode
  : Int
  = (uuid, name, source, target, isAbstract, e, rSource, rTarget, unreified, inverse, characteristics).##

  override def equals(other: Any): Boolean = other match {
    case that: ModelEntityReifiedRelationship =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.source == that.source) &&
        (this.target == that.target) &&
        (this.isAbstract == that.isAbstract) &&
        (this.e == that.e) &&
        (this.rSource == that.rSource) &&
        (this.rTarget == that.rTarget) &&
        (this.unreified == that.unreified) &&
        (this.inverse == that.inverse) &&
        (this.characteristics.to[Set] == that.characteristics.to[Set])
    case _ =>
      false
  }
}