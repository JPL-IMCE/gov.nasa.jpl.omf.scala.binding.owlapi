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

package gov.nasa.jpl.omf.scala.binding.owlapi.types.termAxioms

import java.util.UUID

import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.{DataRange, Entity, EntityScalarDataProperty}

import scala.{Any,Boolean,Int}

case class EntityScalarDataPropertyExistentialRestrictionAxiom
(override val uuid: UUID,
 override val restrictedEntity: Entity,
 override val scalarProperty: EntityScalarDataProperty,
 scalarRestriction: DataRange)
extends EntityScalarDataPropertyRestrictionAxiom {

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: EntityScalarDataPropertyExistentialRestrictionAxiom => true
    case _ => false
  }

  override val hashCode
  : Int
  = (uuid, restrictedEntity, scalarProperty, scalarRestriction).##

  override def equals(other: Any): Boolean = other match {
    case that: EntityScalarDataPropertyExistentialRestrictionAxiom =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.restrictedEntity == that.restrictedEntity) &&
        (this.scalarProperty == that.scalarProperty) &&
        (this.scalarRestriction == that.scalarRestriction)
    case _ =>
      false
  }

}
