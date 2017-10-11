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

import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.{Entity, EntityRelationship}

import scala.{Any,Boolean,Int}

case class EntityUniversalRestrictionAxiom
(override val uuid: UUID,
 override val restrictedDomain: Entity,
 override val restrictedRelation: EntityRelationship,
 override val restrictedRange: Entity )
  extends EntityRestrictionAxiom {

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: EntityUniversalRestrictionAxiom => true
    case _ => false
  }

  override val hashCode: Int = (uuid, restrictedDomain, restrictedRelation, restrictedRange).##

  override def equals(other: Any): Boolean = other match {
    case that: EntityUniversalRestrictionAxiom =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.restrictedDomain == that.restrictedDomain) &&
        (this.restrictedRelation == that.restrictedRelation) &&
        (this.restrictedRange == that.restrictedRange)
    case _ =>
      false
  }
}