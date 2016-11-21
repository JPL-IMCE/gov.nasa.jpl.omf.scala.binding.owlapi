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

import scala.{Any, Boolean, Int}
import scala.Predef.require

case class EntityReifiedRelationshipSubClassAxiom
(uuid: UUID,
 sub: ModelEntityReifiedRelationship,
 sup: ModelEntityReifiedRelationship)
  extends ModelTermAxiom {

    require( null != sub )
    require( null != sup )

    override def canEqual(other: Any)
    : Boolean
    = other match {
        case _: EntityReifiedRelationshipSubClassAxiom => true
        case _ => false
    }

    override val hashCode: Int = (uuid, sub, sup).##

    override def equals(other: Any): Boolean = other match {
        case that: EntityReifiedRelationshipSubClassAxiom =>
            (that canEqual this) &&
              (this.uuid == that.uuid) &&
              (this.sub == that.sub) &&
              (this.sup == that.sup)
        case _ =>
            false
    }

}