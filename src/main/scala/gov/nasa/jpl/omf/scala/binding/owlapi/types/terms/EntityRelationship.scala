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

import gov.nasa.jpl.omf.scala.binding.owlapi.types.Term
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics.RelationshipCharacteristics

import scala.collection.immutable.Iterable
import scala.{Any,Boolean}

trait EntityRelationship extends Term {

  val source: Entity
  val target: Entity
  val characteristics: Iterable[RelationshipCharacteristics]

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: EntityRelationship => true
    case _ => false
  }
}
