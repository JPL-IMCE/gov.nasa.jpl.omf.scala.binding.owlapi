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
import gov.nasa.jpl.omf.scala.binding.owlapi.common.LogicalElement

import scala.{Any,Boolean,Int,Option,None,Some}

case class RuleBodySegment
(override val uuid: api.taggedTypes.RuleBodySegmentUUID,
 chainRule: Option[ChainRule],
 previousSegment: Option[RuleBodySegment])
  extends LogicalElement {

  val position: Int = previousSegment match {
    case None => 1
    case Some(seg) => 1 + seg.position
  }

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: RuleBodySegment => true
    case _ => false
  }

  override val hashCode: Int = (uuid, chainRule, previousSegment).##

  override def equals(other: Any): Boolean = other match {
    case that: RuleBodySegment =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.chainRule == that.chainRule) &&
        (this.previousSegment == that.previousSegment)
    case _ =>
      false
  }
}
