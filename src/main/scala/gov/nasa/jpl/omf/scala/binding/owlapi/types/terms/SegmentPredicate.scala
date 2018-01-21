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
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{LogicalElement, Predicate}

import scala.{Any, Boolean, Int, Option, Some}
import scala.Predef.require

case class SegmentPredicate
( override val uuid: resolver.api.taggedTypes.SegmentPredicateUUID,
  bodySegment: RuleBodySegment,
  predicate: Option[Predicate],
  reifiedRelationshipSource: Option[ReifiedRelationship],
  reifiedRelationshipInverseSource: Option[ReifiedRelationship],
  reifiedRelationshipTarget: Option[ReifiedRelationship],
  reifiedRelationshipInverseTarget: Option[ReifiedRelationship],
  unreifiedRelationshipInverse: Option[UnreifiedRelationship])
  extends LogicalElement {

  require(null != bodySegment)
  require(null != predicate)
  require(null != reifiedRelationshipSource)
  require(null != reifiedRelationshipInverseSource)
  require(null != reifiedRelationshipTarget)
  require(null != reifiedRelationshipInverseTarget)
  require(null != unreifiedRelationshipInverse)

  val isPredicate: Boolean
  = predicate.nonEmpty &&
    reifiedRelationshipSource.isEmpty &&
    reifiedRelationshipInverseSource.isEmpty &&
    reifiedRelationshipTarget.isEmpty &&
    reifiedRelationshipInverseTarget.isEmpty &&
    unreifiedRelationshipInverse.isEmpty

  val isReifiedRelationshipSource: Boolean
  = predicate.isEmpty &&
    reifiedRelationshipSource.nonEmpty &&
    reifiedRelationshipInverseSource.isEmpty &&
    reifiedRelationshipTarget.isEmpty &&
    reifiedRelationshipInverseTarget.isEmpty &&
    unreifiedRelationshipInverse.isEmpty

  val isReifiedRelationshipInverseSource: Boolean
  = predicate.isEmpty &&
    reifiedRelationshipSource.isEmpty &&
    reifiedRelationshipInverseSource.nonEmpty &&
    reifiedRelationshipTarget.isEmpty &&
    reifiedRelationshipInverseTarget.isEmpty &&
    unreifiedRelationshipInverse.isEmpty

  val isReifiedRelationshipTarget: Boolean
  = predicate.isEmpty &&
    reifiedRelationshipSource.isEmpty &&
    reifiedRelationshipInverseSource.isEmpty &&
    reifiedRelationshipTarget.nonEmpty &&
    reifiedRelationshipInverseTarget.isEmpty &&
    unreifiedRelationshipInverse.isEmpty

  val isReifiedRelationshipInverseTarget: Boolean
  = predicate.isEmpty &&
    reifiedRelationshipSource.isEmpty &&
    reifiedRelationshipInverseSource.isEmpty &&
    reifiedRelationshipTarget.isEmpty &&
    reifiedRelationshipInverseTarget.nonEmpty &&
    unreifiedRelationshipInverse.isEmpty

  val isUnreifiedRelationshipInverse: Boolean
  = predicate.isEmpty &&
    reifiedRelationshipSource.isEmpty &&
    reifiedRelationshipInverseSource.isEmpty &&
    reifiedRelationshipTarget.isEmpty &&
    reifiedRelationshipInverseTarget.isEmpty &&
    unreifiedRelationshipInverse.nonEmpty

  require(
      ( isPredicate &&
        !isReifiedRelationshipSource &&
        !isReifiedRelationshipInverseSource &&
        !isReifiedRelationshipTarget &&
        !isReifiedRelationshipInverseTarget &&
        !isUnreifiedRelationshipInverse ) ||
      ( !isPredicate &&
        isReifiedRelationshipSource &&
        !isReifiedRelationshipInverseSource &&
        !isReifiedRelationshipTarget &&
        !isReifiedRelationshipInverseTarget &&
        !isUnreifiedRelationshipInverse ) ||
      ( !isPredicate &&
        !isReifiedRelationshipSource &&
        isReifiedRelationshipInverseSource &&
        !isReifiedRelationshipTarget &&
        !isReifiedRelationshipInverseTarget &&
        !isUnreifiedRelationshipInverse ) ||
      ( !isPredicate &&
        !isReifiedRelationshipSource &&
        !isReifiedRelationshipInverseSource &&
        isReifiedRelationshipTarget &&
        !isReifiedRelationshipInverseTarget &&
        !isUnreifiedRelationshipInverse ) ||
      ( !isPredicate &&
        !isReifiedRelationshipSource &&
        !isReifiedRelationshipInverseSource &&
        !isReifiedRelationshipTarget &&
        isReifiedRelationshipInverseTarget &&
        !isUnreifiedRelationshipInverse ) ||
      ( !isPredicate &&
        !isReifiedRelationshipSource &&
        !isReifiedRelationshipInverseSource &&
        !isReifiedRelationshipTarget &&
        !isReifiedRelationshipInverseTarget &&
        isUnreifiedRelationshipInverse )
  )

  // Unary = Entity (Aspect, Concept, ReifiedRelationship)
  val isUnary: Boolean
  = predicate match {
    case Some(_: Entity) =>
      true
    case _ =>
      false
  }

  val isBinaryForward: Boolean
  = isReifiedRelationshipSource ||
    isReifiedRelationshipTarget ||
    (predicate match {
      case Some(_: ForwardProperty) =>
        true
      case Some(_: UnreifiedRelationship) =>
        true
      case _ =>
        false
    })

  val isBinaryInverse: Boolean
  = isReifiedRelationshipInverseSource ||
    isReifiedRelationshipInverseTarget ||
    (predicate match {
      case Some(_: InverseProperty) =>
        true
      case _ =>
        false
    })

  val isBinary: Boolean
  = isBinaryForward || isBinaryInverse

  override val hashCode
  : Int
  = (uuid, bodySegment,
    predicate,
    reifiedRelationshipSource,
    reifiedRelationshipInverseSource,
    reifiedRelationshipTarget,
    reifiedRelationshipInverseTarget,
    unreifiedRelationshipInverse).##

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: SegmentPredicate => true
    case _ => false
  }

  override def equals(other: Any): Boolean = other match {
    case that: SegmentPredicate =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.bodySegment == that.bodySegment) &&
        (this.predicate == that.predicate) &&
        (this.reifiedRelationshipSource == that.reifiedRelationshipSource) &&
        (this.reifiedRelationshipInverseSource == that.reifiedRelationshipInverseSource) &&
        (this.reifiedRelationshipTarget == that.reifiedRelationshipInverseTarget) &&
        (this.reifiedRelationshipInverseTarget == that.reifiedRelationshipInverseTarget) &&
        (this.unreifiedRelationshipInverse == that.unreifiedRelationshipInverse)
    case _ =>
      false
  }
}
