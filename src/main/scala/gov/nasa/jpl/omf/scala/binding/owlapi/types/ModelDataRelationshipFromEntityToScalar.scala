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

import scala.{Any,Boolean,Int}
import scala.Predef.require
import org.semanticweb.owlapi.model.OWLDataProperty

case class ModelDataRelationshipFromEntityToScalar
(dp: OWLDataProperty,
 override val name: LocalName,
 override val uuid: UUID,
 source: ModelEntityDefinition,
 target: ModelScalarDataType)
  extends ModelDataRelationship
    with ModelDataRelationshipFromEntity
    with ModelDataRelationshipToScalar {

  require(null != dp)
  require(null != source)
  require(null != target)

  override val iri = dp.getIRI

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ModelDataRelationshipFromEntityToScalar => true
    case _ => false
  }

  override val hashCode
  : Int
  = (uuid, name, source, target, dp).##

  override def equals(other: Any): Boolean = other match {
    case that: ModelDataRelationshipFromEntityToScalar =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.source == that.source) &&
        (this.target == that.target) &&
        (this.dp == that.dp)
    case _ =>
      false
  }

}