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

package gov.nasa.jpl.omf.scala.binding.owlapi

import java.util.UUID

import gov.nasa.jpl.omf.scala.core.EntityScalarDataPropertyParticularRestrictionSignature

import scala.Predef.{require,String}

case class OWLAPIEntityScalarDataPropertyParticularRestrictionSignature
( override val uuid: UUID,
  override val restrictedEntity: OWLAPIOMF#Entity,
  override val scalarDataProperty: OWLAPIOMF#EntityScalarDataProperty,
  override val literalValue: String)
  extends EntityScalarDataPropertyParticularRestrictionSignature[OWLAPIOMF]
{
  require(null != uuid)
  require(null != restrictedEntity)
  require(null != scalarDataProperty)
  require(null != literalValue)
}
