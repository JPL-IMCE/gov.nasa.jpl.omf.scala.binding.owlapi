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

package gov.nasa.jpl.omf.scala.binding.owlapi.descriptions

import java.util.UUID

import gov.nasa.jpl.omf.scala.binding.owlapi.common.Element
import gov.nasa.jpl.omf.scala.binding.owlapi.types.terms.{DataRange,ScalarDataProperty}
import gov.nasa.jpl.imce.oml.tables.LiteralValue

import scala.Option

case class ScalarDataPropertyValue
( override val uuid: UUID,
  context: SingletonInstanceStructuredDataPropertyContext,
  dataRelationship: ScalarDataProperty,
  value: LiteralValue,
  valueType: Option[DataRange])
  extends Element