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

import scala.Predef.require
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLObjectProperty

case class ModelDataRelationshipFromStructureToStructure(
  val c: OWLClass,
  val source: ModelStructuredDataType,
  val rSource: OWLObjectProperty,
  val target: ModelStructuredDataType,
  val rTarget: OWLObjectProperty,
  val unreified: OWLObjectProperty )
  extends ModelDataRelationship
  with ModelDataRelationshipFromStructure
  with ModelDataRelationshipToStructure {

  require(null != c)
  require(null != source)
  require(null != rSource)
  require(null != target)
  require(null != rTarget)
  require(null != unreified)

  override val iri = c.getIRI
}