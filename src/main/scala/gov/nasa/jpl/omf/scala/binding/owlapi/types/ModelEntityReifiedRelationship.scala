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

import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLObjectProperty
import scala.collection.immutable._
import scala.{Boolean,Option}
import scala.Predef.require

case class ModelEntityReifiedRelationship(
  override val e: OWLClass,
  val unreified: OWLObjectProperty,
  val inverse: Option[OWLObjectProperty],
  val source: ModelEntityDefinition, val rSource: OWLObjectProperty,
  val target: ModelEntityDefinition, val rTarget: OWLObjectProperty,
  val characteristics: Iterable[RelationshipCharacteristics],
  val isAbstract: Boolean )
  extends ModelEntityDefinition( e ) {

  require(null != e)
  require(null != inverse)
  require(null != source)
  require(null != target)
  require(null != characteristics)

  override val iri = e.getIRI
}