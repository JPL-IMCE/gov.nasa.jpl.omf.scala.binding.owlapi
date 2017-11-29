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

import gov.nasa.jpl.imce.oml.resolver.api

import org.semanticweb.owlapi.model.{IRI, OWLOntology}
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{ImmutableModule, Module, MutableModule}
import gov.nasa.jpl.omf.scala.core.builtin.BuiltInDatatypeMaps.DataRangeCategories
import gov.nasa.jpl.omf.scala.core.{Mutable2ImmutableModuleTable, OMFError}

import scala.collection.immutable._
import scala.{Option,StringContext}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

case class OntologyMapping
( ont2m: Map[OWLOntology, MutableModule] = Map.empty,
  ont2i: Map[OWLOntology, ImmutableModule] = Map.empty,
  m2i: Mutable2ImmutableModuleMap = Mutable2ImmutableModuleTable.empty[OWLAPIOMF],
  drc: DataRangeCategories[OWLAPIOMF]) {

  def addLoadedOntologyModule
  (ont: OWLOntology, m: MutableModule)
  (implicit ops: OWLAPIOMFOps)
  : OMFError.Throwables \/ OntologyMapping
  = if (!ont2m.contains(ont) && !m2i.containsKey(m))
    copy(ont2m = ont2m + (ont -> m)).right
  else
    Set[java.lang.Throwable](OMFError.omfError(
      s"Cannot add ontology/mutable entry for ${m.iri} because there is already an entry!"
    )).left

  def addMappedModule
  (m: MutableModule, i: ImmutableModule)
  (implicit ops: OWLAPIOMFOps)
  : OMFError.Throwables \/ OntologyMapping
  = if (m.ont == i.ont && !ont2m.contains(m.ont) && !ont2i.contains(m.ont) && !m2i.containsKey(m) && !m2i.containsValue(i))
    for {
      updated <- m2i :+ (m -> i)
    } yield copy(ont2i = ont2i + (m.ont -> i), ont2m = ont2m + (m.ont -> m), m2i = updated)
    else
      Set[java.lang.Throwable](OMFError.omfError(
        s"Cannot add ontology/immutable entry for ${m.iri}"
      )).left

  def lookupImmutableModule
  (iri: IRI)
  (implicit ops: OWLAPIOMFOps)
  : Option[ImmutableModule]
  = m2i.lookupValue(iri)

  def lookupMutableModule
  (iri: IRI)
  (implicit ops: OWLAPIOMFOps)
  : Option[MutableModule]
  = m2i.lookupKey(iri)

  def lookupModule
  (iri: IRI)
  (implicit ops: OWLAPIOMFOps)
  : Option[Module]
  = lookupImmutableModule(iri) orElse lookupMutableModule(iri)

  def lookupImmutableModule
  (uuid: api.taggedTypes.ModuleUUID)
  : Option[ImmutableModule]
  = m2i.is.get(uuid)

  def lookupMutableModule
  (uuid: api.taggedTypes.ModuleUUID)
  : Option[MutableModule]
  = m2i.ms.get(uuid)

  def lookupModule
  (uuid: api.taggedTypes.ModuleUUID)
  : Option[Module]
  = lookupImmutableModule(uuid) orElse lookupMutableModule(uuid)

}

object OntologyMapping {

  def initialize(m2i: Mutable2ImmutableModuleMap, drc: DataRangeCategories[OWLAPIOMF])
  : OntologyMapping
  = OntologyMapping(
    ont2m = m2i.keys.map { m => m.ont -> m }.toMap,
    ont2i = m2i.values.map { i => i.ont -> i }.toMap,
    m2i,
    drc)

}