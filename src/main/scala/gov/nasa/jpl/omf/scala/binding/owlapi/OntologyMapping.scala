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
import org.semanticweb.owlapi.model.OWLOntology
import gov.nasa.jpl.omf.scala.binding.owlapi.common.{ImmutableModule, MutableModule}
import gov.nasa.jpl.omf.scala.core.builtin.BuiltInDatatypeMaps.DataRangeCategories
import gov.nasa.jpl.omf.scala.core.{Mutable2ImmutableModuleTable, OMFError, OMFOps}

import scala.collection.immutable._
import scala.{Int, None, Some, StringContext}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

case class OntologyMapping
( override val drc: DataRangeCategories[OWLAPIOMF],

  private val ont2m: Map[OWLOntology, MutableModule] = Map.empty,
  private val ont2i: Map[OWLOntology, ImmutableModule] = Map.empty,

  override protected val pairs
  : Seq[(MutableModule, ImmutableModule)]
  = Seq.empty[(MutableModule, ImmutableModule)],

  override protected val ms
  : Map[api.taggedTypes.ModuleUUID, MutableModule]
  = Map.empty[api.taggedTypes.ModuleUUID, MutableModule],

  override protected val is
  : Map[api.taggedTypes.ModuleUUID, ImmutableModule]
  = Map.empty[api.taggedTypes.ModuleUUID, ImmutableModule])
  extends Mutable2ImmutableModuleTable[OWLAPIOMF] {

  def addMutableModule
  (m: MutableModule)
  (implicit ops: OWLAPIOMFOps)
  : OMFError.Throwables \/ OntologyMapping
  = if (!ont2m.contains(m.ont) && !ont2i.contains(m.ont) && !containsKey(m))
    copy(
      ont2m = ont2m + (m.ont -> m),
      ms = ms + (m.uuid -> m)
    ).right
  else
    Set[java.lang.Throwable](OMFError.omfError(
      s"Cannot add ontology/immutable entry for ${m.iri}"
    )).left

  override def size()
  : Int
  = scala.math.max(super.size(), ont2m.size)

  override def `:+`
  (pair: (MutableModule, ImmutableModule))
  (implicit ops: OMFOps[OWLAPIOMF])
  : OMFError.Throwables \/ OntologyMapping
  = pairs.find(_._1 == pair._1) match {
    case Some((_, im)) =>
      if (im == pair._2)
        this.right
      else
        Set[java.lang.Throwable](
          OMFError.omfError(
            s"Mutable2ImmutableModuleTable: key conflict: ${pair._1} is already mapped to a different value"
          )).left
    case None =>
      val uuid = ops.getModuleUUID(pair._1)
      val (m, i) = (pair._1, pair._2)
      val (mo, io) = (m.ont, i.ont)

      val c1 = mo == io
      val c3 = !ont2i.contains(io)
      val c5 = !containsValue(i)

      if (c1 && c3 && c5) {

        val c2 = !ont2m.contains(mo)
        val c4 = !containsKey(m)

        val omm = if (c2 && c4)
          copy(
            ont2m = ont2m + (mo -> m),
            ms = ms + (m.uuid -> m))
        else
          this

        val omi = omm.copy(
          ont2i = ont2i + (mo -> i),
          is = is + (i.uuid -> i),
          pairs = pairs :+ (m -> i))

        omi.right
      } else
        Set[java.lang.Throwable](OMFError.omfError(
          s"Cannot add ontology/immutable entry for ${m.iri}"
        )).left
  }

}

object OntologyMapping {

  def initialize(drc: DataRangeCategories[OWLAPIOMF])
  : OntologyMapping
  = OntologyMapping(drc)

}