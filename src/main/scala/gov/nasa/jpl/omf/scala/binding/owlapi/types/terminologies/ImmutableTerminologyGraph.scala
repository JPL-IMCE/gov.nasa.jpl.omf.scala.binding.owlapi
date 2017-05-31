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

package gov.nasa.jpl.omf.scala.binding.owlapi.types.terminologies


import gov.nasa.jpl.omf.scala.binding.owlapi._
import gov.nasa.jpl.omf.scala.core.ImmutableTerminologyBoxSignature
import gov.nasa.jpl.omf.scala.core.OMFError.Throwables
import org.semanticweb.owlapi.model._

import scala.{Any, Boolean, Int, Option}
import scala.Predef.{Map => _, Set => _, _}
import scalaz._

case class ImmutableTerminologyGraph
(override val sig: ImmutableTerminologyBoxSignature[OWLAPIOMF],
 override val ont: OWLOntology,
 override val extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance],
 override val backbone: OMFBackbone)
(override implicit val ops: OWLAPIOMFOps)
  extends TerminologyGraph
    with ImmutableTerminologyBox {

  require(null != ont)

  override type MS = ImmutableTerminologyBoxSignature[OWLAPIOMF]

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ImmutableTerminologyGraph => true
    case _ => false
  }

  override val hashCode: Int = (sig, ont, extraProvenanceMetadata).##

  override def equals(other: Any): Boolean = other match {
    case that: ImmutableTerminologyGraph =>
      (that canEqual this) &&
        (this.sig == that.sig) &&
        (this.ont == that.ont) &&
        (this.extraProvenanceMetadata == that.extraProvenanceMetadata)
    case _ =>
      false
  }

  override val mutabilityKind: String = "immutableGraph"
  override val isImmutable = true
  override val isMutable = false

  override val kindIRI: IRI = makeKindIRI(mutabilityKind)

}

object ImmutableTerminologyGraph {

  def initialize
  (sig: ImmutableTerminologyBoxSignature[OWLAPIOMF],
   ont: OWLOntology,
   extraProvenanceMetadata: Option[OTI2OMFModelTerminologyGraphProvenance],
   backbone: OMFBackbone)
  (implicit store: OWLAPIOMFGraphStore)
  : Throwables \/ ImmutableTerminologyGraph
  = {
    import scalaz._

    val ig = ImmutableTerminologyGraph(sig, ont, extraProvenanceMetadata, backbone)(store.ops)

    // we cannot set the name & uuid annotations because the OWLAPI ontology object may be immutable.
    //      for {
    //        _ <- mg.setTerminologyGraphLocalName(Some(name))
    //        _ <- mg.setTerminologyGraphUUID(uuid)
    //      } yield mg
    \/-(ig)
  }

}