/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.binding.owlapi

import gov.nasa.jpl.omf.scala.core._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{IRI,OWLDataFactory}
import org.apache.xml.resolver.CatalogManager

import scala.collection.immutable.Set
import scala.Predef.require
import scalaz._

case class OWLAPIOMFModule
( catalogManager: CatalogManager,
  ops: OWLAPIOMFOps,
  omfOntologyIRI: IRI
) extends OMFModule
  with OMFOpsModule {

  require(null != catalogManager )

  type omf = OWLAPIOMF
  
  implicit val dataFactory: OWLDataFactory = OWLManager.getOWLDataFactory

}

object OWLAPIOMFModule {
  
  def owlAPIOMFModule(catalogManager: CatalogManager)
  : Set[java.lang.Throwable] \/ OWLAPIOMFModule =
  for {
    rdfs_label <-
    OWLAPIIRIOps.makeIRI("http://www.w3.org/2000/01/rdf-schema#label")

    OMF_TBox_DataProperty_HasShortName <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/omf/omfMetadata#hasShortName")

    OMF_TBox_DataProperty_HasUUID <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/omf/omfMetadata#hasUUID")

    AnnotationHasUUID <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hasUUID")

    AnnotationHasRelativeIRI <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hasRelativeIRI")

    AnnotationHasIRIHashPrefix <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hashIRIHashPrefix")

    AnnotationHasIRIHashSuffix <-
      OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hasIRIHashSuffix")

    AnnotationIsAbstract <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#isAbstract")

    AnnotationIsDerived <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#isDerived")

    AnnotationIsDefinition <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#isDefinition")

    AnnotationIsDesignation <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#isDesignation")

    AnnotationIsToplevel <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#isToplevel")

    AnnotationHasContext <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hasContext")

    AnnotationHasGraph <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hasGraph")

    omfOntologyIRI <-
    OWLAPIIRIOps.makeIRI( "http://imce.jpl.nasa.gov/foundation/omf/omf.owl" )

    ops = new OWLAPIOMFOps(
      rdfs_label,
      OMF_TBox_DataProperty_HasShortName,
      OMF_TBox_DataProperty_HasUUID,
      AnnotationHasUUID,
      AnnotationHasRelativeIRI,
      AnnotationHasIRIHashPrefix,
      AnnotationHasIRIHashSuffix,
      AnnotationIsAbstract,
      AnnotationIsDerived,
      AnnotationIsDefinition,
      AnnotationIsDesignation,
      AnnotationIsToplevel,
      AnnotationHasContext,
      AnnotationHasGraph)

  } yield
    OWLAPIOMFModule(catalogManager, ops, omfOntologyIRI)

}