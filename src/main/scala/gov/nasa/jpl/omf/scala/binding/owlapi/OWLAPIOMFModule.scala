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

    AnnotationHasUUID <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hasUUID")

    AnnotationHasID <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hasOTIToolSpecificID")

    AnnotationHasURL <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hasOTIToolSpecificURL")

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

    AnnotationHasRestrictedSourceProperty <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hasRestrictedSourceProperty")

    AnnotationHasRestroctedTargetProperty <-
    OWLAPIIRIOps.makeIRI("http://imce.jpl.nasa.gov/foundation/annotation/annotation#hasRestrictedTargetProperty")

    omfOntologyIRI <-
    OWLAPIIRIOps.makeIRI( "http://imce.jpl.nasa.gov/foundation/omf/omf.owl" )

    ops = new OWLAPIOMFOps(
      rdfs_label,
      AnnotationHasUUID,
      AnnotationHasID,
      AnnotationHasURL,
      AnnotationHasRelativeIRI,
      AnnotationHasIRIHashPrefix,
      AnnotationHasIRIHashSuffix,
      AnnotationIsAbstract,
      AnnotationIsDerived,
      AnnotationIsDefinition,
      AnnotationIsDesignation,
      AnnotationIsToplevel,
      AnnotationHasContext,
      AnnotationHasGraph,
      AnnotationHasRestrictedSourceProperty,
      AnnotationHasRestroctedTargetProperty)

  } yield
    OWLAPIOMFModule(catalogManager, ops, omfOntologyIRI)

}