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

import org.semanticweb.owlapi.model.OWLOntology

import scala.Option
import scala.Predef.String
import scalaz._, Scalaz._

/**
  * OIT/OMF Mapping Provenance Information for an OMF ModelTerminologyGraph
  *
  * @todo refactor the mapping provenance concern to avoid coupling it with the OMF/OWL binding concern
  *
  *       The OTI/OMF Mapping Provenance is special case of provenance metadata.
  *       That is, there are two independent concerns that are orthogonal to OMF itself:
  *       - an OMF binding concern (like the OWLAPI binding here)
  *       - an OMF mapping concern (like the OTI=>OMF mapping for the exporter)
  */
case class OTI2OMFModelTerminologyGraphProvenance
(provenanceKind: OMFModelTerminologyGraphProvenanceKind,
 provenanceURI: String)

sealed abstract class OMFModelTerminologyGraphProvenanceKind(val literal: String)

/**
  * Corresponds to built-in W3C vocabularies
  * (e.g., XML Schema 1.1 Datatypes, RDFS, OWL Datatypes)
  */
case object OMFModelTerminologyGraphW3CProvenanceKind
  extends OMFModelTerminologyGraphProvenanceKind(literal="W3C")

/**
  * Corresponds to exporting either
  * - OTISerializableMetamodelArtifactKind
  * - OTIBuiltInMetamodelArtifactKind
  * (there is no existing OMF Graph Ontology corresponding to such a Metamodel)
  */
case object OMFModelTerminologyGraphOTIMetamodelProvenanceKind
extends OMFModelTerminologyGraphProvenanceKind(literal="OTIMetamodel")

/**
  * Corresponds to exporting either
  * - OTISerializableProfileArtifactKind
  * - OTIBuiltInProfileArtifactKind
  * (there is no existing OMF Graph Ontology corresponding to such a Profile)
  */
case object OMFModelTerminologyGraphOTIProfileProvenanceKind
  extends OMFModelTerminologyGraphProvenanceKind(literal="OTIProfile")

/**
  * Corresponds to exporting either
  * - OTISerializableModelLibraryArtifactKind
  * - OTIBuiltInModelLibraryArtifactKind
  * (there is no existing OMF Graph Ontology corresponding to such a ModelLibrary)
  */
case object OMFModelTerminologyGraphOTIModelLibraryProvenanceKind
  extends OMFModelTerminologyGraphProvenanceKind(literal="OTIModelLibrary")

/**
  * Corresponds to exporting a UML Package/Profile
  * that has a corresponding OMF Graph Ontology
  * (in this case, the export should be redundant)
  */
case object OMFModelTerminologyGraphOMFGraphOntologyProvenanceKind
  extends OMFModelTerminologyGraphProvenanceKind(literal="OMFGraphOntology")

/**
  * Corresponds to an instance of the OMF Metadata ontology
  * created as part of exporting an OTI artifact of some kind (Metamodel, Profile, ModelLibrary)
  */
case object OMFModelTerminologyGraphOMFMetadataOntologyProvenanceKind
  extends OMFModelTerminologyGraphProvenanceKind(literal="OMFMetadataOntology")

object OTI2OMFModelTerminologyGraphProvenance {

  def asOMFGraphOntologyProvenance
  (o: OWLOntology)
  : Option[OTI2OMFModelTerminologyGraphProvenance] = {
    val oIRI = o.getOntologyID.getOntologyIRI
    if (oIRI.isPresent)
      OTI2OMFModelTerminologyGraphProvenance(
        provenanceKind = OMFModelTerminologyGraphOMFGraphOntologyProvenanceKind,
        provenanceURI = oIRI.get.toURI.toString).some
    else
      Option.empty[OTI2OMFModelTerminologyGraphProvenance]
  }

}