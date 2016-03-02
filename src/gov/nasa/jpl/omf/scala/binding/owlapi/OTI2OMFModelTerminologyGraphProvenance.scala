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