/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2015, California Institute of Technology ("Caltech").
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
import gov.nasa.jpl.omf.scala.binding._
import org.semanticweb.owlapi.model.OWLOntologyManager
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

trait OWLAPIOMF
  extends OMF
  with OWLAPIOMFstore
  with OWLAPIOMFiri
  with OWLAPIOMFtbox
  with OWLAPIOMFabox {

}

trait OWLAPIOMFstore extends OMFstore {
  
  type Store = OWLAPIOMFGraphStore
  
}

trait OWLAPIOMFiri extends OMFiri {
  
  type IRI = org.semanticweb.owlapi.model.IRI
  
}

trait OWLAPIOMFtbox extends OMFtbox {
  
  type ModelTerminologyGraph =
  types.ModelTerminologyGraph

  type ImmutableModelTerminologyGraph =
  types.ImmutableModelTerminologyGraph

  type MutableModelTerminologyGraph =
  types.MutableModelTerminologyGraph

  override type Mutable2IMutableTerminologyMap =
  types.Mutable2IMutableTerminologyMap

  type ModelTypeTerm =
  types.ModelTypeTerm
    
  type ModelEntityDefinition =
  types.ModelEntityDefinition

  type ModelEntityAspect =
  types.ModelEntityAspect

  type ModelEntityConcept =
  types.ModelEntityConcept

  type ModelEntityReifiedRelationship =
  types.ModelEntityReifiedRelationship

  type ModelEntityUnreifiedRelationship =
  types.ModelEntityUnreifiedRelationship
  
  type ModelDataTypeDefinition =
  types.ModelDataTypeDefinition

  type ModelScalarDataType =
  types.ModelScalarDataType

  type ModelStructuredDataType =
  types.ModelStructuredDataType
  
  type ModelDataRelationship =
  types.ModelDataRelationship
  
  type ModelDataRelationshipFrom =
  types.ModelDataRelationshipFrom

  type ModelDataRelationshipFromEntity =
  types.ModelDataRelationshipFromEntity

  type ModelDataRelationshipFromStructure =
  types.ModelDataRelationshipFromStructure
  
  type ModelDataRelationshipTo =
  types.ModelDataRelationshipTo

  type ModelDataRelationshipToScalar =
  types.ModelDataRelationshipToScalar

  type ModelDataRelationshipToStructure =
  types.ModelDataRelationshipToStructure
  
  type ModelDataRelationshipFromEntityToScalar =
  types.ModelDataRelationshipFromEntityToScalar

  type ModelDataRelationshipFromEntityToStructure =
  types.ModelDataRelationshipFromEntityToStructure

  type ModelDataRelationshipFromStructureToScalar =
  types.ModelDataRelationshipFromStructureToScalar

  type ModelDataRelationshipFromStructureToStructure =
  types.ModelDataRelationshipFromStructureToStructure
  
  type ModelTermAxiom =
  types.ModelTermAxiom
   
  type EntityDefinitionAspectSubClassAxiom =
  types.EntityDefinitionAspectSubClassAxiom

  type EntityConceptDesignationTerminologyGraphAxiom =
  types.EntityConceptDesignationTerminologyGraphAxiom

  type EntityConceptSubClassAxiom =
  types.EntityConceptSubClassAxiom

  type EntityConceptRestrictionAxiom =
  types.EntityConceptRestrictionAxiom

  type EntityConceptUniversalRestrictionAxiom =
  types.EntityConceptUniversalRestrictionAxiom

  type EntityConceptExistentialRestrictionAxiom =
  types.EntityConceptExistentialRestrictionAxiom

  type EntityReifiedRelationshipSubClassAxiom =
  types.EntityReifiedRelationshipSubClassAxiom

  type ScalarDataTypeFacetRestriction =
  types.ScalarDataTypeFacetRestriction

  type TerminologyGraphAxiom =
  types.TerminologyGraphAxiom

  type TerminologyGraphDirectExtensionAxiom =
  types.TerminologyGraphDirectExtensionAxiom

  type TerminologyGraphDirectNestingAxiom =
  types.TerminologyGraphDirectNestingAxiom

}

trait OWLAPIOMFabox extends OMFabox {
  
  type ModelInstanceGraph =
  instances.ModelInstanceGraph

  type ImmutableModelInstanceGraph =
  instances.ImmutableModelInstanceGraph

  type MutableModelInstanceGraph =
  instances.MutableModelInstanceGraph
  
  type ModelInstanceAssertion =
  instances.ModelInstanceAssertion
  
  type ModelNamedIndividual =
  instances.ModelNamedIndividual
  
  type ModelEntityInstance =
  instances.ModelEntityInstance

  type ModelInstanceObject =
  instances.ModelInstanceObject

  type ModelInstanceRelation =
  instances.ModelInstanceRelation
  
  type ModelDataInstance =
  instances.ModelDataInstance

  type ModelInstanceDataLiteral =
  instances.ModelInstanceDataLiteral

  type ModelInstanceDataStructure =
  instances.ModelInstanceDataStructure
  
  type ModelInstanceDataRelationshipFromEntityToScalar =
  instances.ModelInstanceDataRelationshipFromEntityToScalar

  type ModelInstanceDataRelationshipFromEntityToStructure =
  instances.ModelInstanceDataRelationshipFromEntityToStructure

  type ModelInstanceDataRelationshipFromStructureToScalar =
  instances.ModelInstanceDataRelationshipFromStructureToScalar

  type ModelInstanceDataRelationshipFromStructureToStructure =
  instances.ModelInstanceDataRelationshipFromStructureToStructure
    
}


case class OWLAPITerminologyGraphSignature
( override val iri: OWLAPIOMF#IRI,
  override val kind: TerminologyKind,
  override val nesting: Option[OWLAPIOMF#ModelTerminologyGraph],
  override val nested: Iterable[OWLAPIOMF#ModelTerminologyGraph],
  override val imports: Iterable[OWLAPIOMF#ModelTerminologyGraph],
  override val aspects: Iterable[OWLAPIOMF#ModelEntityAspect],
  override val concepts: Iterable[OWLAPIOMF#ModelEntityConcept],
  override val reifiedRelationships: Iterable[OWLAPIOMF#ModelEntityReifiedRelationship],
  override val unreifiedRelationships: Iterable[OWLAPIOMF#ModelEntityUnreifiedRelationship],
  override val scalarDataTypes: Iterable[OWLAPIOMF#ModelScalarDataType],
  override val structuredDataTypes: Iterable[OWLAPIOMF#ModelStructuredDataType],
  override val entity2scalarDataRelationships: Iterable[OWLAPIOMF#ModelDataRelationshipFromEntityToScalar],
  override val entity2structureDataRelationships: Iterable[OWLAPIOMF#ModelDataRelationshipFromEntityToStructure],
  override val structure2scalarDataRelationships: Iterable[OWLAPIOMF#ModelDataRelationshipFromStructureToScalar],
  override val structure2structureDataRelationships: Iterable[OWLAPIOMF#ModelDataRelationshipFromStructureToStructure],
  override val axioms: Iterable[OWLAPIOMF#ModelTermAxiom] )
  extends TerminologyGraphSignature[OWLAPIOMF]
{
  require(null != iri)
  require(null != kind)
  require(null != nesting)
  require(null != nested)
  require(null != imports)
  require(null != aspects)
  require(null != concepts)
  require(null != reifiedRelationships)
  require(null != unreifiedRelationships)
  require(null != scalarDataTypes)
  require(null != structuredDataTypes)
  require(null != entity2scalarDataRelationships)
  require(null != entity2structureDataRelationships)
  require(null != structure2scalarDataRelationships)
  require(null != structure2structureDataRelationships)
  require(null != axioms)
}

case class OWLAPIEntityConceptSignature
( override val iri: OWLAPIOMF#IRI,
  override val isAbstract: Boolean)
  extends EntityConceptSignature[OWLAPIOMF]
{
  require(null != iri)
}

case class OWLAPIEntityReifiedRelationshipSignature
( override val iri: OWLAPIOMF#IRI,
  override val source: OWLAPIOMF#ModelEntityDefinition,
  override val target: OWLAPIOMF#ModelEntityDefinition,
  override val characteristics: Iterable[RelationshipCharacteristics],
  override val isAbstract: Boolean )
  extends EntityReifiedRelationshipSignature[OWLAPIOMF]
{
  require(null != iri)
  require(null != source)
  require(null != target)
  require(null != characteristics)
}

case class OWLAPIEntityUnreifiedRelationshipSignature
( override val iri: OWLAPIOMF#IRI,
  override val source: OWLAPIOMF#ModelEntityDefinition,
  override val target: OWLAPIOMF#ModelEntityDefinition,
  override val characteristics: Iterable[RelationshipCharacteristics] )
  extends EntityUnreifiedRelationshipSignature[OWLAPIOMF]
{
  require(null != iri)
  require(null != source)
  require(null != target)
  require(null != characteristics)
}