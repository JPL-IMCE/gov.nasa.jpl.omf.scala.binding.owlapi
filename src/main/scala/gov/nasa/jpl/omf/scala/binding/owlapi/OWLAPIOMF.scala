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

import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._

import scala.collection.immutable._
import scala.{Boolean, Option}
import scala.Predef.require

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

  override type Mutable2ImmutableTerminologyMap =
  types.Mutable2ImmutableTerminologyMap

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

  type ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral =
  types.ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral

  type EntityDefinitionAspectSubClassAxiom =
  types.EntityDefinitionAspectSubClassAxiom

  type EntityConceptDesignationTerminologyGraphAxiom =
  types.EntityConceptDesignationTerminologyGraphAxiom

  type EntityConceptSubClassAxiom =
  types.EntityConceptSubClassAxiom

  type EntityDefinitionRestrictionAxiom =
  types.EntityDefinitionRestrictionAxiom

  type EntityDefinitionUniversalRestrictionAxiom =
  types.EntityDefinitionUniversalRestrictionAxiom

  type EntityDefinitionExistentialRestrictionAxiom =
  types.EntityDefinitionExistentialRestrictionAxiom

  type EntityReifiedRelationshipSubClassAxiom =
  types.EntityReifiedRelationshipSubClassAxiom

  type ScalarDataTypeFacetRestrictionAxiom =
  types.ScalarDataTypeFacetRestrictionAxiom

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
( override val uuid: UUID,
  override val name: LocalName,
  override val iri: OWLAPIOMF#IRI,
  override val kind: TerminologyKind,
  override val imports: Iterable[OWLAPIOMF#ModelTerminologyGraph],
  override val nesting: Option[OWLAPIOMF#ModelEntityConcept],
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
  override val axioms: Iterable[OWLAPIOMF#ModelTermAxiom],
  override val gaxioms: Iterable[OWLAPIOMF#TerminologyGraphAxiom])
  extends TerminologyGraphSignature[OWLAPIOMF]
{
  require(null != iri)
  require(null != kind)
  require(null != imports)
  require(null != nesting)
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
( override val uuid: UUID,
  override val name: LocalName,
  override val iri: OWLAPIOMF#IRI,
  override val isAbstract: Boolean)
  extends EntityConceptSignature[OWLAPIOMF]
{
  require(null != iri)
}

case class OWLAPIEntityReifiedRelationshipSignature
( override val uuid: UUID,
  override val name: LocalName,
  override val iri: OWLAPIOMF#IRI,
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
( override val uuid: UUID,
  override val name: LocalName,
  override val iri: OWLAPIOMF#IRI,
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