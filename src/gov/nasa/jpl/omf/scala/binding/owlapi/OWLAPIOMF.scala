/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.binding.owlapi

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.binding._
import org.semanticweb.owlapi.model.OWLOntologyManager

trait OWLAPIOMF extends OMF {

  type Store = OWLAPIOMFStore
  
  type IRI = org.semanticweb.owlapi.model.IRI
  
  type ModelTerminologyGraph = types.ModelTerminologyGraph
  
  type ModelTypeTerm = types.ModelTypeTerm
    
  type ModelEntityDefinition = types.ModelEntityDefinition
  type ModelEntityAspect = types.ModelEntityAspect
  type ModelEntityConcept = types.ModelEntityConcept
  type ModelEntityRelationship = types.ModelEntityRelationship
  
  type ModelDataTypeDefinition = types.ModelDataTypeDefinition
  type ModelScalarDataType = types.ModelScalarDataType
  type ModelStructuredDataType = types.ModelStructuredDataType
  
  type ModelDataRelationship = types.ModelDataRelationship
  type ModelDataRelationshipFromEntityToScalar = types.ModelDataRelationshipFromEntityToScalar
  type ModelDataRelationshipFromEntityToStructure = types.ModelDataRelationshipFromEntityToStructure
  type ModelDataRelationshipFromStructureToScalar = types.ModelDataRelationshipFromStructureToScalar
  type ModelDataRelationshipFromStructureToStructure = types.ModelDataRelationshipFromStructureToStructure
  
  type ModelTermAxiom = types.ModelTermAxiom
   
  type EntityDefinitionAspectSubClassAxiom = types.EntityDefinitionAspectSubClassAxiom
 
  type EntityConceptSubClassAxiom = types.EntityConceptSubClassAxiom
  type EntityConceptRestrictionAxiom = types.EntityConceptRestrictionAxiom
  type EntityRelationshipSubClassAxiom = types.EntityRelationshipSubClassAxiom
  
  type ScalarDataTypeFacetRestriction = types.ScalarDataTypeFacetRestriction
  
  type ModelInstanceGraph = instances.ModelInstanceGraph
  
  type ModelInstanceAssertion = instances.ModelInstanceAssertion
  
  type ModelNamedIndividual = instances.ModelNamedIndividual
  
  type ModelEntityInstance = instances.ModelEntityInstance
  type ModelInstanceObject = instances.ModelInstanceObject
  type ModelInstanceRelation = instances.ModelInstanceRelation
  
  type ModelDataInstance = instances.ModelDataInstance
  type ModelInstanceDataLiteral = instances.ModelInstanceDataLiteral
  type ModelInstanceDataStructure = instances.ModelInstanceDataStructure
  
  type ModelInstanceDataRelationshipFromEntityToScalar = instances.ModelInstanceDataRelationshipFromEntityToScalar
  type ModelInstanceDataRelationshipFromEntityToStructure = instances.ModelInstanceDataRelationshipFromEntityToStructure
  type ModelInstanceDataRelationshipFromStructureToScalar = instances.ModelInstanceDataRelationshipFromStructureToScalar
  type ModelInstanceDataRelationshipFromStructureToStructure = instances.ModelInstanceDataRelationshipFromStructureToStructure
    
}
