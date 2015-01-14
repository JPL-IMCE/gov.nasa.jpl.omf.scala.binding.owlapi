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

import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLOntology
import scala.util.Try
import scala.util.Success
import gov.nasa.jpl.omf.scala.core.ConstrainingFacet
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import org.semanticweb.owlapi.model.OWLOntologyManager
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper
import scala.collection.JavaConverters._
import scala.util.Failure
import org.semanticweb.owlapi.model.AddImport

case class OWLAPIOMFStore( val omfModule: OWLAPIOMFModule, val ontManager: OWLOntologyManager ) {

  omfModule.catalogManager match {
    case None => ()
    case Some( catalogManager ) =>
      val mapper: Iterable[OWLOntologyIRIMapper] = Iterable( new CatalogIRIMapper( catalogManager ) )
      ontManager.getIRIMappers.add( mapper.asJava )
  }

  val tboxGraphs = scala.collection.mutable.HashMap[IRI, types.ModelTerminologyGraph]()

  def loadTerminologyGraph( iri: IRI ): Try[types.ModelTerminologyGraph] = ???
  
  def makeTerminologyGraph(
    iri: IRI,
    extendedTGraphs: Iterable[types.ModelTerminologyGraph] ): Try[types.ModelTerminologyGraph] =
    if ( ontManager.contains( iri ) ) 
      Failure( new IllegalArgumentException( s"An ontology with iri='${iri}' already exists" ) )
    else {
      val foreignTGraphs = extendedTGraphs filter { tg => ! tboxGraphs.contains( tg.iri ) }
      if (foreignTGraphs.nonEmpty) 
        Failure( new IllegalArgumentException( 
          s"Cannot create an ontology with iri='${iri}' extending ${foreignTGraphs.size} foreign terminology graphs (i.e., not managed by this ontology mananger)"
          ) )
      val o = ontManager.createOntology( iri )
      
      extendedTGraphs.foreach { tg =>       
        val decl = ontManager.getOWLDataFactory.getOWLImportsDeclaration(tg.iri)
        ontManager.applyChange(new AddImport(o, decl))
      }
      
      val g = new types.ModelTerminologyGraph( o )
      tboxGraphs.put( iri, g )
      Success( g )
    }
  
  def addEntityAspect(graph: types.ModelTerminologyGraph, facetName: String): Try[types.ModelEntityAspect] = ??? 
      
  def addEntityConcept(
    graph: types.ModelTerminologyGraph,
    conceptName: String, 
    isAbstract: Boolean = false ): Try[types.ModelEntityConcept] = ???

  def addEntityRelationship(
    graph: types.ModelTerminologyGraph,
    source: types.ModelEntityDefinition,
    target: types.ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics],
    reifiedRelationshipName: String,
    unreifiedRelationshipName: String,
    unreifiedInverseRelationshipName: Option[String],
    isAbstract: Boolean = false ): Try[types.ModelEntityRelationship] = ???

  def addScalarDataType(
    graph: types.ModelTerminologyGraph,
    fragment: String ): Try[types.ModelScalarDataType] = ???

  def addStructuredDataType(
    graph: types.ModelTerminologyGraph,
    fragment: String ): Try[types.ModelStructuredDataType] = ???

  def addStructuredDataRelationship(
    graph: types.ModelTerminologyGraph,
    source: types.ModelStructuredDataType,
    target: types.ModelDataTypeDefinition,
    fragment: String ): Try[types.ModelStructuredDataRelationship] = ???

  def addEntityDataRelationship(
    graph: types.ModelTerminologyGraph,
    source: types.ModelEntityDefinition,
    target: types.ModelDataTypeDefinition,
    fragment: String ): Try[types.ModelEntityDataRelationship] = ???

  def addEntityDefinitionAspectSubClassAxiom(
      graph: types.ModelTerminologyGraph,
      sub: types.ModelEntityDefinition,
      sup: types.ModelEntityAspect): Try[types.EntityDefinitionAspectSubClassAxiom] = ???
      
  def addEntityConceptSubClassAxiom(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelEntityConcept,
    sup: types.ModelEntityConcept ): Try[types.EntityConceptSubClassAxiom] = ???

  def addEntityConceptRestrictionAxiom(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelEntityConcept,
    rel: types.ModelEntityRelationship,
    range: types.ModelEntityDefinition ): Try[types.EntityConceptRestrictionAxiom] = ???

  def addEntityRelationshipSubClassAxiom(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelEntityRelationship,
    sup: types.ModelEntityRelationship ): Try[types.EntityRelationshipSubClassAxiom] = ???

  def addScalarDataTypeFacetRestriction(
    graph: types.ModelTerminologyGraph,
    sub: types.ModelScalarDataType,
    sup: types.ModelScalarDataType,
    restrictions: Iterable[ConstrainingFacet] ): Try[types.ScalarDataTypeFacetRestriction] = ???

  def loadInstanceGraph(iri: IRI): Try[instances.ModelInstanceGraph] = ???

  def makeInstanceGraph(
    iri: IRI,
    instantiatedTGraphs: Iterable[types.ModelTerminologyGraph],
    extendedIGraphs: Iterable[instances.ModelInstanceGraph] ): Try[instances.ModelInstanceGraph] = ???

  def addInstanceObject(
    graph: instances.ModelInstanceGraph,
    conceptType: types.ModelEntityConcept,
    fragment: String ): Try[instances.ModelInstanceObject] = ???

  def addInstanceRelation(
    graph: instances.ModelInstanceGraph,
    relationshipType: types.ModelEntityRelationship,
    source: instances.ModelEntityInstance,
    target: instances.ModelEntityInstance,
    fragment: String ): Try[instances.ModelInstanceRelation] = ???

  def addDataLiteral(
    graph: instances.ModelInstanceGraph,
    datatype: types.ModelScalarDataType,
    lexicalForm: String ): Try[instances.ModelInstanceDataLiteral] = ???

  def addDataStructure(
    graph: instances.ModelInstanceGraph,
    datatype: types.ModelStructuredDataType,
    fragment: String ): Try[instances.ModelInstanceDataStructure] = ???

  def addStructuredDataProperty(
    graph: instances.ModelInstanceGraph,
    ds: instances.ModelInstanceDataStructure,
    structuredDataRelationshipType: types.ModelStructuredDataRelationship,
    value: instances.ModelDataInstance ): Try[instances.ModelStructuredDataProperty] = ???

  def addEntityDataProperty(
    graph: instances.ModelInstanceGraph,
    e: instances.ModelEntityInstance,
    entityDataRelationshipType: types.ModelEntityDataRelationship,
    value: instances.ModelDataInstance ): Try[instances.ModelEntityDataProperty] = ???
}