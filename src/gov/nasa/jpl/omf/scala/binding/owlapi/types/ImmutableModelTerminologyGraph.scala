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
package gov.nasa.jpl.omf.scala.binding.owlapi.types

import gov.nasa.jpl.omf.scala.binding.owlapi._
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.parameters.Imports
import scala.collection.JavaConversions._
import scala.language.postfixOps
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import org.semanticweb.owlapi.model.AddAxiom
import org.semanticweb.owlapi.model.OWLDatatype
import org.semanticweb.owlapi.model.OWLClass
import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import org.semanticweb.owlapi.model.OWLObjectProperty
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import org.semanticweb.owlapi.model.OWLLiteral
import org.semanticweb.owlapi.model.OWLDataProperty

case class ImmutableModelTerminologyGraph(
  override val imports: Iterable[ModelTerminologyGraph],
  override protected val ont: OWLOntology,
  override protected val aspects: List[ModelEntityAspect],
  override protected val concepts: List[ModelEntityConcept],
  override protected val relationships: List[ModelEntityRelationship],
  override protected val sc: List[ModelScalarDataType],
  override protected val st: List[ModelStructuredDataType],
  override protected val e2sc: List[ModelDataRelationshipFromEntityToScalar],
  override protected val e2st: List[ModelDataRelationshipFromEntityToStructure],
  override protected val s2sc: List[ModelDataRelationshipFromStructureToScalar],
  override protected val s2st: List[ModelDataRelationshipFromStructureToStructure],
  override protected val ax: List[ModelTermAxiom] )( override implicit val ops: OWLAPIOMFOps )
  extends ModelTerminologyGraph( imports, ont )( ops ) {

  val getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition] =
    ( ( aspects map ( a => ( a.c -> a ) ) ) ++
      ( concepts map ( c => ( c.c -> c ) ) ) ++
      ( relationships map ( r => ( r.c -> r ) ) ) ) toMap

  override protected val iri2typeTerm = {
    def term2pair[T <: ModelTypeTerm]( t: T ) = ( t.iri -> t )

    ( aspects map ( term2pair( _ ) ) ) ++
      ( concepts map ( term2pair( _ ) ) ) ++
      ( relationships map ( term2pair( _ ) ) ) ++
      ( sc map ( term2pair( _ ) ) ) ++
      ( st map ( term2pair( _ ) ) ) ++
      ( e2sc map ( term2pair( _ ) ) ) ++
      ( e2st map ( term2pair( _ ) ) ) ++
      ( s2sc map ( term2pair( _ ) ) ) ++
      ( s2st map ( term2pair( _ ) ) ) toMap
  }

  def save( saveIRI: IRI ): Try[Unit] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

  def addEntityAspect( aspectIRI: IRI ): Try[types.ModelEntityAspect] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

  def addEntityConcept( conceptIRI: IRI, isAbstract: Boolean ): Try[types.ModelEntityConcept] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

  def addEntityRelationship(
    rIRI: IRI, rIRISource: IRI, rIRITarget: IRI,
    uIRI: IRI, uiIRI: Option[IRI],
    source: ModelEntityDefinition, target: ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics], isAbstract: Boolean ): Try[types.ModelEntityRelationship] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

  def addScalarDataType( scalarIRI: IRI ): Try[types.ModelScalarDataType] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

  def addDataRelationshipFromEntityToScalar(
    dIRI: IRI,
    source: types.ModelEntityDefinition,
    target: types.ModelScalarDataType ): Try[types.ModelDataRelationshipFromEntityToScalar] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

  def addDataRelationshipFromEntityToStructure(
    dIRI: IRI,
    source: types.ModelEntityDefinition,
    target: types.ModelStructuredDataType ): Try[types.ModelDataRelationshipFromEntityToStructure] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

  def addDataRelationshipFromStructureToScalar(
    dIRI: IRI,
    source: types.ModelStructuredDataType,
    target: types.ModelScalarDataType ): Try[types.ModelDataRelationshipFromStructureToScalar] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

  def addDataRelationshipFromStructureToStructure(
    dIRI: IRI,
    source: types.ModelStructuredDataType,
    target: types.ModelStructuredDataType ): Try[types.ModelDataRelationshipFromStructureToStructure] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

  def addEntityConceptSubClassAxiom(
    sub: types.ModelEntityConcept,
    sup: types.ModelEntityConcept ): Try[types.EntityConceptSubClassAxiom] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

  def addEntityDefinitionAspectSubClassAxiom(
    sub: types.ModelEntityDefinition,
    sup: types.ModelEntityAspect ): Try[types.EntityDefinitionAspectSubClassAxiom] =
    Failure( new IllegalArgumentException( "Read-Only" ) )

}

object ImmutableModelTerminologyGraph {

  def resolve( imports: Iterable[ModelTerminologyGraph], ont: OWLOntology )( implicit ops: OWLAPIOMFOps ): Try[ImmutableModelTerminologyGraph] = {

    import ops._

    val importedEntityDefinitionMaps = imports flatMap ( _.getEntityDefinitionMap ) toMap

    val DTs = ont.getDatatypesInSignature( Imports.EXCLUDED ) map ( dt => ( dt -> ModelScalarDataType( dt.getIRI ) ) ) toMap

    val ( bCs, tCs ) = ont.getClassesInSignature( Imports.EXCLUDED ) partition { c => isBackboneIRI( c.getIRI ) }
    val ( bOPs, tOPs ) = ont.getObjectPropertiesInSignature( Imports.EXCLUDED ) partition { c => isBackboneIRI( c.getIRI ) }
    val ( bDPs, tDPs ) = ont.getDataPropertiesInSignature( Imports.EXCLUDED ) partition { c => isBackboneIRI( c.getIRI ) }

    def isAnnotatedAbstract( iri: IRI ): Boolean = {
      for {
        aaa <- ont.getAnnotationAssertionAxioms( iri )
        if ( aaa.getProperty.getIRI == AnnotationIsAbstract )
      } {
        aaa.getValue match {
          case l: OWLLiteral if ( l.isBoolean ) => return l.parseBoolean
          case _                                => ()
        }
      }

      false
    }

    def isAnnotatedDerived( iri: IRI ): Boolean = {
      for {
        aaa <- ont.getAnnotationAssertionAxioms( iri )
        if ( aaa.getProperty.getIRI == AnnotationIsDerived )
      } {
        aaa.getValue match {
          case l: OWLLiteral if ( l.isBoolean ) => return l.parseBoolean
          case _                                => ()
        }
      }

      false
    }

    // Lookup of entity aspects

    def findDirectEntityAspect( iri: IRI, aspects: Map[OWLClass, ModelEntityAspect] ): Option[ModelEntityAspect] =
      ( for {
        ( aspectC, aspectM ) <- aspects
        if ( iri == aspectC.getIRI )
      } yield aspectM ) headOption

    def findImportedEntityAspect( iri: IRI ): Option[ModelEntityAspect] =
      ( for {
        g <- imports
        aspectM <- lookupEntityAspect( g, iri )
      } yield aspectM ) headOption

    def findEntityAspect( iri: IRI, aspects: Map[OWLClass, ModelEntityAspect] ): Option[ModelEntityAspect] =
      findDirectEntityAspect( iri, aspects ) orElse findImportedEntityAspect( iri )

    // Lookup of entity concepts

    def findDirectEntityConcept( iri: IRI, concepts: Map[OWLClass, ModelEntityConcept] ): Option[ModelEntityConcept] =
      ( for {
        ( conceptC, conceptM ) <- concepts
        if ( iri == conceptC.getIRI )
      } yield conceptM ) headOption

    def findImportedEntityConcept( iri: IRI ): Option[ModelEntityConcept] =
      ( for {
        g <- imports
        conceptM <- lookupEntityConcept( g, iri )
      } yield conceptM ) headOption

    def findEntityConcept( iri: IRI, concepts: Map[OWLClass, ModelEntityConcept] ): Option[ModelEntityConcept] =
      findDirectEntityConcept( iri, concepts ) orElse findImportedEntityConcept( iri )

    // Lookup of entity relationships

    def findDirectEntityRelationship( iri: IRI, relationships: Map[OWLClass, ModelEntityRelationship] ): Option[ModelEntityRelationship] =
      ( for {
        ( conceptC, conceptM ) <- relationships
        if ( iri == conceptC.getIRI )
      } yield conceptM ) headOption

    def findImportedEntityRelationship( iri: IRI ): Option[ModelEntityRelationship] =
      ( for {
        g <- imports
        conceptM <- lookupEntityRelationship( g, iri )
      } yield conceptM ) headOption

    def findEntityRelationship( iri: IRI, relationships: Map[OWLClass, ModelEntityRelationship] ): Option[ModelEntityRelationship] =
      findDirectEntityRelationship( iri, relationships ) orElse findImportedEntityRelationship( iri )

    // ------

    type DOPInfo = ( IRI, OWLDataProperty, OWLClass, OWLDatatype )
    
    def resolveDataRelationshipsFromEntity2Scalars(
        entityDefinitions: Map[OWLClass, ModelEntityDefinition],
        dataPropertyDPIRIs: Iterable[DOPInfo],
        DTs: Map[OWLDatatype, ModelScalarDataType]): Try[List[ModelDataRelationshipFromEntityToScalar]] = {
      val remainingDataPropertyDPIRIs = scala.collection.mutable.ListBuffer[DOPInfo]()
      remainingDataPropertyDPIRIs ++= dataPropertyDPIRIs
      
      val e2sc = for {
         remainingDataPropertyDPIRI <- remainingDataPropertyDPIRIs
         ( e2sc_iri, e2sc_dp, e2sc_source, e2sc_target ) = remainingDataPropertyDPIRI
         e2sc_sourceDef <- entityDefinitions.get( e2sc_source )
         e2sc_targetDef <- DTs.get( e2sc_target )
      } yield {
        remainingDataPropertyDPIRIs -= remainingDataPropertyDPIRI
        ModelDataRelationshipFromEntityToScalar( e2sc_iri, e2sc_sourceDef, e2sc_targetDef )
      }
      
      if ( remainingDataPropertyDPIRIs.isEmpty )
        Success( e2sc.toList )
      else
        Failure( new IllegalArgumentException("... esc ..."))
    }
    
    type ROPInfo = ( IRI, OWLObjectProperty, OWLClass, OWLClass )
        
    def resolveEntityDefinitionsForRelationships(
      entityDefinitions: Map[OWLClass, ModelEntityDefinition],
      RCs: Map[IRI, OWLClass],
      ROPs: Iterable[ROPInfo],
      sourceROPs: Iterable[ROPInfo],
      targetROPs: Iterable[ROPInfo],
      entityRelationships: Map[OWLClass, ModelEntityRelationship] ): Try[Map[OWLClass, ModelEntityRelationship]] = {
      val rcs = RCs.values.toSet
      val ( resolvableROPs, unresolvedROPs ) = ROPs.partition {
        case ( iri, op, source, target ) =>
          entityDefinitions.contains( source ) && entityDefinitions.contains( target )
      }
      val ( resolvableSourceROPs, unresolvableSourceROPs ) = sourceROPs.partition {
        case ( iri, op, source, target ) =>
          rcs.contains( source ) && entityDefinitions.contains( target )
      }
      val ( resolvableTargetROPs, unresolvableTargetROPs ) = targetROPs.partition {
        case ( iri, op, source, target ) =>
          rcs.contains( source ) && entityDefinitions.contains( target )
      }

      val remainingROPs = scala.collection.mutable.HashSet[ROPInfo]()
      remainingROPs ++= resolvableROPs

      val remainingSourceROPs = scala.collection.mutable.HashSet[ROPInfo]()
      remainingSourceROPs ++= resolvableSourceROPs

      val remainingTargetROPs = scala.collection.mutable.HashSet[ROPInfo]()
      remainingTargetROPs ++= resolvableTargetROPs

      val m = for {
        ( r_iri, r_op, r_source, r_target ) <- resolvableROPs
        if ( ! isAnnotatedDerived( r_iri ) )
        ( s_iri, s_op, s_source, s_target ) <- resolvableSourceROPs filter ( _._4 == r_source )
        ( t_iri, t_op, t_source, t_target ) <- resolvableTargetROPs filter ( _._4 == r_target )

        if ( s_source == t_source )

        inverseInfo = resolvableROPs find ( _rop => isAnnotatedDerived( _rop._1 ) && _rop._3 == r_target && _rop._4 == r_source )
        inverseROP = if (inverseInfo.isDefined) Some( inverseInfo.get._2) else None
        
        r_sourceDef <- entityDefinitions.get( r_source )
        r_targetDef <- entityDefinitions.get( r_target )

        rc = s_source

        resolvedROP = ( r_iri, r_op, r_source, r_target )
        resolvedSourceROP = ( s_iri, s_op, s_source, s_target )
        resolvedTargetROP = ( t_iri, t_op, t_source, t_target )

        rop = ModelEntityRelationship(
          iri=rc.getIRI,
          c=rc,
          unreified=r_op,
          inverse=inverseROP,
          source=r_sourceDef, rSource=s_op,
          target=r_targetDef, rTarget=t_op,
          characteristics=Nil,
          isAbstract=isAnnotatedAbstract( rc.getIRI ) )
      } yield {
        remainingROPs -= resolvedROP
        if ( inverseInfo.isDefined ) remainingROPs -= inverseInfo.get
        remainingSourceROPs -= resolvedSourceROP
        remainingTargetROPs -= resolvedTargetROP
        ( rc -> rop )
      }

      if ( ( remainingROPs.isEmpty && ( remainingSourceROPs.nonEmpty || remainingTargetROPs.nonEmpty ) ) ||
        ( remainingSourceROPs.isEmpty && ( remainingROPs.nonEmpty || remainingTargetROPs.nonEmpty ) ) ||
        ( remainingTargetROPs.isEmpty && ( remainingROPs.nonEmpty || remainingSourceROPs.nonEmpty ) ) ||
        remainingROPs.size != remainingSourceROPs.size ||
        remainingROPs.size != remainingTargetROPs.size )
        Failure( new IllegalArgumentException( "..." ) )

      else if ( remainingROPs.isEmpty && remainingSourceROPs.isEmpty && remainingTargetROPs.isEmpty &&
        unresolvedROPs.isEmpty && unresolvableSourceROPs.isEmpty && unresolvableTargetROPs.isEmpty )
        Success( m.toMap )

      else
        resolveEntityDefinitionsForRelationships(
          entityDefinitions ++ m.toMap,
          RCs ++ ( m map { case ( rc, rcDef ) => ( rc.getIRI -> rc ) } ).toMap,
          unresolvedROPs ++ remainingROPs,
          unresolvableSourceROPs ++ remainingSourceROPs,
          unresolvableTargetROPs ++ remainingTargetROPs,
          m.toMap )
    }

    // ------
    for {
      backbone <- Backbone.resolveBackbone( ont, bCs.toSet, bOPs.toSet, bDPs.toSet, ops )
      hasBackbone = backbone.isBackboneDefined
      g <- if ( !hasBackbone )
        Success( ImmutableModelTerminologyGraph(
          imports, ont,
          aspects = Nil,
          concepts = Nil,
          relationships = Nil,
          sc = DTs.values.toList,
          st = Nil,
          e2sc = Nil,
          e2st = Nil,
          s2sc = Nil,
          s2st = Nil,
          ax = Nil )( ops ) )
      else {
        val reasonerFactory = new StructuralReasonerFactory()
        val reasoner = reasonerFactory.createReasoner( ont )

        val thingSubClasses = reasoner.getSubClasses( backbone.ThingC.get, false )
        val entitySubClasses = reasoner.getSubClasses( backbone.EntityC.get, false )
        val reifiedObjectPropertySubClasses = reasoner.getSubClasses( backbone.ReifiedObjectPropertyC.get, false )
        val structuredDatatypeSubClasses = reasoner.getSubClasses( backbone.StructuredDatatypeC.get, false )
        val reifiedStructuredDataPropertySubClasses = reasoner.getSubClasses( backbone.ReifiedStructuredDataPropertyC.get, false )

        val thingCIRIs = ( for {
          thingN <- thingSubClasses
          if ( !thingN.isBottomNode )
          thingC = thingN.getRepresentativeElement
          if ( tCs.contains( thingC ) )
          thingIRI = thingC.getIRI
        } yield ( thingIRI -> thingC ) ).toMap;

        val conceptCIRIs = ( for {
          conceptN <- entitySubClasses
          if ( !conceptN.isBottomNode )
          conceptC = conceptN.getRepresentativeElement
          if ( tCs.contains( conceptC ) )
          conceptIRI = conceptC.getIRI
        } yield ( conceptIRI -> conceptC ) ).toMap;

        val reifiedObjectPropertyCIRIs = ( for {
          reifiedObjectPropertyN <- reifiedObjectPropertySubClasses
          if ( !reifiedObjectPropertyN.isBottomNode )
          reifiedObjectPropertyC = reifiedObjectPropertyN.getRepresentativeElement
          if ( tCs.contains( reifiedObjectPropertyC ) )
          reifiedObjectPropertyCIRI = reifiedObjectPropertyC.getIRI
        } yield ( reifiedObjectPropertyCIRI -> reifiedObjectPropertyC ) ).toMap;

        val reifiedStructuredDataPropertyCIRIs = ( for {
          reifiedStructuredDataPropertyN <- reifiedStructuredDataPropertySubClasses
          if ( !reifiedStructuredDataPropertyN.isBottomNode )
          reifiedStructuredDataPropertyC = reifiedStructuredDataPropertyN.getRepresentativeElement
          if ( tCs.contains( reifiedStructuredDataPropertyC ) )
          reifiedStructuredDataPropertyCIRI = reifiedStructuredDataPropertyC.getIRI
        } yield ( reifiedStructuredDataPropertyCIRI -> reifiedStructuredDataPropertyC ) ).toMap;

        val structuredDatatypeCIRIs = ( for {
          structuredDatatypeN <- structuredDatatypeSubClasses
          if ( !structuredDatatypeN.isBottomNode )
          structuredDatatypeC = structuredDatatypeN.getRepresentativeElement
          if ( tCs.contains( structuredDatatypeC ) )
          structuredDatatypeCIRI = structuredDatatypeC.getIRI
        } yield ( structuredDatatypeCIRI -> structuredDatatypeC ) ).toMap;

        val nonAspectIRIs = conceptCIRIs.keys ++ reifiedObjectPropertyCIRIs.keys ++ reifiedStructuredDataPropertyCIRIs.keys ++ structuredDatatypeCIRIs.keys
        val aspectCIRIs = thingCIRIs -- nonAspectIRIs

        val aspectCMs = ( for {
          ( aspectIRI, aspectC ) <- aspectCIRIs
          aspectM = ModelEntityAspect( aspectIRI, aspectC )
        } yield ( aspectC -> aspectM ) ).toMap;

        val conceptCMs = ( for {
          ( conceptIRI, conceptC ) <- conceptCIRIs
          conceptM = ModelEntityConcept( conceptIRI, conceptC, isAbstract = isAnnotatedAbstract( conceptIRI ) )
        } yield ( conceptC -> conceptM ) ).toMap;

        val conceptSubClassAxs = for {
          ( subC, subM ) <- conceptCMs
          supN <- reasoner.getSuperClasses( subC, true )
          supC = supN.getRepresentativeElement
          if ( supC != backbone.EntityC.get )
          supM <- findEntityConcept( supC.getIRI, conceptCMs )
        } yield EntityConceptSubClassAxiom( subM, null )

        val topReifiedObjectPropertySubOPs = reasoner.getSubObjectProperties( backbone.topReifiedObjectPropertyOP.get, false )
        val reifiedObjectPropertyOPIRIs = ( for {
          reifiedObjectPropertyN <- topReifiedObjectPropertySubOPs
          reifiedObjectPropertyOP <- reifiedObjectPropertyN flatMap { case op: OWLObjectProperty => Some( op ) }
          if ( tOPs.contains( reifiedObjectPropertyOP ) )
          reifiedObjectPropertyDomain <- reasoner.getObjectPropertyDomains( reifiedObjectPropertyOP, true ).getFlattened
          reifiedObjectPropertyRange <- reasoner.getObjectPropertyRanges( reifiedObjectPropertyOP, true ).getFlattened
        } yield (
          reifiedObjectPropertyOP.getIRI,
          reifiedObjectPropertyOP,
          reifiedObjectPropertyDomain,
          reifiedObjectPropertyRange ) )

        val topReifiedObjectPropertySourceSubOPs = reasoner.getSubObjectProperties( backbone.topReifiedObjectPropertySourceOP.get, false )
        val reifiedObjectPropertySourceOPIRIs = ( for {
          reifiedObjectPropertySourceN <- topReifiedObjectPropertySourceSubOPs
          reifiedObjectPropertySourceOP <- reifiedObjectPropertySourceN flatMap { case op: OWLObjectProperty => Some( op ) }
          if ( tOPs.contains( reifiedObjectPropertySourceOP ) )
          reifiedObjectPropertySourceDomain <- reasoner.getObjectPropertyDomains( reifiedObjectPropertySourceOP, true ).getFlattened
          reifiedObjectPropertySourceRange <- reasoner.getObjectPropertyRanges( reifiedObjectPropertySourceOP, true ).getFlattened
        } yield (
          reifiedObjectPropertySourceOP.getIRI,
          reifiedObjectPropertySourceOP,
          reifiedObjectPropertySourceDomain,
          reifiedObjectPropertySourceRange ) )

        val topReifiedObjectPropertyTargetSubOPs = reasoner.getSubObjectProperties( backbone.topReifiedObjectPropertyTargetOP.get, false )
        val reifiedObjectPropertyTargetOPIRIs = ( for {
          reifiedObjectPropertyTargetN <- topReifiedObjectPropertyTargetSubOPs
          reifiedObjectPropertyTargetOP <- reifiedObjectPropertyTargetN flatMap { case op: OWLObjectProperty => Some( op ) }
          if ( tOPs.contains( reifiedObjectPropertyTargetOP ) )
          reifiedObjectPropertyTargetDomain <- reasoner.getObjectPropertyDomains( reifiedObjectPropertyTargetOP, true ).getFlattened
          reifiedObjectPropertyTargetRange <- reasoner.getObjectPropertyRanges( reifiedObjectPropertyTargetOP, true ).getFlattened
        } yield (
          reifiedObjectPropertyTargetOP.getIRI,
          reifiedObjectPropertyTargetOP,
          reifiedObjectPropertyTargetDomain,
          reifiedObjectPropertyTargetRange ) )

        val topDataPropertySubDPs = reasoner.getSubDataProperties( backbone.topDataPropertyDP.get, false )
        val dataPropertyDPIRIs = ( for {
          dataPropertyN <- topDataPropertySubDPs
          dataPropertyDP <- dataPropertyN flatMap { case dp: OWLDataProperty => Some( dp ) }
          if ( tDPs.contains( dataPropertyDP ) )
          dataPropertyDomain <- reasoner.getDataPropertyDomains( dataPropertyDP, true ).getFlattened
          dataPropertyRange <- ont.getDataPropertyRangeAxioms( dataPropertyDP )
          dataPropertyType = dataPropertyRange.getRange.asOWLDatatype
        } yield ( dataPropertyDP.getIRI, dataPropertyDP, dataPropertyDomain, dataPropertyType ) )

        val allEntityDefinitionsExceptRelationships = importedEntityDefinitionMaps ++ aspectCMs ++ conceptCMs
        for {
          entityRelationshipCMs <- resolveEntityDefinitionsForRelationships(
            allEntityDefinitionsExceptRelationships,
            reifiedObjectPropertyCIRIs,
            reifiedObjectPropertyOPIRIs,
            reifiedObjectPropertySourceOPIRIs,
            reifiedObjectPropertyTargetOPIRIs,
            Map() )
          allEntityDefinitions = allEntityDefinitionsExceptRelationships ++ entityRelationshipCMs
          dataRelationshipsFromEntity2Scalars <- resolveDataRelationshipsFromEntity2Scalars( allEntityDefinitions, dataPropertyDPIRIs, DTs )
        } yield ImmutableModelTerminologyGraph(
          imports, ont,
          aspects = aspectCMs.values.toList,
          concepts = conceptCMs.values.toList,
          relationships = entityRelationshipCMs.values.toList,
          sc = DTs.values.toList,
          st = Nil,
          e2sc = dataRelationshipsFromEntity2Scalars,
          e2st = Nil,
          s2sc = Nil,
          s2st = Nil,
          ax = conceptSubClassAxs.toList )( ops )
      }
    } yield g
  }
}
