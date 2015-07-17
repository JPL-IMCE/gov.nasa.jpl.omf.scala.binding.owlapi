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
package gov.nasa.jpl.omf.scala.binding.owlapi.types

import gov.nasa.jpl.omf.scala.core.TerminologyKind._
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
import org.semanticweb.owlapi.reasoner.NodeSet
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.model.OWLObjectInverseOf
import gov.nasa.jpl.omf.scala.core.OMFOps

object ImmutableOperation extends Enumeration {
  type ImmutableOperation = Value
  val Save,
  AddEntityAspect,
  AddEntityConcept,
  AddEntityReifiedRelationship,
  AddScalarDataType,
  AddDataRelationshipFromEntityToScalar,
  AddDataRelationshipFromEntityToStructure,
  AddDataRelationshipFromStructureToScalar,
  AddDataRelationshipFromStructureToStructure,
  AddEntityConceptSubClassAxiom,
  AddEntityDefinitionAspectSubClassAxiom = Value
}

import ImmutableOperation._

sealed abstract class ImmutableModelTerminologyGraphException( val message: String )
  extends IllegalArgumentException( message )

case class ReadOnlyImmutableTerminologyGraphException( val operation: ImmutableOperation )
  extends ImmutableModelTerminologyGraphException(
    s"Operation '${operation}' is illegal on an read-only ImmutableTerminologyGraph" )

case class ImmutableModelTerminologyGraph
( override val kind: TerminologyKind,
  override val imports: Iterable[ModelTerminologyGraph],
  override val ont: OWLOntology,
  override val entityG: Option[IRI],
  override protected val aspects: List[ModelEntityAspect],
  override protected val concepts: List[ModelEntityConcept],
  override protected val reifiedRelationships: List[ModelEntityReifiedRelationship],
  override protected val unreifiedRelationships: List[ModelEntityUnreifiedRelationship],
  override protected val sc: List[ModelScalarDataType],
  override protected val st: List[ModelStructuredDataType],
  override protected val e2sc: List[ModelDataRelationshipFromEntityToScalar],
  override protected val e2st: List[ModelDataRelationshipFromEntityToStructure],
  override protected val s2sc: List[ModelDataRelationshipFromStructureToScalar],
  override protected val s2st: List[ModelDataRelationshipFromStructureToStructure],
  override protected val ax: List[ModelTermAxiom] )( override implicit val ops: OWLAPIOMFOps )
  extends ModelTerminologyGraph( kind, ont, entityG )( ops ) {

  override val isImmutableModelTerminologyGraph = true
  override val isMutableModelTerminologyGraph = false

  require( imports.forall( _.isImmutableModelTerminologyGraph ) )
  def immutableImports: Iterable[ImmutableModelTerminologyGraph] =
    imports.flatMap { case ig: ImmutableModelTerminologyGraph => Some( ig ) }

  /**
   * Reflexive, transitive closure of imports
   */
  def immutableImportClosure: Set[ImmutableModelTerminologyGraph] =
    OMFOps.closure[ImmutableModelTerminologyGraph, ImmutableModelTerminologyGraph]( this, _.immutableImports ) + this

  val getEntityDefinitionMap: Map[OWLClass, ModelEntityDefinition] =
    ( ( aspects map ( a => a.e -> a ) ) ++
      ( concepts map ( c => c.e -> c ) ) ++
      ( reifiedRelationships map ( r => r.e -> r ) ) ) toMap

  override protected val iri2typeTerm = {
    def term2pair[T <: ModelTypeTerm]( t: T ) = t.iri -> t

    ( aspects map term2pair ) ++
      ( concepts map term2pair ) ++
      ( reifiedRelationships map term2pair ) ++
      ( sc map term2pair ) ++
      ( st map term2pair ) ++
      ( e2sc map term2pair ) ++
      ( e2st map term2pair ) ++
      ( s2sc map term2pair ) ++
      ( s2st map term2pair ) toMap
  }

}

case class ResolverHelper(
  val imports: Iterable[ImmutableModelTerminologyGraph],
  val ont: OWLOntology,
  val ops: OWLAPIOMFOps ) {

  import ops._

  def isAnnotatedAbstract( iri: IRI ): Boolean = {
    for {
      aaa <- ont.getAnnotationAssertionAxioms( iri )
      if aaa.getProperty.getIRI == AnnotationIsAbstract
    } {
      aaa.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          return l.parseBoolean
        case _ =>
          ()
      }
    }

    false
  }

  def isAnnotatedDerived( iri: IRI ): Boolean = {
    for {
      aaa <- ont.getAnnotationAssertionAxioms( iri )
      if aaa.getProperty.getIRI == AnnotationIsDerived
    } {
      aaa.getValue match {
        case l: OWLLiteral if l.isBoolean =>
          return l.parseBoolean
        case _ =>
          ()
      }
    }

    false
  }

  def getEntityGraphIRIAnnotation( iri: IRI ): Option[IRI] = {
    for {
      aaa <- ont.getAnnotationAssertionAxioms( iri )
      if aaa.getProperty.getIRI == AnnotationEntityGraphIRI
    } {
      aaa.getValue match {
        case gIRI: IRI => return Some( gIRI )
        case _         => ()
      }
    }

    None
  }

  def getGraphForEntityIRIAnnotation( iri: IRI ): Option[IRI] = {
    for {
      aaa <- ont.getAnnotationAssertionAxioms( iri )
      if aaa.getProperty.getIRI == AnnotationGraphForEntityIRI
    } {
      aaa.getValue match {
        case gIRI: IRI => return Some( gIRI )
        case _         => ()
      }
    }

    None
  }

  // Lookup of entity aspects

  def findDirectEntityAspect( iri: IRI, aspects: Map[OWLClass, ModelEntityAspect] ): Option[ModelEntityAspect] =
    ( for {
      ( aspectC, aspectM ) <- aspects
      if iri == aspectC.getIRI
    } yield aspectM ) headOption

  def findImportedEntityAspect( iri: IRI ): Option[ModelEntityAspect] =
    ( for {
      g <- imports
      aspectM <- ops.lookupEntityAspect( g, iri )
    } yield aspectM ) headOption

  def findEntityAspect( iri: IRI, aspects: Map[OWLClass, ModelEntityAspect] ): Option[ModelEntityAspect] =
    findDirectEntityAspect( iri, aspects ) orElse findImportedEntityAspect( iri )

  // Lookup of entity concepts

  def findDirectEntityConcept( iri: IRI, concepts: Map[OWLClass, ModelEntityConcept] ): Option[ModelEntityConcept] =
    ( for {
      ( conceptC, conceptM ) <- concepts
      if iri == conceptC.getIRI
    } yield conceptM ) headOption

  def findImportedEntityConcept( iri: IRI ): Option[ModelEntityConcept] =
    ( for {
      g <- imports
      conceptM <- ops.lookupEntityConcept( g, iri )
    } yield conceptM ) headOption

  def findEntityConcept( iri: IRI, concepts: Map[OWLClass, ModelEntityConcept] ): Option[ModelEntityConcept] =
    findDirectEntityConcept( iri, concepts ) orElse findImportedEntityConcept( iri )

  // Lookup of entity relationships

  def findDirectEntityReifiedRelationship
  ( iri: IRI, relationships: Map[OWLClass, ModelEntityReifiedRelationship] )
  : Option[ModelEntityReifiedRelationship] =
    ( for {
      ( conceptC, conceptM ) <- relationships
      if iri == conceptC.getIRI
    } yield conceptM ) headOption

  def findImportedEntityReifiedRelationship
  ( iri: IRI )
  : Option[ModelEntityReifiedRelationship] =
    ( for {
      g <- imports
      conceptM <- ops.lookupEntityReifiedRelationship( g, iri )
    } yield conceptM ) headOption

  def findEntityReifiedRelationship
  ( iri: IRI, relationships: Map[OWLClass, ModelEntityReifiedRelationship] )
  : Option[ModelEntityReifiedRelationship] =
    findDirectEntityReifiedRelationship( iri, relationships ) orElse findImportedEntityReifiedRelationship( iri )

  // ------

  type DOPInfo = ( OWLDataProperty, OWLClass, OWLDatatype )

  def resolveDataPropertyDPIRIs
  ( subDPs: NodeSet[OWLDataProperty], tDPs: Set[OWLDataProperty] )
  ( implicit reasoner: OWLReasoner )
  : Iterable[DOPInfo] =
    for {
      dataPropertyN <- subDPs
      dataPropertyDP <- dataPropertyN flatMap { case dp: OWLDataProperty => Some( dp ) }
      if tDPs.contains( dataPropertyDP )
      dataPropertyDomain <- reasoner.getDataPropertyDomains( dataPropertyDP, true ).getFlattened
      dataPropertyRange <- ont.getDataPropertyRangeAxioms( dataPropertyDP )
      dataPropertyType = dataPropertyRange.getRange.asOWLDatatype
    } yield ( dataPropertyDP, dataPropertyDomain, dataPropertyType )

  def resolveDataRelationshipsFromEntity2Scalars
  ( entityDefinitions: Map[OWLClass, ModelEntityDefinition],
    dataPropertyDPIRIs: Iterable[DOPInfo],
    DTs: Map[OWLDatatype, ModelScalarDataType] )
  : Try[List[ModelDataRelationshipFromEntityToScalar]] = {
    val remainingDataPropertyDPIRIs = scala.collection.mutable.ListBuffer[DOPInfo]()
    remainingDataPropertyDPIRIs ++= dataPropertyDPIRIs

    val e2sc = for {
      remainingDataPropertyDPIRI <- remainingDataPropertyDPIRIs
      ( e2sc_dp, e2sc_source, e2sc_target ) = remainingDataPropertyDPIRI
      e2sc_sourceDef <- entityDefinitions.get( e2sc_source )
      e2sc_targetDef <- DTs.get( e2sc_target )
    } yield {
      remainingDataPropertyDPIRIs -= remainingDataPropertyDPIRI
      ModelDataRelationshipFromEntityToScalar( e2sc_dp, e2sc_sourceDef, e2sc_targetDef )
    }

    if ( remainingDataPropertyDPIRIs.isEmpty )
      Success( e2sc.toList )
    else
      Failure( new IllegalArgumentException( "... esc ..." ) )
  }

  // ----------

  type ROPInfo = ( IRI, OWLObjectProperty, OWLClass, OWLClass, Option[OWLObjectProperty] )

  def ropInfoToString( ropInfo: ROPInfo ): String =
    s"""|ROPInfo 
        |       iri=${ropInfo._1}
        | obj. prop=${ropInfo._2}
        |    domain=${ropInfo._3}
        |     range=${ropInfo._4}
        | inv o. p.=${ropInfo._5}
        |""".stripMargin( '|' )

  def resolveEntityDefinitionsForRelationships
  ( entityDefinitions: Map[OWLClass, ModelEntityDefinition],
    RCs: Map[IRI, OWLClass],
    ROPs: Iterable[ROPInfo],
    sourceROPs: Iterable[ROPInfo],
    targetROPs: Iterable[ROPInfo],
    entityReifiedRelationships: Map[OWLClass, ModelEntityReifiedRelationship] )
  : Try[Map[OWLClass, ModelEntityReifiedRelationship]] = {
    import ops._

    val rcs = RCs.values.toSet
    val ( resolvableROPs, unresolvedROPs ) = ROPs.partition {
      case ( _, op, source, target, _ ) =>
        entityDefinitions.contains( source ) && entityDefinitions.contains( target )
    }
    val ( resolvableSourceROPs, unresolvableSourceROPs ) = sourceROPs.partition {
      case ( _, op, source, target, _ ) =>
        rcs.contains( source ) && entityDefinitions.contains( target )
    }
    val ( resolvableTargetROPs, unresolvableTargetROPs ) = targetROPs.partition {
      case ( _, op, source, target, _ ) =>
        rcs.contains( source ) && entityDefinitions.contains( target )
    }

    val remainingROPs = scala.collection.mutable.HashSet[ROPInfo]()
    remainingROPs ++= resolvableROPs

    val remainingSourceROPs = scala.collection.mutable.HashSet[ROPInfo]()
    remainingSourceROPs ++= resolvableSourceROPs

    val remainingTargetROPs = scala.collection.mutable.HashSet[ROPInfo]()
    remainingTargetROPs ++= resolvableTargetROPs

    val m = for {
      ( r_iri, r_op, r_source, r_target, r_inv_op ) <- resolvableROPs
      ( s_iri, s_op, s_source, s_target, s_inv_op ) <- resolvableSourceROPs filter ( _._4 == r_source )
      ( t_iri, t_op, t_source, t_target, t_inv_op ) <- resolvableTargetROPs filter ( _._4 == r_target )

      if s_source == t_source

      r_sourceDef <- entityDefinitions.get( r_source )
      r_targetDef <- entityDefinitions.get( r_target )

      rc = s_source

      resolvedROP = ( r_iri, r_op, r_source, r_target, r_inv_op )
      resolvedSourceROP = ( s_iri, s_op, s_source, s_target, s_inv_op )
      resolvedTargetROP = ( t_iri, t_op, t_source, t_target, t_inv_op )

      rop = ModelEntityReifiedRelationship(
        e = rc, eg = getEntityGraphIRIAnnotation( r_iri ),
        unreified = r_op,
        inverse = None,
        source = r_sourceDef, rSource = s_op,
        target = r_targetDef, rTarget = t_op,
        characteristics = Nil,
        isAbstract = isAnnotatedAbstract( rc.getIRI ) )
    } yield {
      remainingROPs -= resolvedROP
      remainingSourceROPs -= resolvedSourceROP
      remainingTargetROPs -= resolvedTargetROP
      rc -> rop
    }

    if ( ( remainingROPs.isEmpty && ( remainingSourceROPs.nonEmpty || remainingTargetROPs.nonEmpty ) ) ||
      ( remainingSourceROPs.isEmpty && ( remainingROPs.nonEmpty || remainingTargetROPs.nonEmpty ) ) ||
      ( remainingTargetROPs.isEmpty && ( remainingROPs.nonEmpty || remainingSourceROPs.nonEmpty ) ) ||
      remainingROPs.size != remainingSourceROPs.size ||
      remainingROPs.size != remainingTargetROPs.size ) {

      val rops = remainingROPs.map( ropInfoToString ).mkString( "\n" )
      val srops = remainingSourceROPs.map( ropInfoToString ).mkString( "\n" )
      val trops = remainingTargetROPs.map( ropInfoToString ).mkString( "\n" )

      Failure( new IllegalArgumentException(
        s"""|Unresolved Reified Object Properties, ROPs: 
              |
              |*** ${remainingROPs.size} remaining ROPs *** 
              |$rops
              |
              |*** ${remainingSourceROPs.size} remaining source ROPs ***
              |$srops
              |
              |*** ${remainingTargetROPs.size} remaining target ROPs *** 
              |$trops
              |""".stripMargin( '|' ) ) )
    } else if ( remainingROPs.isEmpty && remainingSourceROPs.isEmpty && remainingTargetROPs.isEmpty &&
      unresolvedROPs.isEmpty && unresolvableSourceROPs.isEmpty && unresolvableTargetROPs.isEmpty )
      Success( m.toMap )

    else
      resolveEntityDefinitionsForRelationships(
        entityDefinitions ++ m.toMap,
        RCs ++ ( m map { case ( rc, rcDef ) => rc.getIRI -> rc } ).toMap,
        unresolvedROPs ++ remainingROPs,
        unresolvableSourceROPs ++ remainingSourceROPs,
        unresolvableTargetROPs ++ remainingTargetROPs,
        m.toMap )
  }

  // ---------

  def resolveThingCIRIs
  ( thingSubClasses: NodeSet[OWLClass], tCs: Set[OWLClass] )
  : Map[IRI, OWLClass] =
    ( for {
      thingN <- thingSubClasses
      if !thingN.isBottomNode
      thingC = thingN.getRepresentativeElement
      if tCs.contains( thingC )
      thingIRI = thingC.getIRI
    } yield thingIRI -> thingC ).toMap

  def resolveAspectCIRIs
  ( entitySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass] )
  : Map[IRI, OWLClass] =
    ( for {
      aspectN <- entitySubClasses
      if !aspectN.isBottomNode
      aspectC = aspectN.getRepresentativeElement
      if tCs.contains( aspectC )
      aspectIRI = aspectC.getIRI
    } yield aspectIRI -> aspectC ).toMap

  def resolveConceptCIRIs
  ( entitySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass] )
  : Map[IRI, OWLClass] =
    ( for {
      conceptN <- entitySubClasses
      if !conceptN.isBottomNode
      conceptC = conceptN.getRepresentativeElement
      if tCs.contains( conceptC )
      conceptIRI = conceptC.getIRI
    } yield conceptIRI -> conceptC ).toMap

  def resolveReifiedObjectPropertyCIRIs
  ( reifiedObjectPropertySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass] )
  : Map[IRI, OWLClass] =
    ( for {
      reifiedObjectPropertyN <- reifiedObjectPropertySubClasses
      if !reifiedObjectPropertyN.isBottomNode
      reifiedObjectPropertyC = reifiedObjectPropertyN.getRepresentativeElement
      if tCs.contains( reifiedObjectPropertyC )
      reifiedObjectPropertyCIRI = reifiedObjectPropertyC.getIRI
    } yield reifiedObjectPropertyCIRI -> reifiedObjectPropertyC ).toMap

  def resolveReifiedStructuredDataPropertyCIRIs
  ( reifiedStructuredDataPropertySubClasses: NodeSet[OWLClass], tCs: Set[OWLClass] )
  : Map[IRI, OWLClass] =
    ( for {
      reifiedStructuredDataPropertyN <- reifiedStructuredDataPropertySubClasses
      if !reifiedStructuredDataPropertyN.isBottomNode
      reifiedStructuredDataPropertyC = reifiedStructuredDataPropertyN.getRepresentativeElement
      if tCs.contains( reifiedStructuredDataPropertyC )
      reifiedStructuredDataPropertyCIRI = reifiedStructuredDataPropertyC.getIRI
    } yield reifiedStructuredDataPropertyCIRI -> reifiedStructuredDataPropertyC ).toMap

  def resolveStructuredDatatypeCIRIs
  ( structuredDatatypeSubClasses: NodeSet[OWLClass], tCs: Set[OWLClass] )
  : Map[IRI, OWLClass] =
    ( for {
      structuredDatatypeN <- structuredDatatypeSubClasses
      if !structuredDatatypeN.isBottomNode
      structuredDatatypeC = structuredDatatypeN.getRepresentativeElement
      if tCs.contains( structuredDatatypeC )
      structuredDatatypeCIRI = structuredDatatypeC.getIRI
    } yield structuredDatatypeCIRI -> structuredDatatypeC ).toMap

  def resolveDomainRangeForObjectProperties
  ( subOPs: NodeSet[OWLObjectPropertyExpression], tOPs: Set[OWLObjectProperty] )
  ( implicit reasoner: OWLReasoner )
  : Iterable[ROPInfo] =
    ( for {
      _n_ <- subOPs
      _op_ <- _n_ flatMap {
        case op: OWLObjectProperty =>
          if ( tOPs.contains( op ) && !isAnnotatedDerived( op.getIRI ) )
            Some( op )
          else
            None
        case inv: OWLObjectInverseOf =>
          inv.getInverse match {
            case op: OWLObjectProperty =>
              if ( tOPs.contains( op ) && !isAnnotatedDerived( op.getIRI ) )
                Some( op )
              else
                None
            case _ =>
              None
          }
      }
      _d_ <- reasoner.getObjectPropertyDomains( _op_, true ).getFlattened
      _r_ <- reasoner.getObjectPropertyRanges( _op_, true ).getFlattened
    } yield {
      val INV = ( _n_ flatMap {
        case op: OWLObjectProperty =>
          if ( tOPs.contains( op ) && isAnnotatedDerived( op.getIRI ) )
            Some( op )
          else
            None
        case inv: OWLObjectInverseOf =>
          inv.getInverse match {
            case op: OWLObjectProperty =>
              if ( tOPs.contains( op ) && isAnnotatedDerived( op.getIRI ) )
                Some( op )
              else
                None
            case _ =>
              None
          }
      } ).headOption
      ( _op_.getIRI, _op_, _d_, _r_, INV )
    } ).toSet

  def resolveConceptSubClassAxioms
  ( conceptCMs: Map[OWLClass, ModelEntityConcept] )
  ( implicit reasoner: OWLReasoner, backbone: OMFBackbone )
  : Iterable[EntityConceptSubClassAxiom] =
    for {
      ( subC, subM ) <- conceptCMs
      supN <- reasoner.getSuperClasses( subC, true )
      supC = supN.getRepresentativeElement
      if supC != backbone.EntityC
      supM <- findEntityConcept( supC.getIRI, conceptCMs )
    } yield EntityConceptSubClassAxiom( subM, null )
}

case class ImmutableModelTerminologyGraphResolver( resolver: ResolverHelper ) {

  import resolver._
  import resolver.ops._

  def resolve: Try[ImmutableModelTerminologyGraph] = {
    val DTs = ont.getDatatypesInSignature( Imports.EXCLUDED ) map
      ( dt => dt -> ModelScalarDataType( dt.getIRI ) ) toMap

    val ( bCs, tCs ) = ont.getClassesInSignature( Imports.EXCLUDED ) partition
      { c => isBackboneIRI( c.getIRI ) }

    val ( bOPs, tOPs ) = ont.getObjectPropertiesInSignature( Imports.EXCLUDED ) partition
      { c => isBackboneIRI( c.getIRI ) }

    val ( bDPs, tDPs ) = ont.getDataPropertiesInSignature( Imports.EXCLUDED ) partition
      { c => isBackboneIRI( c.getIRI ) }

    Backbone.resolveBackbone( ont, bCs.toSet, bOPs.toSet, bDPs.toSet, ops ) match {
      case Failure( t ) => Failure( t )
      case Success( _: NoBackbone ) =>
        Success( ImmutableModelTerminologyGraph(
          kind = isDefinition,
          imports=imports,
          ont=ont,
          entityG=None,
          aspects = Nil,
          concepts = Nil,
          reifiedRelationships = Nil,
          unreifiedRelationships = Nil,
          sc = DTs.values.toList,
          st = Nil,
          e2sc = Nil,
          e2st = Nil,
          s2sc = Nil,
          s2st = Nil,
          ax = Nil )( ops ) )
      case Success( backbone: OMFBackbone ) =>
        resolve( backbone, DTs, tCs.toSet, tOPs.toSet, tDPs.toSet )
    }
  }

  def resolve(
    backbone: OMFBackbone,
    DTs: Map[OWLDatatype, ModelScalarDataType],
    tCs: Set[OWLClass],
    tOPs: Set[OWLObjectProperty],
    tDPs: Set[OWLDataProperty] ): Try[ImmutableModelTerminologyGraph] = {

    import ops._

    implicit val _backbone = backbone

    val importedEntityDefinitionMaps = imports.flatMap( _.immutableImportClosure ) flatMap
      ( _.getEntityDefinitionMap ) toMap

    val reasonerFactory = new StructuralReasonerFactory()
    implicit val reasoner = reasonerFactory.createReasoner( ont )

    val thingCIRIs =
      resolveThingCIRIs(
        reasoner.getSubClasses( backbone.ThingC, false ),
        tCs )

    val conceptCIRIs =
      resolveConceptCIRIs(
        reasoner.getSubClasses( backbone.EntityC, false ),
        tCs )

    val reifiedObjectPropertyCIRIs =
      resolveReifiedObjectPropertyCIRIs(
        reasoner.getSubClasses( backbone.ReifiedObjectPropertyC, false ),
        tCs )

    val reifiedStructuredDataPropertyCIRIs =
      resolveReifiedStructuredDataPropertyCIRIs( reasoner.getSubClasses( backbone.ReifiedStructuredDataPropertyC, false ), tCs )

    val structuredDatatypeCIRIs =
      resolveStructuredDatatypeCIRIs( reasoner.getSubClasses( backbone.StructuredDatatypeC, false ), tCs )

    val nonAspectCs =
      conceptCIRIs.values ++
        reifiedObjectPropertyCIRIs.values ++
        reifiedStructuredDataPropertyCIRIs.values ++
        structuredDatatypeCIRIs.values

    val ontIRIPrefix = backbone.ont.getOntologyID.getOntologyIRI.get.toString

    val aCs = (tCs -- nonAspectCs).filter { c =>
      c.getIRI.toString.startsWith(ontIRIPrefix)
    }

    val aspectCIRIs =
      resolveAspectCIRIs(
        reasoner.getSubClasses( backbone.AspectC, false ),
        aCs )

    System.out.println(s"backbone: ${backbone.ont.getOntologyID.toString}")
    aspectCIRIs.keySet.toList.sortBy(_.toString).foreach { a =>
      System.out.println(s" aspect: ${a.toString}")
    }

    //val aspectCIRIs = thingCIRIs -- nonAspectIRIs

    val aspectCMs = for {
      ( _, aspectC ) <- aspectCIRIs
      aspectM = ModelEntityAspect( aspectC )
    } yield aspectC -> aspectM

    val conceptCMs = for {
      ( conceptIRI, conceptC ) <- conceptCIRIs
      conceptM =
      ModelEntityConcept( conceptC, getEntityGraphIRIAnnotation( conceptIRI ), isAnnotatedAbstract( conceptIRI ) )
    } yield conceptC -> conceptM

    val structuredDatatypeSCs = for {
      ( _, structuredDatatypeC ) <- structuredDatatypeCIRIs
      structuredDatatypeSC = ModelStructuredDataType( structuredDatatypeC )
    } yield structuredDatatypeC -> structuredDatatypeSC

    val conceptSubClassAxs = resolveConceptSubClassAxioms( conceptCMs )

    val topReifiedObjectPropertySubOPs =
      reasoner.getSubObjectProperties( backbone.topReifiedObjectPropertyOP, false )

    val reifiedObjectPropertyOPIRIs =
      resolveDomainRangeForObjectProperties( topReifiedObjectPropertySubOPs, tOPs )

    val topReifiedObjectPropertySourceSubOPs =
      reasoner.getSubObjectProperties( backbone.topReifiedObjectPropertySourceOP, false )

    val reifiedObjectPropertySourceOPIRIs =
      resolveDomainRangeForObjectProperties( topReifiedObjectPropertySourceSubOPs, tOPs )

    val topReifiedObjectPropertyTargetSubOPs =
      reasoner.getSubObjectProperties( backbone.topReifiedObjectPropertyTargetOP, false )

    val reifiedObjectPropertyTargetOPIRIs =
      resolveDomainRangeForObjectProperties( topReifiedObjectPropertyTargetSubOPs, tOPs )

    val dataPropertyDPIRIs =
      resolveDataPropertyDPIRIs( reasoner.getSubDataProperties( backbone.topDataPropertyDP, false ), tDPs )

    val allEntityDefinitionsExceptRelationships =
      importedEntityDefinitionMaps ++
        aspectCMs ++
        conceptCMs

    for {
      entityReifiedRelationshipCMs <- resolveEntityDefinitionsForRelationships(
        allEntityDefinitionsExceptRelationships,
        reifiedObjectPropertyCIRIs,
        reifiedObjectPropertyOPIRIs,
        reifiedObjectPropertySourceOPIRIs,
        reifiedObjectPropertyTargetOPIRIs,
        Map() )
      allEntityDefinitions = allEntityDefinitionsExceptRelationships ++ entityReifiedRelationshipCMs
      dataRelationshipsFromEntity2Scalars <-
        resolveDataRelationshipsFromEntity2Scalars( allEntityDefinitions, dataPropertyDPIRIs, DTs )
      _ = System.out.println(
      s"""TBox: ${ont.getOntologyID.getOntologyIRI.get}
          | aspects: ${aspectCMs.values.size}
          | concepts: ${conceptCMs.values.size}
          | reifiedRelationships: ${entityReifiedRelationshipCMs.values.size}
       """.stripMargin
      )
      _ = aspectCMs.values.toList.sortBy(_.iri.toString).foreach { a =>
        System.out.println(s" a: ${a.iri}")
      }
    } yield ImmutableModelTerminologyGraph(
      kind = backbone.kind,
      imports=imports,
      ont=ont,
      entityG=getGraphForEntityIRIAnnotation( ont.getOntologyID.getOntologyIRI.get ),
      aspects = aspectCMs.values.toList,
      concepts = conceptCMs.values.toList,
      reifiedRelationships = entityReifiedRelationshipCMs.values.toList,
      unreifiedRelationships = Nil,
      sc = DTs.values.toList,
      st = Nil,
      e2sc = dataRelationshipsFromEntity2Scalars,
      e2st = Nil,
      s2sc = Nil,
      s2st = Nil,
      ax = conceptSubClassAxs.toList )( ops )
  }
}