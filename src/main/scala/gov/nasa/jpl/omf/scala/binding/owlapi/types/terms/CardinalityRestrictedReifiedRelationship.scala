package gov.nasa.jpl.omf.scala.binding.owlapi.types.terms

import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables.taggedTypes.{LocalName, PositiveIntegerLiteral}
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFGraphStore
import org.semanticweb.owlapi.model.{IRI, OWLClass}

import scala.collection.immutable.Set
import scala.{Any, Boolean, Int}

case class CardinalityRestrictedReifiedRelationship
(override val e: OWLClass,
 override val iri: IRI,
 override val name: LocalName,
 override val uuid: api.taggedTypes.CardinalityRestrictedReifiedRelationshipUUID,
 override val source: Entity,
 override val target: Entity,
 restrictionKind: gov.nasa.jpl.imce.oml.tables.CardinalityRestrictionKind,
 restrictedRelationship: RestrictableRelationship,
 restrictedRange: scala.Option[Entity],
 restrictedCardinality: PositiveIntegerLiteral)
  extends ConceptualRelationship {

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: CardinalityRestrictedReifiedRelationship => true
    case _ => false
  }

  override val hashCode: Int =
  (uuid, name, source, target,
  restrictedRelationship, restrictionKind, restrictedRange, restrictedCardinality, e).##

  override def equals(other: Any): Boolean = other match {
    case that: CardinalityRestrictedReifiedRelationship =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.source == that.source) &&
        (this.target == that.target) &&
        (this.restrictedRelationship == that.restrictedRelationship) &&
        (this.restrictionKind == that.restrictionKind) &&
        (this.restrictedRange == that.restrictedRange) &&
        (this.restrictedCardinality == that.restrictedCardinality) &&
        (this.e == that.e)
    case _ =>
      false
  }

  override def rootCharacterizedEntityRelationships
  ()(implicit store: OWLAPIOMFGraphStore)
  : Set[_ <: CharacterizedEntityRelationship]
  = store.rootCharacterizedEntityRelationships(this)

}
