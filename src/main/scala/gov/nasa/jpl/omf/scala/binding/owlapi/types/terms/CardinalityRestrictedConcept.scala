package gov.nasa.jpl.omf.scala.binding.owlapi.types.terms

import gov.nasa.jpl.imce.oml.resolver.api
import gov.nasa.jpl.imce.oml.tables.taggedTypes.{LocalName, PositiveIntegerLiteral}
import org.semanticweb.owlapi.model.{IRI, OWLClass}

import scala.{Any, Boolean, Int}

case class CardinalityRestrictedConcept
(override val e: OWLClass,
 override val iri: IRI,
 override val name: LocalName,
 override val uuid: api.taggedTypes.CardinalityRestrictedConceptUUID,
 restrictionKind: gov.nasa.jpl.imce.oml.tables.CardinalityRestrictionKind,
 restrictedRelationship: RestrictableRelationship,
 restrictedRange: scala.Option[Entity],
 restrictedCardinality: PositiveIntegerLiteral)
  extends ConceptKind {

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: CardinalityRestrictedConcept => true
    case _ => false
  }

  override val hashCode: Int = (uuid, name, e).##

  override def equals(other: Any): Boolean = other match {
    case that: CardinalityRestrictedConcept =>
      (that canEqual this) &&
        (this.uuid == that.uuid) &&
        (this.name == that.name) &&
        (this.e == that.e)
    case _ =>
      false
  }


}
