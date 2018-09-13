package gov.nasa.jpl.omf.scala.binding.owlapi.types.terms

import gov.nasa.jpl.imce.oml.resolver
import gov.nasa.jpl.omf.scala.binding.owlapi.OWLAPIOMFGraphStore
import gov.nasa.jpl.omf.scala.binding.owlapi.common.Predicate
import org.semanticweb.owlapi.model.OWLObjectProperty

import scala.{Any, Boolean}

trait RestrictableRelationship extends Predicate {
  val e: OWLObjectProperty

  override val uuid: resolver.api.taggedTypes.RestrictableRelationshipUUID

  def domain
  ()(implicit store: OWLAPIOMFGraphStore)
  : Entity

  def range
  ()(implicit store: OWLAPIOMFGraphStore)
  : Entity

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: RestrictableRelationship => true
    case _ => false
  }
}
