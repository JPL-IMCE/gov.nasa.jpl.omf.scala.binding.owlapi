package gov.nasa.jpl.omf.scala.binding.owlapi.types.terms

import gov.nasa.jpl.imce.oml.resolver.api.taggedTypes.AspectKindUUID

import scala.{Any, Boolean}

trait AspectKind extends Entity {

  override val uuid: AspectKindUUID

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: AspectKind => true
    case _ => false
  }
}
