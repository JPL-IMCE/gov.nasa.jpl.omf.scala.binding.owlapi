package gov.nasa.jpl.omf.scala.binding.owlapi.types.terms

import gov.nasa.jpl.imce.oml.resolver.api.taggedTypes.ConceptKindUUID

import scala.{Any,Boolean}

trait ConceptKind extends ConceptualEntity {

  override val uuid: ConceptKindUUID

  override def canEqual(other: Any)
  : Boolean
  = other match {
    case _: ConceptKind => true
    case _ => false
  }
}
