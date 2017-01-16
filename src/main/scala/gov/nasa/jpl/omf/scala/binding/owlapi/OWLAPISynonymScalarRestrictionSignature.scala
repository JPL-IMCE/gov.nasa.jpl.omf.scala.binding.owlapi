package gov.nasa.jpl.omf.scala.binding.owlapi

import java.util.UUID

import gov.nasa.jpl.imce.omf.schema.tables.LocalName
import gov.nasa.jpl.omf.scala.core.SynonymScalarRestrictionSignature

import scala.Predef.require

case class OWLAPISynonymScalarRestrictionSignature
( override val uuid: UUID,
  override val name: LocalName,
  override val iri: OWLAPIOMF#IRI,
  override val restrictedRange: OWLAPIOMF#DataRange)
  extends SynonymScalarRestrictionSignature[OWLAPIOMF]
{
  require(null != uuid)
  require(null != name)
  require(null != iri)
  require(null != restrictedRange)
}
