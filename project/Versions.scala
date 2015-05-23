object Versions {
  val scala = "2.11.6"

  // JPL MBEE release
  val jpl_mbee_release_prefix="1800.02-"

  // JPL Ontology Modeling Framework Core
  val jpl_omf_core = jpl_mbee_release_prefix+"76a10a81ecb77c1c765151ab34ea59a9f868c69c"

  // JPL MBEE Common Scala Libraries
  val jpl_mbee_common_scala_libraries_revision="650c69e6e7defc0b3e430d9c3cb290e8b3cc1f88"
  val jpl_mbee_core = jpl_mbee_release_prefix+jpl_mbee_common_scala_libraries_revision
  val jpl_owlapi = jpl_mbee_release_prefix+jpl_mbee_common_scala_libraries_revision

  // IMCE-LOADPROD
  val imce_loadprod=jpl_mbee_release_prefix+"4e67e5645272d2effb48f11742b736c447af92e9"
}
