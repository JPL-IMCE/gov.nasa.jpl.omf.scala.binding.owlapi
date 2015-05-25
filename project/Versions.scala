object Versions {
  val scala = "2.11.6"

  // JPL MBEE release
  val jpl_mbee_release_prefix="1800.02-"

  // JPL Ontology Modeling Framework Core
  val jpl_omf_core = jpl_mbee_release_prefix+"19ead6712b774763f0314f5adbcc8911264fbc39"

  // JPL MBEE Common Scala Libraries
  val jpl_mbee_common_scala_libraries_revision="650c69e6e7defc0b3e430d9c3cb290e8b3cc1f88"
  val jpl_mbee_core = jpl_mbee_release_prefix+jpl_mbee_common_scala_libraries_revision
  val jpl_owlapi = jpl_mbee_release_prefix+jpl_mbee_common_scala_libraries_revision

  // IMCE-LOADPROD
  val imce_loadprod=jpl_mbee_release_prefix+"52e7f929d0b6ecbb55cd4bda24f9f724db1b7c28"
}
