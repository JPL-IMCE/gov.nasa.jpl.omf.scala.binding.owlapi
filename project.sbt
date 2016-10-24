
sbtPlugin := false

name := "gov.nasa.jpl.omf.scala.binding.owlapi"

description := "Binding of the Ontological Modeling Framework (OMF) Core API to OWL2-DL ontologies via the OWLAPI."

moduleName := name.value

organization := "gov.nasa.jpl.imce"

homepage := Some(url(s"https://jpl-imce.github.io/${moduleName.value}"))

organizationName := "JPL-IMCE"

organizationHomepage := Some(url(s"https://github.com/${organizationName.value}"))

git.remoteRepo := s"git@github.com:${organizationName.value}/${moduleName.value}"

scmInfo := Some(ScmInfo(
  browseUrl = url(s"https://github.com/${organizationName.value}/${moduleName.value}"),
  connection = "scm:"+git.remoteRepo.value))

developers := List(
  Developer(
    id="NicolasRouquette",
    name="Nicolas F. Rouquette",
    email="nicolas.f.rouquette@jpl.nasa.gov",
    url=url("https://github.com/NicolasRouquette")))

