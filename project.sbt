
sbtPlugin := false

name := "gov.nasa.jpl.omf.scala.binding.owlapi"

description := "Binding of the Ontological Modeling Framework (OMF) Core API to OWL2-DL ontologies via the OWLAPI."

moduleName := name.value

organization := "gov.nasa.jpl.imce"

homepage := Some(url(s"https://jpl-imce.github.io/${moduleName.value}"))

organizationName := "JPL-IMCE"

organizationHomepage := Some(url(s"https://github.com/JPL-IMCE"))

git.remoteRepo := s"git@github.com:JPL-IMCE/${moduleName.value}"

startYear := Some(2015)

scmInfo := Some(ScmInfo(
  browseUrl = url(s"https://github.com/JPL-IMCE/${moduleName.value}"),
  connection = "scm:"+git.remoteRepo.value))

developers := List(
  Developer(
    id="NicolasRouquette",
    name="Nicolas F. Rouquette",
    email="nicolas.f.rouquette@jpl.nasa.gov",
    url=url("https://github.com/NicolasRouquette")))

