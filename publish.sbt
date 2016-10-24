
// publish to bintray.com via: `sbt publish`
//publishTo := Some(
//  "JPL-IMCE" at
//    s"https://api.bintray.com/content/jpl-imce/${organization.value}/${moduleName.value}/${version.value}")

// Error 409 (conflict)
publishTo := Some(
  "Artifactory Realm" at
    "https://cae-artifactory.jpl.nasa.gov/artifactory/maven-libs-release-local")

PgpKeys.useGpg := true

PgpKeys.useGpgAgent := true

pgpSecretRing := file("local.secring.gpg")

pgpPublicRing := file("local.pubring.gpg")

git.baseVersion := Versions.version

versionWithGit
