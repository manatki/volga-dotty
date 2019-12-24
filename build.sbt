val dottyVersion = "0.21.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "volga-dotty",
    version := "0.1",

    scalaVersion := dottyVersion,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0" withDottyCompat scalaVersion.value
  )
