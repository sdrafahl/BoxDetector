val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "BoxDetector",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    assembly/assemblyJarName := "bounding-box",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "com.monovore" %% "decline" % "2.2.0",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.11",
    libraryDependencies += "eu.timepit" %% "refined" % "0.9.27",
    libraryDependencies += "io.circe" %% "circe-refined" % "0.14.1"
  )
