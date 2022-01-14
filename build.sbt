val scala2Version = "2.13.8"
val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "RockTheJVM CatsEffect",
    version := "0.0.1-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1",
    libraryDependencies += "org.typelevel" %% "cats-free" % "2.6.1",

    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.2.8",
    libraryDependencies += "org.typelevel" %% "cats-effect-laws" % "3.2.8",
    libraryDependencies += "org.typelevel" %% "cats-effect-kernel-testkit" % "3.2.8",

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
