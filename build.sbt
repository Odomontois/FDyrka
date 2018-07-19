name := "FDyrka"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0-RC2"
libraryDependencies += "com.kailuowang" %% "mainecoon-macros" % "0.6.4"

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M11" cross CrossVersion.patch)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4")

scalacOptions += "-Ypartial-unification"