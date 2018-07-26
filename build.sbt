name := "FDyrka"

version := "0.1"

scalaVersion := "2.12.6"

val Http4s = "0.19.0-M1"

libraryDependencies += "org.typelevel"  %% "cats-effect"      % "1.0.0-RC2"
libraryDependencies += "com.kailuowang" %% "mainecoon-macros" % "0.6.4"
libraryDependencies ++= List("blaze-server", "circe", "dsl")
  .map(module => "org.http4s" %% s"http4s-$module" % Http4s)

libraryDependencies += "com.github.mpilquist"       %% "simulacrum"       % "0.12.0"

addCompilerPlugin("org.scalameta"  % "paradise"            % "3.0.0-M11" cross CrossVersion.patch)
addCompilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.3")
addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.2.4")

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-language:higherKinds",
)