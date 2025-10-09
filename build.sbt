scalaVersion := "3.3.4" 

enablePlugins(ScalaNativePlugin,ScalaNativeBrewedConfigPlugin)

// set to Debug for compilation details (Info is default)
logLevel := Level.Info
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots
// import to add Scala Native options
import scala.scalanative.build._

val Http4sVersion = "0.23.30-161-f5b9629-SNAPSHOT"
val VilcacoraVersion = "0.0-e6dd86c-SNAPSHOT"
nativeBrewFormulas ++= Set("cereal", "openblas", "mlpack", "libsvm")
libraryDependencies ++= Seq(
  "org.http4s" %%% "http4s-ember-client" % Http4sVersion,
  "org.http4s" %%% "http4s-ember-server" % Http4sVersion,
  "org.http4s" %%% "http4s-dsl"          % Http4sVersion,
  "org.http4s" %%% "http4s-circe" % Http4sVersion,
  "io.circe" %%% "circe-generic" % "0.14.12",
  "org.typelevel" %%% "keypool" % "0.5.0-RC1",
  "com.armanbilge" %%% "vilcacora-ir" % VilcacoraVersion,
  "com.armanbilge" %%% "vilcacora-onnx" % VilcacoraVersion,
  "com.armanbilge" %%% "vilcacora-runtime" % VilcacoraVersion,
  )
nativeConfig ~= { _.withEmbedResources(true) }


