val akkaHttpVersion = "10.1.3"

val `akka-http` = Seq(
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream" % "2.5.12"
)

libraryDependencies ++= `akka-http`

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)

libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0"

val scalaTestVersion = "3.0.5"
libraryDependencies += "org.scalactic" %% "scalactic" % scalaTestVersion
libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

resolvers += Resolver.bintrayRepo("hseeberger", "maven")

libraryDependencies += "de.heikoseeberger" %% "akka-http-circe" % "1.21.0"

val circeVersion = "0.9.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)