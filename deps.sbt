val `akka-http` = Seq(
  "com.typesafe.akka" %% "akka-http"   % "10.1.3",
  "com.typesafe.akka" %% "akka-stream" % "2.5.12"
)

libraryDependencies ++= `akka-http`

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)

libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0"