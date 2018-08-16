import sbt._
import sbt.Keys._

lazy val ahv = (project in file("."))

CustomSettings.scalaSettings

javaOptions in run += "-Xmx3200M"