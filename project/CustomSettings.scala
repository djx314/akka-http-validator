import sbt._
import sbt.Keys._

object CustomSettings {
  
  def customSettings = scalaSettings
  def commonProjectSettings = scalaSettings
  
  def scalaSettings =
    Seq(
      scalaVersion := "2.12.6",
      scalacOptions ++= Seq("-feature", "-deprecation")
    )

}