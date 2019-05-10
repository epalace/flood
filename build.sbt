name := "flood"
version := "0.1"
scalaVersion := "2.12.8"
libraryDependencies += "org.specs2" %% "specs2-core" % "4.0.3" % "test"
scalacOptions in Test ++= Seq("-Yrangepos")
