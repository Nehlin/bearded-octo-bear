lazy val root = (project in file(".")).
  settings(
    name := "Y",
    version := "1.0",
    scalaVersion := "2.11.4"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2"
