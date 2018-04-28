lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4"
    )),
    name := "scalatest-example"
  )

// import scalariform.formatter.preferences._
// lazy val compileScalastyle = taskKey[Unit]("compileScalastyle")
// compileScalastyle := scalastyle.in(Compile).toTask("")
// (compile in Compile) := ((compile in Compile) dependsOn compileScalastyle).value

// scalacOptions += "-feature"
// scalacOptions += "-Xfatal-warnings"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
