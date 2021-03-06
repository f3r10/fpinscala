val amm =  Seq("com.lihaoyi" % "ammonite" % "1.6.5" % "test" cross CrossVersion.full)

val commonSettings = Seq(
  scalaVersion := "2.12.6",
  libraryDependencies ++= {
    amm
  }
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises",
    sourceGenerators in Test += Def.task {
      val file = (sourceManaged in Test).value / "amm.scala"
      IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
      Seq(file)
    }.taskValue
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
