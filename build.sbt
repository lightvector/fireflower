
//Specifies that there is a project called 'root' that lives in directory "."
lazy val root = (project in file(".")).
  settings(
    name := "Fireflower",
    version := "1.0",
    scalaVersion := "2.11.12",

    fork in run := true,

    mainClass in assembly := Some("fireflower.ForHanabiLive"),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    },

    libraryDependencies += "org.glassfish.jaxb" % "jaxb-runtime" % "2.3.1",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.7.0",

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-language:existentials",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture"
    )

  )
