import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val server = (project in file("server"))
  .settings(commonSettings)
  .settings(
    scalaJSProjects := Seq(client),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    pipelineStages := Seq(digest, gzip),
    // triggers scalaJSPipeline when using compile or continuous compilation
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
    libraryDependencies ++= Seq(
      "com.vmunier" %% "scalajs-scripts" % "1.1.4",
      guice,
      "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test,
      "org.scalamock" %% "scalamock" % "4.4.0" % Test,
    ),
    // Compile the project before generating Eclipse files, so that generated .scala or .class files for views and routes are present
    EclipseKeys.preTasks := Seq(compile in Compile)
  )
  .enablePlugins(PlayScala, WebScalaJSBundlerPlugin)
  .dependsOn(sharedJvm)

lazy val client = (project in file("client"))
  .settings(commonSettings)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "1.0.0",
      "com.github.japgolly.scalajs-react" %%% "core" % "1.6.0"
    ),
    npmDependencies in Compile ++= Seq(
      "react" -> "16.7.0",
      "react-dom" -> "16.7.0"
    )
  )
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)
  .dependsOn(sharedJs)

lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(commonSettings)
lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

lazy val commonSettings = Seq(
  scalaVersion := "2.13.1",
  organization := "com.github.cyberscouter"
)

// loads the server project at sbt startup
onLoad in Global := (onLoad in Global).value.andThen(state =>
  "project server" :: state
)
