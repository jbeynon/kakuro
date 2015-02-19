name := "kakuro"

version in ThisBuild := "1.0.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.5"

lazy val `play-server` = project
  .dependsOn(modelJvm, `js-project`)
  .enablePlugins(PlayScala)
  .settings(
    unmanagedResources in Compile <<=
      (unmanagedResources in Compile, fastOptJS in (`js-project`, Compile)) map { (cp, js) => cp ++ Seq(js.data, new File(js.data.toString + ".map")) })

lazy val `js-project` = project
  .dependsOn(modelJs)
  .enablePlugins(ScalaJSPlugin)

lazy val model = crossProject.crossType(CrossType.Pure)

lazy val modelJvm = model.jvm

lazy val modelJs = model.js
