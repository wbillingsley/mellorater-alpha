name := "five domains"

scalaVersion := "3.2.0"

enablePlugins(ScalaJSPlugin)

resolvers ++= Resolver.sonatypeOssRepos("snapshots")

libraryDependencies ++= Seq(
  "com.wbillingsley" %%% "doctacular" % "0.3.0+2-7eae7e5d-SNAPSHOT",
  "com.lihaoyi" %%% "upickle" % "3.0.0",
)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

val deployFast = taskKey[Unit]("Copies the fastLinkJS script to deployscripts/")
val deployFull = taskKey[Unit]("Copies the fullLinkJS script to deployscripts/")

// Used by GitHub Actions to get the script out from the .gitignored target directory
deployFast := {
  val opt = (Compile / fastOptJS).value
  IO.copyFile(opt.data, new java.io.File("target/compiled.js"))
}

deployFull := {
  val opt = (Compile / fullOptJS).value
  IO.copyFile(opt.data, new java.io.File("target/compiled.js"))
}