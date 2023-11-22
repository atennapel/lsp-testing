val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "lsp-testing",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.10",
    libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.21.1",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M10" % Test,
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-indent",
      "-new-syntax",
      "-print-lines",
      "-unchecked",
      "-Ykind-projector",
      "-Xfatal-warnings",
      "-Xmigration"
    )
  )
