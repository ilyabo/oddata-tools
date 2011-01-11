import sbt.{FileUtilities, Path, DefaultProject, ProjectInfo}

class Project(info: ProjectInfo) extends DefaultProject(info) with ProguardProject {

  val scalatest = "org.scalatest" % "scalatest" % "1.2"

  // override def artifactBaseName =
  override def minJarName = "oddata-tools-cli.jar"

  override def mainClass = Some("oddata.ODDataToolsMain")

  override def allDependencyJars = (super.allDependencyJars +++
    Path.fromFile(buildScalaInstance.compilerJar) +++
    Path.fromFile(buildScalaInstance.libraryJar)
  )

  override def proguardOptions = List(
    proguardKeepMain(mainClass.get),
    "-dontobfuscate",
//    "-dontoptimize",
//    "-dontskipnonpubliclibraryclasses",
//    "-dontskipnonpubliclibraryclassmembers",
//    "-dontpreverify",
    "-dontnote",
    "-dontwarn",
    "-keep class oddata.** { *; }",
    "-keep class countries.** { *; }",
    "-keep class geosearch.** { *; }",
    "-keep interface scala.ScalaObject"
  )

}
