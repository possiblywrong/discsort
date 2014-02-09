name := "linearsort"

version := "0.1"

scalaVersion := "2.10.3"

resolvers ++= Seq(
   "Sonatype releases"  at "http://oss.sonatype.org/content/repositories/releases/"
  ,"Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
  ,"Atalssian"          at "https://maven.atlassian.com/content/groups/public"
)

libraryDependencies in ThisBuild ++= Seq(
   "org.scalaz"  %% "scalaz-core"   % "7.0.5"
//  ,"com.chuusai" %  "shapeless"     % "2.0.0-SNAPSHOT" cross CrossVersion.full changing()
  ,"io.kadai"    %% "kadai-cmdopts" % "1.1.1"
)
