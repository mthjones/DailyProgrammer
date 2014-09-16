name := "Roguelike"

version := "1.0"

mainClass := Some("Game")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.scalaz" %% "scalaz-effect" % "7.1.0"
)