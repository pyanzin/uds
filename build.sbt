name := "uds"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"

libraryDependencies += "net.liftweb" % "lift-json_2.11" % "3.0-M8"

libraryDependencies += "com.assembla.scala-incubator" %% "graph-core" % "1.11.0"


libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.8"

libraryDependencies += "com.typesafe.akka" %% "akka-http-experimental" % "2.4.8"

libraryDependencies += "com.github.etaty" %% "rediscala" % "1.6.0"

// old repo on bintray (1.5.0 and inferior version)
resolvers += "rediscala" at "http://dl.bintray.com/etaty/maven"
libraryDependencies += "com.etaty.rediscala" %% "rediscala" % "1.5.0"

// https://mvnrepository.com/artifact/org.neo4j.driver/neo4j-java-driver
libraryDependencies += "org.neo4j.driver" % "neo4j-java-driver" % "1.0.0"

