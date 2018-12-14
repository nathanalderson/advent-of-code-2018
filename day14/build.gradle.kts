/*
 * This file was generated by the Gradle 'init' task.
 *
 * This generated file contains a sample Scala library project to get you started.
 * For more details take a look at the Scala plugin chapter in the Gradle
 * user guide available at https://docs.gradle.org/5.0/userguide/scala_plugin.html
 */

plugins {
    // Apply the scala plugin to add support for Scala
    scala
    application
}

application {
    mainClassName = "day14.Main"
}

repositories {
    // Use jcenter for resolving your dependencies.
    // You can declare any Maven/Ivy/file repository here.
    jcenter()
}

dependencies {
    // Use Scala 2.12 in our library project
    implementation("org.scala-lang:scala-library:2.12.7")

    // Use Scalatest for testing our library
    testImplementation("junit:junit:4.12")
    testImplementation("org.scalatest:scalatest_2.12:3.0.5")

    // Need scala-xml at test runtime
    testRuntimeOnly("org.scala-lang.modules:scala-xml_2.12:1.1.1")
}
