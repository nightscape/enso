<<<<<<< HEAD
import sbt.Keys.scalacOptions
=======
import scala.sys.process._

>>>>>>> master
// Global Configuration
organization := "org.enso"
scalaVersion in ThisBuild := "2.12.8"

val monocleVersion = "1.6.0"

// Compiler Options
scalacOptions in ThisBuild ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8",                         // Specify character encoding used by source files.
  "-explaintypes",                 // Explain type errors in more detail.
  "-feature",                      // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds",         // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
//  "-Xcheckinit",                   // Wrap field accessors to throw an exception on uninitialized access.
//  "-Xfatal-warnings",                 // Fail the compilation if there are any warnings.
//  "-Xfuture",                         // Turn on future language features.
  "-Xlint:adapted-args",              // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:constant",                  // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",        // Selecting member of DelayedInit.
  "-Xlint:doc-detached",              // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",              // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                 // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",      // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override",          // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit",              // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",           // Option.apply used implicit view.
  "-Xlint:package-object-classes",    // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",    // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",            // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",               // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",     // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match",             // Pattern match may not be typesafe.
  "-Yno-adapted-args",                // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification",            // Enable partial unification in type constructor inference
  "-Ywarn-dead-code",                 // Warn when dead code is identified.
  "-Ywarn-extra-implicit",            // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible",              // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any",                 // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override",          // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit",              // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen",             // Warn when numerics are widened.
  "-Ywarn-unused:implicits",          // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",            // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",             // Warn if a local definition is unused.
  "-Ywarn-unused:params",             // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",            // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",           // Warn if a private member is unused.
  "-Ywarn-value-discard",             // Warn when non-Unit expression results are unused.
  "-Ypartial-unification",
//  "-language:implicitConversions",
  "-Xmacro-settings:-logging@org.enso"
//  "-optimize"
//  "-Xmacro-settings:-logging@org.enso.flexer.automata"
)

<<<<<<< HEAD
javacOptions in ThisBuild ++= Seq("-Xmx3G", "-Xss4M")
=======
javacOptions ++= Seq("-source", "12", "-target", "1.8")
>>>>>>> master

// Benchmark Configuration
lazy val Benchmark = config("bench") extend Test
lazy val bench     = taskKey[Unit]("Run Benchmarks")
lazy val benchOnly = inputKey[Unit]("Run benchmarks by name substring")
lazy val buildNativeImage =
  taskKey[Unit]("Build native image for the Enso executable")

// Global Project
lazy val enso = (project in file("."))
  .settings(version := "0.1")
  .aggregate(
    syntax,
    pkg,
    interpreter,
    projectManager,
    fileManager
  )

// Sub-Projects
<<<<<<< HEAD
lazy val logger = (project in file("lib/logger"))
=======
lazy val syntax = (project in file("Syntax"))
>>>>>>> master
  .settings(
    version := "0.1",
    scalacOptions += "-language:experimental.macros"
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect"  % "2.12.8",
      "org.scala-lang" % "scala-compiler" % "2.12.8"
    )
  )
  .dependsOn(unused)

lazy val flexer = (project in file("lib/flexer"))
  .settings(
    version := "0.1",
    scalacOptions += "-language:experimental.macros",
    scalacOptions -= "-deprecation",    // FIXME
    scalacOptions -= "-Xfatal-warnings" // FIXME
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect"  % "2.12.8",
      "org.scala-lang" % "scala-compiler" % "2.12.8",
      "org.feijoas"    %% "mango"         % "0.14"
    ),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
    )
  )
  .dependsOn(logger) //depends logger macro

lazy val unused = (project in file("lib/unused"))
  .settings(
    version := "0.1",
    scalacOptions += "-nowarn"
  )

lazy val syntax_definition = (project in file("syntax/definition"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"      %% "cats-core"     % "2.0.0-RC1",
      "com.lihaoyi"        %% "pprint"        % "0.5.3",
      "org.scala-lang"     % "scala-reflect"  % "2.12.8",
      "org.scala-lang"     % "scala-compiler" % "2.12.8",
      "org.feijoas"        %% "mango"         % "0.14",
      "org.apache.commons" % "commons-text"   % "1.6",
      "org.scalameta"      %% "scalameta"     % "4.2.0",
      "org.typelevel"      %% "kittens"       % "1.2.1",
      "com.lihaoyi"        %% "scalatags"     % "0.7.0"
    ),
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-law"   % monocleVersion % "test"
    ),
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at
      "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at
      "https://oss.sonatype.org/content/repositories/releases"
    ),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
    )
  )
  .dependsOn(logger)
  .dependsOn(flexer)

lazy val syntax = (project in file("syntax/specialization"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.syntax.text.Main"),
    version := "0.1",
    scalacOptions += "-Ypartial-unification"
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.storm-enroute"  %% "scalameter"    % "0.17" % "bench",
      "org.scalatest"      %% "scalatest"     % "3.0.5" % Test,
      "com.lihaoyi"        %% "pprint"        % "0.5.3",
      "org.scala-lang"     % "scala-reflect"  % "2.12.8",
      "org.scala-lang"     % "scala-compiler" % "2.12.8",
      "org.feijoas"        %% "mango"         % "0.14",
      "org.apache.commons" % "commons-text"   % "1.6",
      "org.scalameta"      %% "scalameta"     % "4.2.0",
      "org.typelevel"      %% "cats-core"     % "2.0.0-RC1"
    ),
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at
      "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at
      "https://oss.sonatype.org/content/repositories/releases"
    ),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
    ),
    (Compile / compile) := (Compile / compile)
      .dependsOn(Def.taskDyn {
        val parserCompile =
          (syntax_definition / Compile / compileIncremental).value
        if (parserCompile.hasModified) {
          Def.task {
            streams.value.log.info("Parser changed, forcing recompilation.")
            clean.value
          }
        } else Def.task {}
      })
      .value
  )
  .dependsOn(syntax_definition)
  .dependsOn(logger)
  .dependsOn(flexer)
  .configs(Test)
  .settings(
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    logBuffered := false
  )
  .configs(Benchmark)
  .settings(
    inConfig(Benchmark)(Defaults.testSettings),
    bench := (test in Benchmark).value,
    parallelExecution in Benchmark := false
  )

lazy val pkg = (project in file("Pkg"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.pkg.Main"),
    version := "0.1"
  )
  .settings(
    libraryDependencies ++= Seq("circe-core", "circe-generic", "circe-yaml")
      .map("io.circe" %% _ % "0.10.0"),
    libraryDependencies += "commons-io" % "commons-io" % "2.6"
  )

val truffleRunOptions = Seq(
  fork := true,
  javaOptions += s"-Dgraal.TruffleIterativePartialEscape=true",
  javaOptions += s"-XX:-UseJVMCIClassLoader",
  javaOptions += s"-Dgraal.TruffleBackgroundCompilation=false"
)

val jmh = Seq(
  "org.openjdk.jmh" % "jmh-core"                 % "1.21" % Benchmark,
  "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.21" % Benchmark
)

lazy val interpreter = (project in file("Interpreter"))
  .settings(
    mainClass in (Compile, run) := Some("org.enso.interpreter.Main"),
    version := "0.1"
  )
  .settings(commands += RunDebugCommand.runDebug)
  .settings(
    libraryDependencies ++= Seq(
<<<<<<< HEAD
      "com.chuusai"       %% "shapeless"  % "2.3.3",
      "com.storm-enroute" %% "scalameter" % "0.17" % "bench",
      "org.graalvm.sdk"   % "graal-sdk"   % "19.0.0",
      "org.scalacheck"    %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest"     %% "scalatest"  % "3.2.0-SNAP10" % Test,
      "org.typelevel"     %% "cats-core"  % "2.0.0-RC1"
=======
      "com.chuusai"            %% "shapeless"                % "2.3.3",
      "org.apache.commons"     % "commons-lang3"             % "3.9",
      "org.apache.tika"        % "tika-core"                 % "1.21",
      "org.graalvm.sdk"        % "graal-sdk"                 % "19.2.0",
      "org.graalvm.sdk"        % "polyglot-tck"              % "19.2.0",
      "org.graalvm.truffle"    % "truffle-api"               % "19.2.0",
      "org.graalvm.truffle"    % "truffle-dsl-processor"     % "19.2.0",
      "org.graalvm.truffle"    % "truffle-tck"               % "19.2.0",
      "org.graalvm.truffle"    % "truffle-tck-common"        % "19.2.0",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "org.scalacheck"         %% "scalacheck"               % "1.14.0" % Test,
      "org.scalactic"          %% "scalactic"                % "3.0.8" % Test,
      "org.scalatest"          %% "scalatest"                % "3.2.0-SNAP10" % Test,
      "org.typelevel"          %% "cats-core"                % "2.0.0-M4",
      "commons-cli"            % "commons-cli"               % "1.4"
    ),
    libraryDependencies ++= jmh
  )
  .settings(
    (Compile / javacOptions) ++= Seq(
      "-s",
      (Compile / sourceManaged).value.getAbsolutePath
>>>>>>> master
    )
  )
  .settings(
    (Compile / compile) := (Compile / compile)
      .dependsOn(Def.task { (Compile / sourceManaged).value.mkdirs })
      .value
  )
  .settings(
    inConfig(Compile)(truffleRunOptions),
    inConfig(Test)(truffleRunOptions),
    parallelExecution in Test := false,
    logBuffered in Test := false
  )
  .settings(
    buildNativeImage := Def
      .task {
        val javaHome        = System.getProperty("java.home")
        val nativeImagePath = s"$javaHome/bin/native-image"
        val classPath       = (Runtime / fullClasspath).value.files.mkString(":")
        val cmd =
          s"$nativeImagePath --macro:truffle --no-fallback --initialize-at-build-time -cp $classPath ${(Compile / mainClass).value.get} enso"
        cmd !
      }
      .dependsOn(Compile / compile)
      .value
  )
  .configs(Benchmark)
  .settings(
    logBuffered := false,
    inConfig(Benchmark)(Defaults.testSettings),
    inConfig(Benchmark)(truffleRunOptions),
    bench := (test in Benchmark).value,
    benchOnly := Def.inputTaskDyn {
      import complete.Parsers.spaceDelimited
      val name = spaceDelimited("<name>").parsed match {
        case List(name) => name
        case _          => throw new IllegalArgumentException("Expected one argument.")
      }
      Def.task {
        (testOnly in Benchmark).toTask(" -- -z " + name).value
      }
    }.evaluated,
    parallelExecution in Benchmark := false
  )

val akkaActor        = "com.typesafe.akka" %% "akka-actor"               % "2.5.23"
val akkaStream       = "com.typesafe.akka" %% "akka-stream"              % "2.5.23"
val akkaHttp         = "com.typesafe.akka" %% "akka-http"                % "10.1.8"
val akkaSpray        = "com.typesafe.akka" %% "akka-http-spray-json"     % "10.1.8"
val akkaTyped        = "com.typesafe.akka" %% "akka-actor-typed"         % "2.5.23"
val akkaTestkit      = "com.typesafe.akka" %% "akka-testkit"             % "2.5.23"
val akkaSLF4J        = "com.typesafe.akka" %% "akka-slf4j"               % "2.5.23"
val akkaTestkitTyped = "com.typesafe.akka" %% "akka-actor-testkit-typed" % "2.5.23" % Test

val akka = Seq(akkaActor, akkaStream, akkaHttp, akkaSpray, akkaTyped)

val circe = Seq("circe-core", "circe-generic", "circe-yaml").map(
  "io.circe" %% _ % "0.10.0"
)

lazy val fileManager = (project in file("FileManager"))
  .settings(
    (Compile / mainClass) := Some("org.enso.filemanager.FileManager")
  )
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies += akkaSLF4J,
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "org.scalatest"  %% "scalatest"      % "3.2.0-SNAP10" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck"     % "1.14.0" % Test,
    libraryDependencies += akkaTestkitTyped,
    libraryDependencies += "commons-io" % "commons-io"        % "2.6",
    libraryDependencies += "io.methvin" % "directory-watcher" % "0.9.6"
  )

lazy val projectManager = (project in file("ProjectManager"))
  .settings(
    (Compile / mainClass) := Some("org.enso.projectmanager.Server")
  )
  .settings(
    libraryDependencies ++= akka,
    libraryDependencies ++= circe,
    libraryDependencies += "io.spray" %% "spray-json" % "1.3.5"
  )
  .dependsOn(pkg)
