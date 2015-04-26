## Tips

Loading a script on repl:

scala> :load src/main/scala/progscala2/introscala/upper1.sc

Compiling and running a main program:

scalac src/main/scala/progscala2/introscala/upper1.scala
scala -cp . progscala2.introscala.Upper Hello World!

or alternatively from the sbt:

run-main progscala2.introscala.Upper Hello World!
