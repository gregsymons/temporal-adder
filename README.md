TemporalAdder
=============

*A simple class for adding offsets to times*

What is this?
-------------

It's mostly an exercise in using Scala parser combinators to parse a very
specific time format out of a string. I wrote this as a coding exercise for a
potential employer, but thought the solution was worth keeping.

Don't use this for, well, anything. But if you want to, it's licensed under the
Apache license. I won't be publishing binary packages of it, so don't ask.

Getting Started
---------------

### Prerequisites ###

* Build and Unit Test
  * A Java 8 JDK install (tested with OpenJDK 1.8.0_121)

### Building ###

This repository includes a copy of Paul Phillips's `sbt-extras` sbt launcher, so
you don't have to have sbt itself installed. Instead, just invoke the `sbt`
script in this directory, and an appropriate version of sbt will be downloaded
automatically and run. If you've already downloaded sbt using the script, it
will be cached.

To build the app and run all the unit tests run `./sbt test` in the project
directory.
