import Models.Grid

import cats.effect.IO
import Controllers.CliController
import cats.effect.unsafe.implicits.global
import scala.io.StdIn.readLine

/**
Using unsafeRunSync here because I wanted to take advantage of the Scala 3 parsing of params
*/
@main def main: Unit = summon[CliController[IO]].detectBoxes.unsafeRunSync()
