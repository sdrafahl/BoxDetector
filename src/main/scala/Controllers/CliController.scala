package Controllers

import cats.implicits._
import cats.effect._
import Models.Grid
import scala.io.StdIn.readLine
import scala.util.CommandLineParser.FromString

abstract class CliController[F[_]] {
  def detectBoxes: F[Unit]
}

object CliController {
  given [F[_]: Sync]: CliController[F] = new CliController[F] {
    override def detectBoxes: F[Unit] = {
      for {
        allStdIn <- readAllLines("")
        grid <- Sync[F].delay(summon[FromString[Grid]].fromString(allStdIn))
        _ <- grid.getSetOfNonOverlappingBoxes.toList.traverse(b => Sync[F].delay(println(b.show))) >> Sync[F].unit    
      } yield ()      
    }
  }

  private[this] def readAllLines[F[_]: Sync](acc: String): F[String] = {
    for {
      maybeNextLine <- Sync[F].delay(readLine()).map(Option(_))
      maybeResult <- maybeNextLine.traverse(line => readAllLines(acc ++ "T" ++ line))
    } yield maybeResult.getOrElse(acc.tail ++ "T")
  }
}
