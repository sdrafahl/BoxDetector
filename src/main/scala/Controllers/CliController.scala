package Controllers

import cats.implicits._
import cats.effect._
import Models.Grid

abstract class CliController[F[_]] {
  def detectBoxes(grid: Grid): F[Unit]
}

object CliController {
  given [F[_]: Sync]: CliController[F] = new CliController[F] {
    override def detectBoxes(grid: Grid): F[Unit] = grid.getSetOfNonOverlappingBoxes.toList.traverse(b => Sync[F].delay(println(b.show))) >> Sync[F].unit 
  }
}
