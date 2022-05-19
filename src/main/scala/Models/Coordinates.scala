package Models

import cats.implicits._
import cats.kernel.Order

final case class Coordinates(rowCoordinate: RowCoordinate, columnCoordinate: ColumnCoordinate)

object Coordinates {
  extension (c: Coordinates) {
    def up = c.rowCoordinate.upCoordinate.map(newRowColumn => c.copy(rowCoordinate = newRowColumn))
    def down = c.copy(rowCoordinate = c.rowCoordinate.downCoordinate)
    def left = c.columnCoordinate.leftCoordinate.map(newColumn => c.copy(columnCoordinate = newColumn))
    def right = c.copy(columnCoordinate = c.columnCoordinate.rightCoordinate)
  }
  
  given Order[Coordinates] = Order.from{(coord1, coord2) =>
    val rowDiff = (coord1.rowCoordinate compare coord2.rowCoordinate)
    lazy val columnDiff = coord1.columnCoordinate compare coord2.columnCoordinate

    if (rowDiff == 0) columnDiff else {
      if (rowDiff > 0) 1 else 0
    }
  }

  given Ordering[Coordinates] = summon[Order[Coordinates]].toOrdering
}
