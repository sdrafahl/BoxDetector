package Models

import cats.implicits._
import cats.kernel.Order
import cats.kernel.Eq
import cats.implicits._
import eu.timepit.refined.types.numeric.PosInt
import cats.Show

final case class Coordinates(rowCoordinate: RowCoordinate, columnCoordinate: ColumnCoordinate)

object Coordinates {
  extension (c: Coordinates) {
    def up = c.rowCoordinate.upCoordinate.map(newRowColumn => c.copy(rowCoordinate = newRowColumn))
    def down = c.copy(rowCoordinate = c.rowCoordinate.downCoordinate)
    def left = c.columnCoordinate.leftCoordinate.map(newColumn => c.copy(columnCoordinate = newColumn))
    def right = c.copy(columnCoordinate = c.columnCoordinate.rightCoordinate)

    def isRightOf(other: Coordinates): Boolean = c.columnCoordinate.compare(other.columnCoordinate) > 0
    def isBottomOf(other: Coordinates): Boolean = c.rowCoordinate.compare(other.rowCoordinate) > 0
    def isLeftOf(other: Coordinates): Boolean = c.columnCoordinate.compare(other.columnCoordinate) < 0
    def isTopOf(other: Coordinates): Boolean = c.rowCoordinate.compare(other.rowCoordinate) < 0
    def isSame(other: Coordinates): Boolean = c === other

    def isTopLeftOf(other: Coordinates): Boolean = c.isTopOf(other) && c.isLeftOf(other)
    def isDirectlyLeft(other: Coordinates): Boolean = c.isLeftOf(other) && c.rowCoordinate === other.rowCoordinate
    def isDirectlyTop(other: Coordinates): Boolean = c.isTopOf(other) && c.columnCoordinate === other.columnCoordinate

    def downBy(x: Int): Coordinates = c.copy(rowCoordinate = c.rowCoordinate.downBy(x))
    def rightBy(x: Int): Coordinates = c.copy(columnCoordinate = c.columnCoordinate.rightBy(x))
  }

  given Show[Coordinates] = Show.show(coords => s"(${coords.rowCoordinate.show}, ${coords.columnCoordinate.show})")
  given Order[Coordinates] = Order.from{(coord1, coord2) =>
    val rowDiff = (coord1.rowCoordinate compare coord2.rowCoordinate)
    lazy val columnDiff = coord1.columnCoordinate compare coord2.columnCoordinate

    if (rowDiff == 0) columnDiff else {
      rowDiff
    }
  }

  given Eq[Coordinates] = Eq.instance((coord1, coord2) => coord1.rowCoordinate === coord2.rowCoordinate && coord1.columnCoordinate === coord2.columnCoordinate)
  given Ordering[Coordinates] = summon[Order[Coordinates]].toOrdering
}
