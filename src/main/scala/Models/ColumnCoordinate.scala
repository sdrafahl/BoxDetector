package Models

import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.auto.autoUnwrap
import cats.kernel.Order
import cats.kernel.Eq
import cats.Show

opaque type ColumnCoordinate = PosInt

object ColumnCoordinate {

  def apply(x: PosInt): ColumnCoordinate = x

  extension (cc: ColumnCoordinate) {
    def leftCoordinate: Either[NonPositiveColumnError, ColumnCoordinate] = PosInt.from(cc - 1).left.map(_ => NonPositiveColumnError())
    def rightCoordinate: ColumnCoordinate = PosInt.unsafeFrom(cc + 1) // Adding a positive Int to a positive int Will always be a positive Int
    def isLessThan(other: ColumnCoordinate): Boolean = cc < other
    def isLessThanOrEqual(other: ColumnCoordinate): Boolean = cc <= other
    def rightBy(x: Int): ColumnCoordinate = PosInt.from(x).map(pos => pos + cc).map(PosInt.unsafeFrom(_)).getOrElse(cc)
    def toInt: Int = cc
  }

  given Show[ColumnCoordinate] = Show.show(coord => coord.toString)
  given Order[ColumnCoordinate] = Order.from((a, b) => a - b)
  given Eq[ColumnCoordinate] = Eq.fromUniversalEquals
}

final case class NonPositiveColumnError() extends Throwable
