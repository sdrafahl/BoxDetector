package Models

import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.auto.autoUnwrap
import cats.kernel.Order

opaque type ColumnCoordinate = PosInt

object ColumnCoordinate {
  extension (cc: ColumnCoordinate) {
    def leftCoordinate: Either[NonPositiveColumnError, ColumnCoordinate] = PosInt.from(cc - 1).left.map(_ => NonPositiveColumnError())
    def rightCoordinate: ColumnCoordinate = PosInt.unsafeFrom(cc + 1) // Adding a positive Int to a positive int Will always be a positive Int
    def isLessThan(other: ColumnCoordinate): Boolean = cc < other
  }

  given Order[ColumnCoordinate] = Order.from((a, b) => a - b)

}

final case class NonPositiveColumnError() extends Throwable
