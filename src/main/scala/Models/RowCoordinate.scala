package Models

import eu.timepit.refined.auto.autoUnwrap
import eu.timepit.refined.types.numeric.PosInt
import cats.implicits._
import cats.kernel.Order


opaque type RowCoordinate = PosInt

object RowCoordinate {
  extension (cc: RowCoordinate) {
    def upCoordinate: Either[NonPositiveRowError, RowCoordinate] = PosInt.from(cc - 1).left.map(_ => NonPositiveRowError())
    def downCoordinate: RowCoordinate = PosInt.unsafeFrom(cc + 1) // Adding a positive Int to a positive int Will always be a positive Int
    def isLessThan(other: RowCoordinate): Boolean = cc < other
  }

  given Order[RowCoordinate] = Order.from((a, b) => a - b)
}

final case class NonPositiveRowError() extends Throwable

