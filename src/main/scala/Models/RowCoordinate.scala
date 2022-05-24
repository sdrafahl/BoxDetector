package Models

import eu.timepit.refined.auto.autoUnwrap
import eu.timepit.refined.types.numeric.PosInt
import cats.implicits._
import cats.kernel.Order
import cats.kernel.Eq
import cats.Show


opaque type RowCoordinate = PosInt

object RowCoordinate {
  def apply(x: PosInt): RowCoordinate = x

  extension (cc: RowCoordinate) {
    def upCoordinate: Either[NonPositiveRowError, RowCoordinate] = PosInt.from(cc - 1).left.map(_ => NonPositiveRowError())
    def downCoordinate: RowCoordinate = PosInt.unsafeFrom(cc + 1) // Adding a positive Int to a positive int Will always be a positive Int
    def isLessThanOrEqual(other: RowCoordinate): Boolean = cc <= other
    def isLessThan(other: RowCoordinate): Boolean = cc < other
    def downBy(x: Int): RowCoordinate = PosInt.from(x).map(pos => pos + cc).map(PosInt.unsafeFrom(_)).getOrElse(cc)
    def toInt: Int = cc
  }

  given Show[RowCoordinate] = Show.show(coord => coord.toString)
  given Eq[RowCoordinate] = Eq.fromUniversalEquals
  given Order[RowCoordinate] = Order.from((a, b) => a - b)
}

final case class NonPositiveRowError() extends Throwable

