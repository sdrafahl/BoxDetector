package Models

import scala.language.postfixOps
import eu.timepit.refined.auto.autoUnwrap
import cats.kernel.Eq
import cats.implicits._
import cats.Show

final case class Box(topLeftCoordinate: Coordinates, bottomRightCoordinate: Coordinates)

object Box {
  extension (b: Box) {
    def perimeterCoords: Set[Coordinates] = {
      val height = b.bottomRightCoordinate.rowCoordinate.compare(b.topLeftCoordinate.rowCoordinate)
      val width = b.bottomRightCoordinate.columnCoordinate.compare(b.topLeftCoordinate.columnCoordinate)

      val bottomLeftCoord = b.topLeftCoordinate.downBy(height)
      val topRightCoord = b.topLeftCoordinate.rightBy(width)

      val leftCoords = ((1 to height).foldLeft((Set(b.topLeftCoordinate), b.topLeftCoordinate))((coordsAndCursor, _) => (coordsAndCursor._1 ++ Set(coordsAndCursor._2.down), coordsAndCursor._2.down)))._1
      val bottomCoords = ((1 to width).foldLeft((Set(bottomLeftCoord), bottomLeftCoord))((coordsAndCursor, _) => (coordsAndCursor._1 ++ Set(coordsAndCursor._2.right), coordsAndCursor._2.right)))._1
      val rightCoords = ((1 to height).foldLeft((Set(topRightCoord), topRightCoord))((coordsAndCursor, _) => (coordsAndCursor._1 ++ Set(coordsAndCursor._2.down), coordsAndCursor._2.down)))._1
      val topCoords = ((1 to width).foldLeft((Set(b.topLeftCoordinate), b.topLeftCoordinate))((coordsAndCursor, _) => (coordsAndCursor._1 ++ Set(coordsAndCursor._2.right), coordsAndCursor._2.right)))._1

      leftCoords ++ bottomCoords ++ rightCoords ++ topCoords
    }

    def isContainedEntirelyWithin(container: Box): Boolean = {
      val topLeftIsContained: Boolean =
        (container.topLeftCoordinate === b.topLeftCoordinate ||
          container.topLeftCoordinate.isTopLeftOf(b.topLeftCoordinate) ||
          container.topLeftCoordinate.isDirectlyLeft(b.topLeftCoordinate) ||
          container.topLeftCoordinate.isDirectlyTop(b.topLeftCoordinate))

      val bottomRightIsContained: Boolean = (
        b.bottomRightCoordinate === container.bottomRightCoordinate ||
        b.bottomRightCoordinate.isTopLeftOf(container.topLeftCoordinate) ||
          b.bottomRightCoordinate.isDirectlyLeft(container.topLeftCoordinate) ||
          b.bottomRightCoordinate.isDirectlyTop(container.topLeftCoordinate)
      )
      
      topLeftIsContained && bottomRightIsContained
    }

    def overlaps(other: Box): Boolean = {
      lazy val perimiter1 = b.perimeterCoords
      lazy val perimiter2 = other.perimeterCoords
      lazy val coordsIntersect = perimiter1.intersect(perimiter2).nonEmpty
      (
        other === b ||
        b.isContainedEntirelyWithin(other) ||
        other.isContainedEntirelyWithin(b) ||
        coordsIntersect
      )
    }
    
  }

  given Show[Box] = Show.show(b => s"${b.topLeftCoordinate.show}${b.bottomRightCoordinate.show}")
  given Eq[Box] = Eq.fromUniversalEquals
}
