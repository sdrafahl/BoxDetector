package Models

import cats.implicits._
import cats.data.NonEmptySet
import cats.data.NonEmptyList
import scala.util.CommandLineParser.FromString
import eu.timepit.refined.types.numeric.PosInt
import cats.implicits._

final case class Grid(
    lastRowCoordinate: RowCoordinate,
    lastColumnCoordinate: ColumnCoordinate,
    nonEmptyCoords: Set[Coordinates]
)

object Grid {

  given FromString[Grid] = new FromString[Grid] {
    override def fromString(s: String): Grid = {
      val rows = s.split("T")
      val lastRowCoordinate = RowCoordinate(PosInt.unsafeFrom(rows.length))
      val width = rows.head.length
      val gridWidth = ColumnCoordinate(PosInt.unsafeFrom(width))
      if (rows.toList.exists(_.length != width)) throw IllegalArgumentException("Grid is not in expected form") else ()
      val rowAndCoordinate: List[(String, RowCoordinate)] = rows.toList.mapWithIndex((row, index) => (row, RowCoordinate(PosInt.unsafeFrom(index + 1))))
      val columnsAndRowsOnlyUsedSpace: List[(RowCoordinate, List[(Char, ColumnCoordinate)])] = rowAndCoordinate
        .map(rowAndCoordinate => (rowAndCoordinate._2, rowAndCoordinate._1.toCharArray.toList.mapWithIndex((ch, indexOfColumn) => (ch, ColumnCoordinate(PosInt.unsafeFrom(indexOfColumn + 1)))).filter((ch, _) => ch == '*')))

      val occupiedSpace = columnsAndRowsOnlyUsedSpace.foldLeft(Set.empty[Coordinates]){(setOfCoords, row) =>
        setOfCoords ++ row._2.foldLeft(Set.empty[Coordinates])((setOfCoords, charAndIndex) => setOfCoords ++ Set(Coordinates(row._1, charAndIndex._2)))
      }

      Grid(lastRowCoordinate, gridWidth, occupiedSpace)
    }
  }

  extension (grid: Grid) {
    def getSetOfBoxes: Set[Box] = {
      grid.nonEmptyCoords
        .groupBy(bfsSearchForContiguousNonEmptySpaceGroup)
        .values
        .toList
        .map(set => NonEmptyList.fromList(set.toList))
        .collect {
          case Some(nonEmptyListOfCoords) => {
            val sortedCoords = nonEmptyListOfCoords.sorted
            val topLeftCoord = sortedCoords.head
            val bottomRightCoord = sortedCoords.last
            Box(topLeftCoord, bottomRightCoord)
          }
        }
        .toSet
    }

    def getSetOfNonOverlappingBoxes: Set[Box] = {
      val setOfBoxes = grid.getSetOfBoxes
      setOfBoxes.filterNot(box => setOfBoxes.find(anyOtherBox => box != anyOtherBox && box.overlaps(anyOtherBox)).isDefined)     
    }

    def search(coords: Coordinates): Space = {
      val isWithinGrid = coords.rowCoordinate.isLessThanOrEqual(
        grid.lastRowCoordinate
      ) && coords.columnCoordinate.isLessThanOrEqual(grid.lastColumnCoordinate)
      lazy val isoccupied = grid.nonEmptyCoords.contains(coords)
      if (isWithinGrid) {
        if (isoccupied) Space.Occupied else Space.Empty
      } else {
        Space.InvalidSpace
      }
    }

    def bfsSearchForContiguousNonEmptySpaceGroup(
        coordinate: Coordinates
    ): Set[Coordinates] = recursiveBFSSearch(
      DFSContext(Set.empty, Set(coordinate))
    ).visitedCoords

    private[this] def itterateOverNeighbors(
        dfsContext: DFSContext
    ): DFSContext = {
      val allValidNeighbors =
        dfsContext.seenCoords.foldLeft(Set.empty[Coordinates]) {
          (coordinateAccNeighbors, coordinate) =>
            val upCoordinate = coordinate.up
              .map(coordinate => (grid.search(coordinate), Some(coordinate)))
              .getOrElse((Space.InvalidSpace, None))
            val rightCoordinate =
              (grid.search(coordinate.right), Some(coordinate.right))
            val downCoordinate =
              (grid.search(coordinate.down), Some(coordinate.down))
            val leftCoordinate = coordinate.left
              .map(coordinate => (grid.search(coordinate), Some(coordinate)))
              .getOrElse((Space.InvalidSpace, None))

            val validNeighborCoords = (List(
              upCoordinate,
              rightCoordinate,
              downCoordinate,
              leftCoordinate
            ).collect { case (Space.Occupied, Some(coord)) =>
              coord
            }).toSet

            coordinateAccNeighbors ++ validNeighborCoords
        }

      val nonNewNeighbors = allValidNeighbors.filter(neigbor =>
        !dfsContext.seenCoords.contains(neigbor) && !dfsContext.visitedCoords
          .contains(neigbor)
      )

      DFSContext(
        dfsContext.visitedCoords ++ dfsContext.seenCoords,
        nonNewNeighbors
      )
    }

    private[this] def recursiveBFSSearch(dfsContext: DFSContext): DFSContext = {
      if (dfsContext.seenCoords.isEmpty) {
        dfsContext
      } else {
        val nextState = itterateOverNeighbors(dfsContext)
        recursiveBFSSearch(nextState)
      }
    }

  }

  private[this] case class DFSContext(
      visitedCoords: Set[Coordinates],
      seenCoords: Set[Coordinates]
  )
}
