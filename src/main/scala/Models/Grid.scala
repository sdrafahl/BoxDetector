package Models

import cats.implicits._
import cats.data.NonEmptySet
import cats.data.NonEmptyList

final case class Grid(
    lastRowCoordinate: RowCoordinate,
    lastColumnCoordinate: ColumnCoordinate,
    nonEmptyCoords: Set[Coordinates]
)

object Grid {
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

    def search(coords: Coordinates): Space = {
      val isWithinGrid = coords.rowCoordinate.isLessThan(
        grid.lastRowCoordinate
      ) && coords.columnCoordinate.isLessThan(grid.lastColumnCoordinate)
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
