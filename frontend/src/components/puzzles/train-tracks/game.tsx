import { TrainTracks as ServerTrainTracks } from "../../../api-hooks";
import { range } from "./utils";
import { clearScreenDown } from "readline";
const scc = require("strongly-connected-components");

export enum Direction {
  Unknown = "Unknown",
  Horizontal = "Horizontal",
  Vertical = "Vertical",
  DownLeft = "DownLeft",
  DownRight = "DownRight",
  UpLeft = "UpLeft",
  UpRight = "UpRight",
  Empty = "Empty"
}

export interface PlacedCell {
  direction: Direction;
  position: GridPosition;
}

export interface GridPosition {
  col: number;
  row: number;
}

export interface GridSize {
  rows: number;
  cols: number;
}

export interface TrainTracks {
  gridSize: GridSize;
  fixedCells: PlacedCell[];
  colCounts: number[];
  rowCounts: number[];
}

export const parseDirection = (direction: string): Direction => {
  const mapping: { [key: string]: Direction } = {
    Unknown: Direction.Unknown,
    Horizontal: Direction.Horizontal,
    Vertical: Direction.Vertical,
    DownLeft: Direction.DownLeft,
    DownRight: Direction.DownRight,
    UpLeft: Direction.UpLeft,
    UpRight: Direction.UpRight,
    Empty: Direction.Empty
  };
  return mapping[direction];
};

export const parseTrainTracks = (
  serverTrainTracks: ServerTrainTracks
): TrainTracks => {
  const parsedFixedCells = serverTrainTracks._fixedCells.map(fixedCell => {
    return {
      direction: parseDirection(fixedCell._direction),
      position: {
        row: fixedCell._position._row,
        col: fixedCell._position._col
      }
    };
  });
  return {
    gridSize: {
      rows: serverTrainTracks._gridSize._rows,
      cols: serverTrainTracks._gridSize._cols
    },
    colCounts: serverTrainTracks._colCounts,
    rowCounts: serverTrainTracks._rowCounts,
    fixedCells: parsedFixedCells
  };
};

const makeToggleMapping = (directionOrdering: Direction[]) => {
  return (direction: Direction): Direction => {
    for (let i = 0; i < directionOrdering.length; i++) {
      const currentDirection = directionOrdering[i];
      if (currentDirection === direction) {
        const nextDirection =
          directionOrdering[(i + 1) % directionOrdering.length];
        return nextDirection;
      }
    }
    return direction;
  };
};

export const toggleDirection = makeToggleMapping([
  Direction.Unknown,
  Direction.Horizontal,
  Direction.Vertical,
  Direction.DownLeft,
  Direction.UpLeft,
  Direction.UpRight,
  Direction.DownRight,
  Direction.Empty
]);

const isBuilt = (direction: Direction): Boolean => {
  return (
    direction === Direction.Horizontal ||
    direction === Direction.Vertical ||
    direction === Direction.DownLeft ||
    direction === Direction.DownRight ||
    direction === Direction.UpLeft ||
    direction === Direction.UpRight
  );
};

export const countBuilt = (cells: Direction[]): number => {
  return cells.filter(cell => {
    return isBuilt(cell);
  }).length;
};

enum GridDirection {
  Left = "Left",
  Right = "Right",
  Up = "Up",
  Down = "Down"
}

const directionMaps = [
  [GridDirection.Up, Direction.DownLeft, GridDirection.Left],
  [GridDirection.Right, Direction.DownLeft, GridDirection.Down],

  [GridDirection.Up, Direction.DownRight, GridDirection.Right],
  [GridDirection.Left, Direction.DownRight, GridDirection.Down],

  [GridDirection.Down, Direction.UpLeft, GridDirection.Left],
  [GridDirection.Right, Direction.UpLeft, GridDirection.Up],

  [GridDirection.Down, Direction.UpRight, GridDirection.Right],
  [GridDirection.Left, Direction.UpRight, GridDirection.Up],

  [GridDirection.Down, Direction.Vertical, GridDirection.Down],
  [GridDirection.Up, Direction.Vertical, GridDirection.Up],

  [GridDirection.Left, Direction.Horizontal, GridDirection.Left],
  [GridDirection.Right, Direction.Horizontal, GridDirection.Right]
] as [GridDirection, Direction, GridDirection][];

const directionMap = (
  entryDirection: GridDirection,
  pieceDirection: Direction
): GridDirection | undefined => {
  const result = directionMaps.find(([from, piece, to]) => {
    return from === entryDirection && piece === pieceDirection;
  });
  if (result !== undefined) {
    const [f, p, to] = result;
    return to;
  }
  return undefined;
};

const gridDirectionToDelta = (
  gridDirection: GridDirection
): [number, number] => {
  switch (gridDirection) {
    case GridDirection.Up:
      return [-1, 0];
    case GridDirection.Down:
      return [1, 0];
    case GridDirection.Left:
      return [0, -1];
    case GridDirection.Right:
      return [0, 1];
  }
};

const getInDirectionsForCell = (
  startingCellValue: Direction
): [GridDirection, GridDirection] => {
  const relevantDirectionMaps = directionMaps.filter(
    ([inDir, piece, outDir]) => {
      return piece === startingCellValue;
    }
  );
  return [relevantDirectionMaps[0][0], relevantDirectionMaps[1][0]];
};

export const isSinglePath = (
  rows: number,
  cols: number,
  cells: Direction[][],
  [startingRow, startingCol]: [number, number]
): Boolean => {
  const cellSpan = calculateCellSpan(rows, cols, cells, [
    startingRow,
    startingCol
  ]);
  const numberOfBuiltCells = countBuilt(cells.flat());
  console.log(cellSpan, numberOfBuiltCells);
  return cellSpan === numberOfBuiltCells;
};

const calculateCellSpan = (
  rows: number,
  cols: number,
  cells: Direction[][],
  [startingRow, startingCol]: [number, number]
): number => {
  const startingCellValue = cells[startingRow][startingCol];
  const [dir1, dir2] = getInDirectionsForCell(startingCellValue);
  const path1Length = lengthOfPath(rows, cols, cells, dir1, [
    startingRow,
    startingCol
  ]);
  const path2Length = lengthOfPath(rows, cols, cells, dir2, [
    startingRow,
    startingCol
  ]);
  return path1Length + path2Length - 1;
};

const lengthOfPath = (
  rows: number,
  cols: number,
  cells: Direction[][],
  inGridDirection: GridDirection,
  [row, col]: [number, number]
): number => {
  const currentPiece = cells[row][col];
  if (!isBuilt(currentPiece)) {
    return 0;
  }
  const nextDirection = directionMap(inGridDirection, currentPiece);
  if (nextDirection === undefined) {
    return 0;
  }
  const [rowD, colD] = gridDirectionToDelta(nextDirection);
  const [nextRow, nextCol] = [row + rowD, col + colD];
  if (nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol > cols) {
    return 1;
  }
  return 1 + lengthOfPath(rows, cols, cells, nextDirection, [nextRow, nextCol]);
};
