import { useState, useEffect, useMemo } from "react";

import { range, updateNestedListAt, areArraysEqual } from "./utils";
import {
  Direction,
  toggleDirection,
  countBuilt,
  PlacedCell,
  // isFullyConnected,
  isSinglePath
} from "./game";

const getRows = (xs: any[][], rows: number, cols: number): any[][] => {
  return xs;
};

const getCols = (xs: any[][], rows: number, cols: number): any[][] => {
  return range(cols).map(col => {
    return range(rows).map(row => {
      return xs[row][col];
    });
  });
};

const getRowAndColumnCounts = (
  rows: number,
  cols: number,
  cellValues: Direction[][]
): { rowCounts: number[]; colCounts: number[] } => {
  const rowCounts = getRows(cellValues, rows, cols).map(cells =>
    countBuilt(cells)
  );
  const colCounts = getCols(cellValues, rows, cols).map(cells =>
    countBuilt(cells)
  );
  return {
    rowCounts,
    colCounts
  };
};

const makeFixedCellsMap = (
  fixedCells: PlacedCell[]
): ((row: number, col: number) => Direction | null) => {
  return (row: number, col: number) => {
    const maybePlacedCell = fixedCells.find(fixedCell => {
      return fixedCell.position.col === col && fixedCell.position.row === row;
    });
    if (maybePlacedCell === undefined) {
      return null;
    }
    return maybePlacedCell.direction;
  };
};

export const useTrainTracksCells = (
  rows: number,
  cols: number,
  fixedCells: PlacedCell[],
  {
    rows: desiredRowCounts,
    cols: desiredColCounts
  }: { rows: number[]; cols: number[] }
) => {
  const fixedCellsMap = makeFixedCellsMap(fixedCells);
  const initialState: Direction[][] = range(rows).map(row => {
    return range(cols).map(col => {
      const maybeFixedCell = fixedCellsMap(row, col);
      if (maybeFixedCell != null) {
        return maybeFixedCell;
      }
      return Direction.Unknown;
    });
  });
  const [cellStates, setCellStates] = useState(initialState);
  const getCellState = (row: number, col: number): Direction => {
    return cellStates[row][col];
  };
  const setCellState = (row: number, col: number, direction: Direction) => {
    const maybeFixedCell = fixedCellsMap(row, col);
    // dont change the cell if it's 'fixed'
    if (maybeFixedCell !== null) {
      return;
    }
    const newCellStates = updateNestedListAt(cellStates, row, col, direction);
    setCellStates(newCellStates);
  };
  const toggleCellState = (row: number, col: number) => {
    const currentDirection = getCellState(row, col);
    setCellState(row, col, toggleDirection(currentDirection));
  };

  const { rowCounts, colCounts, isComplete } = useMemo(() => {
    const { rowCounts, colCounts } = getRowAndColumnCounts(
      rows,
      cols,
      cellStates
    );
    const { row: startingRow, col: startingCol } = fixedCells[0].position;
    const singlePath = isSinglePath(rows, cols, cellStates, [
      startingRow,
      startingCol
    ]);
    const isComplete =
      singlePath &&
      areArraysEqual(rowCounts, desiredRowCounts) &&
      areArraysEqual(colCounts, desiredColCounts);
    return {
      rowCounts,
      colCounts,
      isComplete
    };
  }, [rows, cols, cellStates]);
  return {
    getCellState,
    toggleCellState,
    rowCounts,
    colCounts,
    isComplete
  };
};

export const useSecondsTimer = () => {
  const [secondsElapsed, setSecondsElapsed] = useState(0);
  useEffect(() => {
    const interval = setInterval(() => {
      setSecondsElapsed(s => s + 1);
    }, 1000);
    return () => {
      clearInterval(interval);
    };
  }, []);
  return secondsElapsed;
};

export const useTrainTracksGame = (
  rows: number,
  cols: number,
  fixedCells: PlacedCell[],
  desiredCounts: { rows: number[]; cols: number[] }
) => {
  const {
    getCellState,
    toggleCellState,
    rowCounts,
    colCounts,
    isComplete
  } = useTrainTracksCells(rows, cols, fixedCells, desiredCounts);
  const elapsedSeconds = useSecondsTimer();
  return {
    cells: { getCellState, toggleCellState, rowCounts, colCounts, isComplete },
    elapsedSeconds
  };
};
