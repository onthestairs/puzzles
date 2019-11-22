import { useState, useEffect } from "react";

import { range, updateListAt } from "./utils";
import { Direction, toggleDirection } from "./game";

export const useTrainTracksCells = (rows: number, cols: number) => {
  const initialState: Direction[] = range(rows * cols).map(i => {
    return Direction.Unknown;
  });
  const rowAndColToIndex = (row: number, col: number) => {
    return row * cols + col;
  };
  const [cellStates, setCellStates] = useState(initialState);
  const getCellState = (row: number, col: number): Direction => {
    return cellStates[rowAndColToIndex(row, col)];
  };
  const setCellState = (row: number, col: number, direction: Direction) => {
    const newCellStates = updateListAt(
      cellStates,
      rowAndColToIndex(row, col),
      direction
    );
    setCellStates(newCellStates);
  };
  const toggleCellState = (row: number, col: number) => {
    const currentDirection = getCellState(row, col);
    setCellState(row, col, toggleDirection(currentDirection));
  };
  return {
    getCellState,
    toggleCellState
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
  });
  return secondsElapsed;
};

export const useTrainTracksGame = (rows: number, cols: number) => {
  const { getCellState, toggleCellState } = useTrainTracksCells(rows, cols);
  const elapsedSeconds = useSecondsTimer();
  return {
    cells: { getCellState, toggleCellState },
    elapsedSeconds
  };
};
