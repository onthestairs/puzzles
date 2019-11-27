import React from "react";

import { range } from "./utils";
import Cell from "./Cell";
import TrackCounter from "./TrackCounter";
import Grid from "../../generic/Grid";

const TrainTracksGrid = ({
  rows,
  cols,
  getCellState,
  toggleCellState,
  counts: {
    rows: { actual: actualRowCounts, desired: desiredRowCounts },
    cols: { actual: actualColCounts, desired: desiredColCounts }
  },
  isComplete: isComplete
}: {
  rows: number;
  cols: number;
  getCellState: any;
  toggleCellState: any;
  counts: {
    rows: {
      actual: number[];
      desired: number[];
    };
    cols: {
      actual: number[];
      desired: number[];
    };
  };
  isComplete: Boolean;
}) => {
  const cells = range(rows).flatMap(row => {
    return range(cols).map(col => {
      return (
        <Cell
          row={row}
          col={col}
          key={`cell${row * cols + col}`}
          value={getCellState(row, col)}
          toggleValue={() => toggleCellState(row, col)}
        />
      );
    });
  });

  const rowCounters = actualRowCounts.map((count, i) => {
    const desiredCount = desiredRowCounts[i];
    return (
      <TrackCounter
        key={`rowCounter${i}`}
        currentCount={count}
        desiredCount={desiredCount}
      />
    );
  });
  const colCounters = actualColCounts.map((count, i) => {
    const desiredCount = desiredColCounts[i];
    return (
      <TrackCounter
        key={`colCounter${i}`}
        currentCount={count}
        desiredCount={desiredCount}
      />
    );
  });

  const structuredCells = range(rows).map(row => {
    return range(cols).map(col => {
      return cells[row * cols + col];
    });
  });

  const cellsWithRowCounters = structuredCells.map((rowCells, rowNumber) => {
    return [...rowCells, rowCounters[rowNumber]];
  });

  const cellsWithRowAndColCounters = [...cellsWithRowCounters, colCounters];

  return (
    <Grid
      isComplete={isComplete}
      width="500px"
      height="500px"
      cells={cellsWithRowAndColCounters}
    ></Grid>
  );
};

export default TrainTracksGrid;
