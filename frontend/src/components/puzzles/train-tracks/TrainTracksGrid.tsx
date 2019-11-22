import React from "react";

import { range } from "./utils";
import Cell from "./Cell";
import { Direction } from "./game";
import Grid from "../../generic/Grid";

const TrainTracksGrid = ({
  rows,
  cols,
  getCellState,
  toggleCellState
}: {
  rows: number;
  cols: number;
  getCellState: any;
  toggleCellState: any;
}) => {
  const cells = range(rows).flatMap(row => {
    return range(cols).map(col => {
      return (
        <Cell
          row={row}
          col={col}
          value={getCellState(row, col)}
          toggleValue={() => toggleCellState(row, col)}
        />
      );
    });
  });

  return (
    <Grid rows={rows} cols={cols} width="500px" height="500px">
      {cells}
    </Grid>
  );
};

export default TrainTracksGrid;
