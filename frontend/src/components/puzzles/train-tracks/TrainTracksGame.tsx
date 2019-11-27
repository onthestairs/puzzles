import React from "react";

import { TrainTracks } from "./game";
import Grid from "../../generic/Grid";
import TrainTracksGrid from "./TrainTracksGrid";
import { useTrainTracksGame } from "./gameHooks";

const TrainTracksGame = ({ trainTracks }: { trainTracks: TrainTracks }) => {
  const rows = trainTracks.gridSize.rows;
  const cols = trainTracks.gridSize.cols;
  const fixedCells = trainTracks.fixedCells;
  const desiredCounts = {
    rows: trainTracks.rowCounts,
    cols: trainTracks.colCounts
  };
  const {
    elapsedSeconds,
    cells: { getCellState, toggleCellState, rowCounts, colCounts, isComplete }
  } = useTrainTracksGame(rows, cols, fixedCells, desiredCounts);
  const counts = {
    rows: {
      actual: rowCounts,
      desired: trainTracks.rowCounts
    },
    cols: {
      actual: colCounts,
      desired: trainTracks.colCounts
    }
  };

  return (
    <div>
      <em>{elapsedSeconds} elapsed</em>
      <TrainTracksGrid
        rows={rows}
        cols={cols}
        counts={counts}
        isComplete={isComplete}
        getCellState={getCellState}
        toggleCellState={toggleCellState}
      />
    </div>
  );
};

export default TrainTracksGame;
