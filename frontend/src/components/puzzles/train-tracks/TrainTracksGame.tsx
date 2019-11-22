import React from "react";

import { TrainTracks } from "../../../api-hooks";
import Grid from "../../generic/Grid";
import TrainTracksGrid from "./TrainTracksGrid";
import { useTrainTracksGame } from "./gameHooks";

const TrainTracksGame = ({ trainTracks }: { trainTracks: TrainTracks }) => {
  const rows = trainTracks._gridSize._rows;
  const cols = trainTracks._gridSize._cols;
  const {
    elapsedSeconds,
    cells: { getCellState, toggleCellState }
  } = useTrainTracksGame(rows, cols);

  return (
    <div>
      <em>{elapsedSeconds} elapsed</em>
      <TrainTracksGrid
        rows={rows}
        cols={cols}
        getCellState={getCellState}
        toggleCellState={toggleCellState}
      />
    </div>
  );
};

export default TrainTracksGame;
