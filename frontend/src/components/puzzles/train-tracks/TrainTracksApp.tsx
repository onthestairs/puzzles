import React from "react";

import { TrainTracks } from "./game";
import styles from "./TrainTracks.module.css";
import TrainTracksGame from "./TrainTracksGame";

const TrainTracksApp = ({ trainTracks }: { trainTracks: TrainTracks }) => {
  return (
    <div className={styles.puzzlePage}>
      <div className={styles.puzzleHeader}>
        <h2>Train tracks</h2>
      </div>

      <div className={styles.play}>
        <TrainTracksGame trainTracks={trainTracks} />
      </div>
    </div>
  );
};

export default TrainTracksApp;
