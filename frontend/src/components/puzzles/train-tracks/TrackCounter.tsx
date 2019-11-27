import React from "react";

import styles from "./TrainTracks.module.css";

const TrackCounter = ({
  desiredCount,
  currentCount
}: {
  desiredCount: number;
  currentCount: number;
}) => {
  let classes = [styles.trackCounter];
  if (currentCount > desiredCount) {
    classes.push(styles.overTrackCounter);
  } else if (currentCount === desiredCount) {
    classes.push(styles.equalTrackCounter);
  }
  return <div className={classes.join(" ")}>{desiredCount}</div>;
};

export default TrackCounter;
