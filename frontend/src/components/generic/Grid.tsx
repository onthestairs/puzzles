import React from "react";

import styles from "./Grid.module.css";
import { range } from "../puzzles/train-tracks/utils";

const Grid = ({
  width,
  height,
  cells,
  isComplete
}: {
  width: string | number;
  height: string | number;
  cells: JSX.Element[][];
  isComplete: Boolean;
}) => {
  const rows = cells.length;
  const cols = cells[0].length;

  const style = {
    gridTemplateRows: `repeat(${rows}, ${100 / rows}%)`,
    gridTemplateColumns: `repeat(${cols}, ${100 / cols}%)`,
    // gridTemplateAreas: gridTemplateAreas,
    width: width,
    height: height
  } as any;
  if (isComplete) {
    style.backgroundColor = "green";
  }
  return (
    <div className={styles.grid} style={style}>
      {cells.flat()}
    </div>
  );
};

export default Grid;
