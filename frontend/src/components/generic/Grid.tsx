import React from "react";

import styles from "./Grid.module.css";

const Grid = ({
  rows,
  cols,
  width,
  height,
  children
}: {
  rows: number;
  cols: number;
  width: string | number;
  height: string | number;
  children: any[];
}) => {
  const style = {
    gridTemplateRows: `repeat(${rows}, ${100 / rows}%)`,
    gridTemplateColumns: `repeat(${cols}, ${100 / cols}%)`,
    width: width,
    height: height
  };
  return (
    <div className={styles.grid} style={style}>
      {children}
    </div>
  );
};

export default Grid;
