import React from "react";

import styles from "./TrainTracks.module.css";
import { Direction } from "./game";
import corner from "./tracks-svgs/corner.svg";
import horizontal from "./tracks-svgs/horizontal.svg";

const Cell = ({
  row,
  col,
  value,
  toggleValue
}: {
  row: number;
  col: number;
  value: Direction;
  toggleValue: any;
}) => {
  const glyph = getGridChar(value);
  return (
    <div className={styles.cell} onClick={e => toggleValue()}>
      {glyph}
    </div>
  );
};

const SVGCell = ({ src, rotate = 0 }: { src: string; rotate?: number }) => {
  return (
    <img
      style={{
        width: "100%",
        height: "100%",
        transform: `rotate(${rotate}deg)`
      }}
      src={src}
    />
  );
};

const Grass = () => {
  return (
    <div
      style={{
        width: "100%",
        height: "100%",
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
        color: "grey",
        fontSize: "18px"
      }}
    >
      x
    </div>
  );
};

const getGridChar = (direction: Direction) => {
  switch (direction) {
    case Direction.Unknown:
      return " ";
    case Direction.DownRight:
      return <SVGCell src={corner} rotate={270} />;
    case Direction.UpLeft:
      return <SVGCell src={corner} rotate={90} />;
    case Direction.UpRight:
      return <SVGCell src={corner} rotate={180} />;
    case Direction.DownLeft:
      return <SVGCell src={corner} />;
    case Direction.Horizontal:
      return <SVGCell src={horizontal} />;
    case Direction.Vertical:
      return <SVGCell src={horizontal} rotate={90} />;
    case Direction.Empty:
      return <Grass />;
    default:
      return "-";
  }
};

export default Cell;
