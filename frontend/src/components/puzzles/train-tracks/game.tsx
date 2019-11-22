export enum Direction {
  Unknown,
  Horizontal,
  Vertical,
  DownLeft,
  DownRight,
  UpLeft,
  UpRight,
  Empty
}

const makeToggleMapping = (directionOrdering: Direction[]) => {
  return (direction: Direction): Direction => {
    for (let i = 0; i < directionOrdering.length; i++) {
      const currentDirection = directionOrdering[i];
      if (currentDirection === direction) {
        const nextDirection =
          directionOrdering[(i + 1) % directionOrdering.length];
        return nextDirection;
      }
    }
    return direction;
  };
};

export const toggleDirection = makeToggleMapping([
  Direction.Unknown,
  Direction.Horizontal,
  Direction.Vertical,
  Direction.DownLeft,
  Direction.UpLeft,
  Direction.UpRight,
  Direction.DownRight,
  Direction.Empty
]);
