export const range = (n: number): number[] => {
  let xs: number[] = [];
  for (let i = 0; i < n; i++) {
    xs.push(i);
  }
  return xs;
};

export const updateListAt = <T,>(xs: T[], i: number, x: any): T[] => {
  let ys = [...xs];
  ys[i] = x;
  return ys;
};

export const updateNestedListAt = <T,>(
  xs: T[][],
  i: number,
  j: number,
  x: any
): T[][] => {
  let ys = [...xs];
  ys[i] = updateListAt(ys[i], j, x);
  return ys;
};

export const areArraysEqual = (xs: number[], ys: number[]): Boolean => {
  if (xs.length !== ys.length) return false;

  return xs.every((x, i) => {
    return x === ys[i];
  });
};
