export const range = (n: number): number[] => {
  let xs: number[] = [];
  for (let i = 0; i < n; i++) {
    xs.push(i);
  }
  return xs;
};

export const updateListAt = (xs: any[], index: number, x: any): any[] => {
  let ys = [...xs];
  ys[index] = x;
  return ys;
};
