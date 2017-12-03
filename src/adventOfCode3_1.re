/*
 17  16  15  14  13
 18   5   4   3  12
 19   6   1   2  11
 20   7   8   9  10
 21  22  23  -> ...
 */
let myInput = 277678;

/* adapted from https://rosettacode.org/wiki/Spiral_matrix#OCaml */
let calculateStepsToMemorySlot = (n) => {
  let arraySize = int_of_float(ceil(sqrt(float(n)))) + 1;
  let array = Array.make_matrix(arraySize, arraySize, (-1));
  let half = arraySize / 2;
  let out = (i) => i < 0 || i >= n;
  let shouldTurn = ((x, y)) => out(x) || out(y) || array[x][y] == (-1);
  let step = (x, y, (dx, dy)) => (x + dx, y + dy);
  let turn = ((i, j)) => (- j, i);
  let memoryPos = ref((0, 0));
  let rec iter = ((x, y), d, i) => {
    array[x][y] = i;
    if (i == n) {
      memoryPos := (x, y)
    } else if (i < arraySize * arraySize - 1) {
      let d' =
        if (shouldTurn(step(x, y, turn(d)))) {
          turn(d)
        } else {
          d
        };
      iter(step(x, y, d'), d', i + 1)
    }
  };
  let start = (half, half);
  iter(start, (1, 0), 1);
  let (x, y) = memoryPos^;
  if (half == 0) {
    1
  } else {
    let dx = abs(x - half);
    let dy = abs(y - half);
    dx + dy
  }
};

Js.log(calculateStepsToMemorySlot(1)); /* 1 */

Js.log(calculateStepsToMemorySlot(12)); /* 12 */

Js.log(calculateStepsToMemorySlot(23)); /* 23 */

Js.log(calculateStepsToMemorySlot(1024)); /* 31 */

Js.log(calculateStepsToMemorySlot(myInput)); /* 31 */
