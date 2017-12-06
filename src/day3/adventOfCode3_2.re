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
  let array = Array.make_matrix(arraySize, arraySize, 0);
  let half = arraySize / 2;
  let out = (i) => i < 0 || i >= n;
  let shouldTurn = ((x, y)) => out(x) || out(y) || array[x][y] == 0;
  let step = (x, y, (dx, dy)) => (x + dx, y + dy);
  let turn = ((i, j)) => (- j, i);
  let adjacentSquares = [
    ((-1), (-1)),
    ((-1), 0),
    ((-1), 1),
    (0, (-1)),
    (0, 1),
    (1, (-1)),
    (1, 0),
    (1, 1)
  ];
  let adjacentSquaresValue = (x, y) =>
    adjacentSquares
    |> List.fold_left(
         (a, (dx, dy)) => {
           let value =
             switch array[x + dx][y + dy] {
             | value => value
             | exception (Invalid_argument("index out of bounds")) => 0
             };
           a + value
         },
         0
       );
  let rec iter = ((x, y), d, i) => {
    let value = i === 1 ? 1 : adjacentSquaresValue(x, y);
    array[x][y] = value;
    if (value > n) {
      Js.log(value)
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
  iter(start, (1, 0), 1)
};

calculateStepsToMemorySlot(277678);
