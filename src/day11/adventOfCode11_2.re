/* this article rules https://www.redblobgames.com/grids/hexagons */
type hex = (int, int, int);

let hexAdd = (a, b) => {
  let (q1, r1, s1) = a;
  let (q2, r2, s2) = b;
  let newHex: hex = (q1 + q2, r1 + r2, s1 + s2);
  newHex
};

let hexSubstract = (a, b) => {
  let (q1, r1, s1) = a;
  let (q2, r2, s2) = b;
  let newHex: hex = (q1 - q2, r1 - r2, s1 - s2);
  newHex
};

let start: hex = (0, 0, 0);

let n: hex = (1, 0, (-1));

let ne: hex = (1, (-1), 0);

let se: hex = (0, (-1), 1);

let s: hex = ((-1), 0, 1);

let sw: hex = ((-1), 1, 0);

let nw: hex = (0, 1, (-1));

let hexDirections: list(hex) = [n, ne, se, s, sw, nw];

let hexDiagonals: list(hex) = [
  (2, (-1), (-1)),
  (1, (-2), 1),
  ((-1), (-1), 2),
  ((-2), 1, 1),
  ((-1), 2, (-1)),
  (1, 1, (-2))
];

let hexLength = ((q, r, s)) => (abs(q) + abs(r) + abs(s)) / 2;

let hexDistance = (a, b) => hexLength(hexSubstract(a, b));

let hexDirection = (direction) => direction |> List.nth(hexDirections);

let hexDiagonal = (direction) => direction |> List.nth(hexDiagonals);

let hexNeighbour = (hex, direction) => hexAdd(hex, hexDirection(direction));

let hexDiagonalNeighbour = (hex, direction) => hexAdd(hex, hexDiagonal(direction));

let maxDistance = ref(0);

let traverse = (l, startHex) =>
  l
  |> List.fold_left(
       (a, b) => {
         let stop = hexAdd(a, b);
         let distance = hexDistance(start, stop);
         Js.log(distance);
         maxDistance := max(distance, maxDistance^);
         stop
       },
       startHex
     );

let test1 = [ne, ne, ne];

let test2 = [ne, ne, sw, sw];

let test3 = [ne, ne, s, s];

let test4 = [se, sw, se, sw, sw];

Js.log(hexDistance(start, traverse(test1, start))); /* 3 */

Js.log(hexDistance(start, traverse(test2, start))); /* 0 */

Js.log(hexDistance(start, traverse(test3, start))); /* 2 */

Js.log(hexDistance(start, traverse(test4, start))); /* 3 */

let inputs = [Input.input1, Input.input2, Input.input3];

let inputEnd = List.fold_left((a, b) => traverse(b, a), start, inputs);

hexDistance(start, inputEnd);

Js.log(maxDistance);
