let myInput = [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11];

/* let myInput = [0, 2, 7, 0]; */
let length = List.length(myInput);

let withIndex = (arr) => arr |> List.mapi((i, a) => (i, a));

let initial = withIndex(myInput);

let max =
  fun
  | [] => invalid_arg("Empty list")
  | [x, ...xs] =>
    List.fold_left(
      ((aIndex, a), (bIndex, b)) =>
        if (a >= b) {
          (aIndex, a)
        } else {
          (bIndex, b)
        },
      x,
      xs
    );

let newList = (bank, biggest) => {
  let arr = Array.of_list(bank);
  let (startIndex, amount) = biggest;
  arr[startIndex] = 0;
  for (x in startIndex + 1 to startIndex + amount) {
    let index = x mod length;
    arr[index] = arr[index] + 1
  };
  Array.to_list(arr)
};

let rec foo = (bank, acc, biggest, steps, found) =>
  switch (List.find((a) => a == bank, acc)) {
  | exception Not_found =>
    let newList = newList(bank, biggest);
    foo(newList, [bank, ...acc], max(withIndex(newList)), steps + 1, found)
  | _ =>
    if (found > 0) {
      steps - found
    } else {
      let newList = newList(bank, biggest);
      foo(newList, List.filter((a) => a == bank, acc), max(withIndex(newList)), steps + 1, steps)
    }
  };

Js.log("start");

Js.log(foo(myInput, [], max(initial), 0, 0));
