let myInput = [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11];

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

let rec foo = (bank, acc, biggest, steps) =>
  switch (List.find((a) => a == bank, acc)) {
  | exception Not_found =>
    let arr = Array.of_list(bank);
    let (startIndex, amount) = biggest;
    arr[startIndex] = 0;
    for (x in startIndex + 1 to startIndex + amount) {
      let index = x mod length;
      arr[index] = arr[index] + 1
    };
    let newList = Array.to_list(arr);
    foo(newList, [bank, ...acc], max(withIndex(newList)), steps + 1)
  | _ =>
    Js.log("foo");
    steps
  };

Js.log("start");

Js.log(foo(myInput, [], max(initial), 0));
