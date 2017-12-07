let words = (string) =>
  switch (Js.String.match([%re "/[a-zA-z]+/gi"], string)) {
  | Some(result) => Array.to_list(result)
  | None => Array.to_list([||])
  };

let number = (string) =>
  [%re "/(\\d+)/"]
  |> Js.Re.exec(string)
  |> (
    fun
    | Some(result) => Js.Nullable.to_opt(Js.Re.captures(result)[1])
    | None => None
  );

/* pbga (66)
   xhth (57)
   ebii (61)
   havc (66)
   ktlj (57)
   fwft (72) -> ktlj, cntj, xhth
   qoyq (66)
   padx (45) -> pbga, havc, qoyq
   tknk (41) -> ugml, padx, fwft
   jptl (61)
   ugml (68) -> gyxo, ebii, jptl
   gyxo (61)
   cntj (57) */
let testInput =
  "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"
  |> Js.String.split("\n")
  |> Array.map(
       (a) => {
         let [first, ...rest] = words(a);
         let index =
           switch (number(a)) {
           | Some(result) => int_of_string(result)
           | None => 0
           };
         (first, index, rest)
       }
     );
