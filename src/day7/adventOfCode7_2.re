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

let input =
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
     )
  |> Array.to_list;

let isLeaf = ((_, _, children)) => List.length(children) == 0;

let rec getChildIndexes = ((_, _, children)) =>
  children |> Array.fold_left((a, (_, i, _)) => a + i, 0);

let (_, _, bottomChildren) =
  switch (List.find(((x, _, _)) => "tknk" == x, input)) {
  | y => y
  | exception Not_found => raise(Not_found)
  };

let rec compareChildren = (l) => {
  let firstChildWeight = bottomChildren |> List.hd;
  Js.log(bottomChildren)
};

Js.log(compareChildren(bottomChildren));
