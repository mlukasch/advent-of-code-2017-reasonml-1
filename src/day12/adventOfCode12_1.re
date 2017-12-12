module IntSet =
  Set.Make(
    {
      let compare = Pervasives.compare;
      type t = int;
    }
  );

let parseInput = (input) =>
  input
  |> Js.String.split("\n")
  |> Array.map(
       (v) => {
         let [|program, links|] = Js.String.split(" <-> ", v);
         (
           int_of_string(program),
           links |> Js.String.split(", ") |> Array.map((a) => int_of_string(a)) |> Array.to_list
         )
       }
     )
  |> Array.to_list;

let testInput = "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5";

let input = parseInput(Input12.input);

let test = parseInput(testInput);

/* borrowed mostly from https://www.reddit.com/r/adventofcode/comments/7j89tr/2017_day_12_solutions/dr4kwgv/ */
let makeGraph = (input) => {
  let graph = Hashtbl.create(List.length(input));
  List.iter(((program, links)) => Hashtbl.add(graph, program, links), input);
  graph
};

let walkGraph = (start, graph) => {
  let rec walk = (visited, queue) =>
    switch queue {
    | [x, ...xs] when ! IntSet.mem(x, visited) =>
      walk(IntSet.add(x, visited), List.append(xs, Hashtbl.find(graph, x)))
    | [_, ...xs] => walk(visited, xs)
    | [] => visited
    };
  walk(IntSet.empty, [start])
};

walkGraph(0, makeGraph(test)) |> IntSet.cardinal |> Js.log;
