exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
  mutable n_critical_path : int;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = []; n_critical_path = 0; } in
  g.g_nodes <- n :: g.g_nodes

let node_of_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to   <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found -> Format.eprintf "Tried to add an edge between non-existing nodes"; raise Not_found

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g = 
  let rec dfs n = 
    match n.n_mark with
    | Visited -> ()
    | InProgress -> raise Cycle
    | NotVisited -> 
      n.n_mark <- InProgress;
      List.iter dfs n.n_link_to;
      n.n_mark <- Visited
  in
  try 
    clear_marks g;
    List.iter dfs g.g_nodes;
    false
  with Cycle -> true

let topological g =
  let rec dfs q n = 
    match n.n_mark with
    | Visited -> ()
    | InProgress -> raise Cycle
    | NotVisited ->
      n.n_mark <- InProgress;
      List.iter (dfs q) n.n_link_to;
      n.n_critical_path <- 1 + List.fold_left (fun acc n -> max acc n.n_critical_path) 0 n.n_link_to;
      Stack.push n.n_label q;
      n.n_mark <- Visited
  in
  try 
    clear_marks g;
    let q = Stack.create () in
      List.iter (dfs q) (find_roots g);
      List.iter (dfs q) (List.filter (fun n -> n.n_mark = NotVisited) g.g_nodes);
      let l = ref [] in
      while not (Stack.is_empty q) do
        l := Stack.pop q :: !l
      done;
      List.rev !l
  with Cycle -> raise Cycle