open Netlist_ast
open Graph

exception Combinational_cycle

let get_args expr =
  match expr with
  | Earg a -> [a]
  | Ereg _ -> []
  | Enot a -> [a]
  | Ebinop (_, a, b) -> [a; b]
  | Emux (a, b, c) -> [a; b; c]
  | Erom (_, _, a) -> [a]
  | Eram (_, _, a, b, c, d) -> [a; b; c; d]
  | Econcat (a, b) -> [a; b]
  | Eslice (_, _, a) -> [a]
  | Eselect (_, a) -> [a]

let get_vars_from_args = 
  List.fold_left (fun acc arg -> match arg with
    | Avar v -> v :: acc
    | _ -> acc
  ) []

let read_exp (_, expr) = get_vars_from_args (get_args expr)

let schedule p = 
  (* We make a dictionary associating a variable with a list of equations giving values to this variable *)
  let dict = Hashtbl.create 16 in
  List.iter (fun (iter, expr) -> 
    List.iter (
      fun var -> 
        if not (Hashtbl.mem dict var) then Hashtbl.add dict var []
    ) (get_vars_from_args (get_args expr));
    if (Hashtbl.mem dict iter) then 
      Hashtbl.replace dict iter ((iter, expr) :: (Hashtbl.find dict iter))
    else
      Hashtbl.add dict iter [(iter, expr)]
  ) p.p_eqs;
  (* We make a graph, first with nodes being the variables in the program...*)
  let vars = Hashtbl.fold (fun k _ acc -> k :: acc) dict [] in 
  let graph = mk_graph () in 
  List.iter (fun var -> add_node graph var) vars;
  (* ... then we add edges between variables that are used in the same equation *)
  List.iter (fun (iter, expr) -> 
    match expr with
    | Ereg _ -> ()
    | Eram (_, _, ra, we, wa, _) -> 
      List.iter (fun var -> add_edge graph var iter) (get_vars_from_args [ra; we; wa])
    | _ -> List.iter (fun var -> add_edge graph var iter) (read_exp (iter, expr))
  ) p.p_eqs;
  (* We then compute a topological sort of the graph *)
  try
    let sorted_vars = topological graph in
    {
      p_eqs = List.concat (List.map (Hashtbl.find dict) sorted_vars);
      p_vars = p.p_vars;
      p_inputs = p.p_inputs;
      p_outputs = p.p_outputs;
    } 
  with Cycle -> raise Combinational_cycle