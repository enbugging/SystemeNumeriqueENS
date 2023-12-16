open Netlist_ast
open Graph

exception Combinational_cycle
exception Variable_assigned_twice of ident
exception Unknown_variable of ident

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
  Env.iter (fun k v -> Hashtbl.add dict k []) p.p_vars;
  List.iter (fun (iter, expr) -> 
    try 
      let r = Hashtbl.find dict iter in
        if r = [] then 
          Hashtbl.add dict iter [(iter, expr)] 
        else
          raise (Variable_assigned_twice iter)
    with
      Not_found -> raise (Unknown_variable iter)
  ) p.p_eqs;
  (* We make a graph, first with nodes being the variables in the program...*)
  let graph = mk_graph () in 
  Env.iter (fun var ty -> add_node graph var) p.p_vars;
  (* ... then we add edges between variables that are used in the same equation *)
  let logic_gates = ref 0 in 
  List.iter (fun (iter, expr) -> 
    match expr with
    | Ereg _ -> ()
    | Eram (_, _, ra, we, wa, _) -> 
      List.iter (fun var -> 
        if Hashtbl.mem dict var then add_edge graph var iter 
        else raise (Unknown_variable var)
      ) 
      (get_vars_from_args [ra; we; wa])
    | Enot a -> 
      begin
        increment graph iter;
        logic_gates := !logic_gates + 1;
        List.iter (fun var -> 
          if Hashtbl.mem dict var then add_edge graph var iter 
          else raise (Unknown_variable var)
        )
        (get_vars_from_args [a])
      end
    | Ebinop (_, a, b) ->
      begin
        increment graph iter;
        logic_gates := !logic_gates + 1;
        List.iter (fun var -> 
          if Hashtbl.mem dict var then add_edge graph var iter 
          else raise (Unknown_variable var)
        )
        (get_vars_from_args [a; b])
      end
    | Emux (a, b, c) ->
      begin
        increment graph iter;
        logic_gates := !logic_gates + 1;
        List.iter (fun var -> 
          if Hashtbl.mem dict var then add_edge graph var iter 
          else raise (Unknown_variable var)
        )
        (get_vars_from_args [a; b; c])
      end
    | _ -> 
      List.iter (fun var -> 
        if Hashtbl.mem dict var then add_edge graph var iter 
        else raise (Unknown_variable var)
      )
      (read_exp (iter, expr))
  ) p.p_eqs;
  (* We then compute a topological sort of the graph *)
  try
    let sorted_vars = topological graph in
    let critical_length = List.fold_left (fun acc n -> max acc n.n_critical_path) 0 graph.g_nodes in
    Printf.printf "Number of variables: %d\nCritical path length: %d\n" !logic_gates critical_length;
    {
      p_eqs = List.concat (List.map (Hashtbl.find dict) sorted_vars);
      p_vars = p.p_vars;
      p_inputs = p.p_inputs;
      p_outputs = p.p_outputs;
    } 
  with Cycle -> raise Combinational_cycle