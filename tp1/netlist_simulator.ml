open Netlist_ast
exception Wrong_input
let print_only = ref false
let number_steps = ref (-1)

module StrMap = Map.Make(String)
module IntMap = Map.Make(Int)

exception Arrays_of_different_length of ident
exception Incompatible_types of ident
exception Variable_not_found of ident
exception Cannot_slice_bit of ident
exception Value_to_RAM_not_found

type environment = 
    {
        vars : value StrMap.t;
        roms : value IntMap.t;
        rams : value IntMap.t;
    }

let print_state env p = 
    List.iter (
        fun input ->
            let raw_output = StrMap.find input env.vars
        in let formatted_output = 
            match raw_output with
            | VBit b -> if b then "1" else "0"
            | VBitArray b -> String.of_seq (Array.to_seq (Array.map (fun x -> if x then '1' else '0') b))
        in Printf.printf "=> %s = %s\n" input formatted_output
    ) p.p_outputs

let value_to_int (c : value) : int =
    match c with 
    | VBit b -> if b then 1 else 0
    | VBitArray b -> Array.fold_left (fun x y -> if y then x*2 else x*2 + 1) 0 b

let array_combine arr1 arr2 =
    if Array.length arr1 <> Array.length arr2 then
        invalid_arg "array_combine: input arrays have different lengths"
    else
        let combined = Array.make (Array.length arr1) (false, false) in
        for i = 0 to Array.length arr1 - 1 do
        combined.(i) <- (arr1.(i), arr2.(i))
        done;
        combined

let read_ram rams word_size addr = 
	try
		IntMap.find addr rams
	with
		| Not_found -> VBitArray (Array.make word_size false)

let next_step prev_env (env, rams_to_write) (iter, expr) = 
	try
        begin
            let vars = env.vars in 
            let roms = env.roms in
            let rams = env.rams in 
            let get_value c = 
                match c with 
                | Aconst c -> c
                | Avar v -> StrMap.find v vars;
            in
            match expr with
            | Earg var -> 
                ({
                    vars = StrMap.add iter (get_value var) vars;
                    roms = roms;
                    rams = rams;
                }, rams_to_write)
            | Ereg var -> 
                ({
                    vars = StrMap.add iter (StrMap.find var prev_env.vars) vars;
                    roms = roms;
                    rams = rams;
                }, rams_to_write)
            | Enot var -> 
                let old_value = get_value var in
                let new_value = 
                    match old_value with
                    | VBit b -> VBit (not b)
                    | VBitArray b -> VBitArray (Array.map not b)
                in
                ({
                    vars = StrMap.add iter new_value vars;
                    roms = roms;
                    rams = rams;
                }, rams_to_write)
            | Ebinop (op, var1, var2) -> 
                let value1 = get_value var1 in
                let value2 = get_value var2 in
                let new_value = 
                    match (value1, value2) with
                    | (VBit b1, VBit b2) -> 
                        begin
                            match op with
                            | Or -> VBit (b1 || b2)
                            | Xor -> VBit (b1 <> b2)
                            | And -> VBit (b1 && b2)
                            | Nand -> VBit (not (b1 && b2))
                        end
                    | (VBitArray b1, VBitArray b2) ->
                        begin
                            if Array.length b1 <> Array.length b2 
                                then raise (Arrays_of_different_length iter)
                            else match op with
                            | Or -> VBitArray (Array.map2 (fun x y -> x || y) b1 b2)
                            | Xor -> VBitArray (Array.map2 (fun x y -> x <> y) b1 b2)
                            | And -> VBitArray (Array.map2 (fun x y -> x && y) b1 b2)
                            | Nand -> VBitArray (Array.map2 (fun x y -> not (x && y)) b1 b2)
                        end
                    | _ -> raise (Incompatible_types iter)
                in
                ({
                    vars = StrMap.add iter new_value vars;
                    roms = roms;
                    rams = rams;
                }, rams_to_write)
            | Emux (var1, var2, var3) ->
                let value1 = get_value var1 in
                let value2 = get_value var2 in
                let value3 = get_value var3 in
                let new_value = 
                    try 
                        match (value1, value2, value3) with
                        | (VBit b1, VBit b2, VBit b3) -> 
                            if b1 then VBit b3 else VBit b2
                        | (VBit b1, VBitArray b2, VBitArray b3) ->
                            if Array.length b2 <> Array.length b3 
                                then raise (Arrays_of_different_length iter)
                            else if b1 then VBitArray b3 else VBitArray b2
                        | _ -> raise (Incompatible_types iter)
                    with 
                        | Invalid_argument _ -> raise (Arrays_of_different_length iter)
                in
                ({
                    vars = StrMap.add iter new_value vars;
                    roms = roms;
                    rams = rams;
                }, rams_to_write)
            | Erom (addr_size, word_size, var) ->
                let addr = value_to_int (get_value var) in
                ({
                    vars = StrMap.add iter (IntMap.find addr prev_env.roms) vars;
                    roms = roms;
                    rams = rams;
                }, rams_to_write)
            | Eram (addr_size, word_size, read_addr, write_enable, write_addr, data) ->
                let read_addr = value_to_int (get_value read_addr) in
				let write_enable = value_to_int (get_value write_enable) in
				let new_rams_to_write = 
                    if write_enable = 1 then
				        let write_addr = value_to_int (get_value write_addr) in
                        (write_addr, data)::rams_to_write
                    else
                        rams_to_write
                in
                ({
                    vars = StrMap.add iter (read_ram rams word_size read_addr) vars;
                    roms = roms;
                    rams = rams;
                }, new_rams_to_write)
            | Econcat (var1, var2) ->
                let value1 = get_value var1 in
                let value2 = get_value var2 in
                let array1 = 
                    match value1 with
                    | VBit b -> [|b|]
                    | VBitArray b -> b
                in
                let array2 = 
                    match value2 with
                    | VBit b -> [|b|]
                    | VBitArray b -> b
                in 
                ({
                    vars = StrMap.add iter (VBitArray (Array.append array1 array2)) vars;
                    roms = roms;
                    rams = rams;
                }, rams_to_write)
            | Eslice (i1, i2, var) ->
                let value = get_value var in
                let new_value = 
                    match value with
                    | VBit b -> if i1 > 0 then raise (Cannot_slice_bit iter) else VBit b
                    | VBitArray b -> VBitArray (Array.sub b i1 (i2 - i1 + 1))
                in
                ({
                    vars = StrMap.add iter new_value vars;
                    roms = roms;
                    rams = rams;
                }, rams_to_write)
            | Eselect (i, var) ->
                let value = get_value var in
                let new_value = 
                    match value with
                    | VBit b -> if i > 0 then raise (Cannot_slice_bit iter) else VBit b
                    | VBitArray b -> VBit (Array.get b i)
                in
                ({
                    vars = StrMap.add iter new_value vars;
                    roms = roms;
                    rams = rams;
                }, rams_to_write)
        end
    with
		| Arrays_of_different_length _ -> raise (Arrays_of_different_length iter)
		| Incompatible_types _ -> raise (Incompatible_types iter)
		| Cannot_slice_bit _ -> raise (Cannot_slice_bit iter)
		| Not_found -> raise (Variable_not_found iter)
let next_state initial_env p = 
    (* Read input from console *)
    let rec read_input x = 
        try
            Printf.printf "%s ? " x;
            flush stdout;
            let t = Env.find x p.p_vars in 
            let input = read_line () in 
            if (String.length input = 1) then 
                match input with 
                | "0" -> if t = TBit then VBit false else VBitArray [|false|]
                | "1" -> if t = TBit then VBit true else VBitArray [|true|]
                | _ -> raise Wrong_input
            else 
                match t with
                | TBit -> raise Wrong_input
                | TBitArray n ->
                    let array =
                        Array.map (
                        fun x -> 
                            match x with 
                            | '0' -> false
                            | '1' -> true
                            | _ -> raise Wrong_input)
                        (input |> String.to_seq |> Array.of_seq)
                    in
                    if Array.length array <> n
                        then raise Wrong_input
                    else VBitArray array
        with
            | Wrong_input ->
                begin
                    print_string "Wrong input.\n";
                    read_input x
                end
            | Not_found ->
                begin
                    print_string "Variable not found?! The simulator is faulty.\n";
                    read_input x
                end
    in let initial_vars = 
        List.fold_left (fun y x -> StrMap.add x (read_input x) y) initial_env.vars p.p_inputs  
    (* Execute through the program *)
    in let env_with_input = 
        {
            vars = initial_vars;
            roms = initial_env.roms;
            rams = initial_env.rams;
        }
    in try
		let (env_without_written_ram, rams_to_write) = List.fold_left (next_step env_with_input) (env_with_input, []) p.p_eqs in
		{
			vars = env_without_written_ram.vars;
			roms = env_without_written_ram.roms;
			rams = List.fold_left (
				fun old_ram (addr, var) -> 
					let get_value c = 
						match c with 
						| Aconst c -> c
						| Avar v -> StrMap.find v env_without_written_ram.vars;
					in
					let value = get_value var in 
					IntMap.add addr value old_ram
				) env_without_written_ram.rams rams_to_write;
		}
	with
		| Not_found -> raise Value_to_RAM_not_found
		| Arrays_of_different_length iter -> raise (Arrays_of_different_length iter)
		| Incompatible_types iter -> raise (Incompatible_types iter)
		| Cannot_slice_bit iter -> raise (Cannot_slice_bit iter)
		| Variable_not_found iter -> raise (Variable_not_found iter)

let simulate program number_steps = 
    (* Initialise an environment *)
    let initial_vars =
        List.fold_left (
            fun m (id, t) ->
                match t with
                | TBit -> StrMap.add id (VBit false) m
                | TBitArray n -> StrMap.add id (VBitArray (Array.make n false)) m
        ) StrMap.empty (Env.bindings program.p_vars)
    in let initial_env = { vars = initial_vars; roms = IntMap.empty; rams = IntMap.empty }
    in let rec simulator_aux env p step_number maximum_number_of_steps = 
		if maximum_number_of_steps - step_number = -1 then ()
		else 
		begin
            Printf.printf "Step %d:\n" step_number;
            flush stdout;
			let new_env = next_state env p in
			print_state new_env p;
			simulator_aux new_env p (step_number+1) maximum_number_of_steps;
		end
	in simulator_aux initial_env program 1 number_steps

let print_only = ref false

let compile filename =
	try
		let raw_program = Netlist.read_file filename in
		let scheduled_program = Scheduler.schedule raw_program in
		if !print_only then
			let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
			let out = open_out out_name in
			let close_all () =
			close_out out
			in begin
				Netlist_printer.print_program out scheduled_program;
				close_all ()
			end
		else
			simulate scheduled_program !number_steps;
	with
		| Netlist.Parse_error s -> 
			begin
				Format.eprintf "An error accurred: %s@." s;
				exit 2
			end
		| Scheduler.Combinational_cycle ->
			Format.eprintf "The netlist has a combinatory cycle.@."
		| Scheduler.Variable_assigned_twice id ->
			Format.eprintf "The variable %s is assigned twice.@." id
		| Scheduler.Unknown_variable id ->
			Format.eprintf "An unknown variable %s was found.@." id
		| Value_to_RAM_not_found -> 
			Format.eprintf "The value to write to the RAM was not found.@."
		| Arrays_of_different_length id ->
			Format.eprintf "The arrays in the expression for variable %s have different lengths.@." id
		| Incompatible_types id ->
			Format.eprintf "The types in the expression for variable %s are incompatible.@." id
		| Cannot_slice_bit id ->
			Format.eprintf "The expression %s cannot be sliced.@." id
		| Variable_not_found id ->
			Format.eprintf "The variable %s was not found.@." id

let main () =
	Arg.parse
		["-print", Arg.Set print_only, "Prints the netlist after scheduling";
		"-n", Arg.Set_int number_steps, "Number of steps to simulate"]
		compile
		"";;

main ()