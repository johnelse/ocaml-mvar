open Mvar

(* Helpers. *)
let print_m = Mutex.create ()
let printer message =
	Mutex.execute print_m (fun () ->
		print_endline message)

(* Tests. *)
let sequential_test () =
	printer "Starting sequential test";
	let mvar = create_empty () in
	let value = "hello" in
	printer "Testing is_empty on empty mvar";
	if not(is_empty mvar) then failwith "is_empty failed";
	printer "Testing try_get on empty mvar";
	if (try_take mvar) <> None then failwith "try_get failed";
	printer "Testing try_put on empty mvar";
	if not(try_put mvar value) then failwith "try_put failed";
	printer "Testing is_empty on populated mvar";
	if is_empty mvar then failwith "is_empty failed";
	printer "Testing try_put on populated mvar";
	if (try_put mvar value) then failwith "try_put failed";
	printer "Testing try_get on populated mvar";
	match try_take mvar with
	| None -> failwith "try_get failed"
	| Some x -> if x <> value then failwith "try_get failed";
	printer "Testing is_empty on empty mvar";
	if not(is_empty mvar) then failwith "is_empty failed"

let _ =
	sequential_test ()
