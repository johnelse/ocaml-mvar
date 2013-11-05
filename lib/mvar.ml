module Mutex = struct
	include Mutex
	(** execute the function f with the mutex hold *)
	let execute lock f =
		Mutex.lock lock;
		let r = begin try f () with exn -> Mutex.unlock lock; raise exn end; in
		Mutex.unlock lock;
		r
end

type 'a t = {
	data: 'a option ref;
	m: Mutex.t;
	c: Condition.t;
}

let create_empty () =
	{
		data = ref None;
		m = Mutex.create ();
		c = Condition.create ();
	}

let create x =
	{
		data = ref (Some x);
		m = Mutex.create ();
		c = Condition.create ();
	}

(* Wait until mvar holds a value, then return it and leave the mvar empty. *)
let take mvar =
	let rec check () =
		match !(mvar.data) with
		| None ->
			Condition.wait mvar.c mvar.m;
			check ()
		| Some x ->
			mvar.data := None;
			Condition.signal mvar.c;
			x
	in
	Mutex.execute mvar.m (fun () -> check ())

(* Non-blocking take - if the mvar holds no value, return None. *)
let try_take mvar =
	Mutex.execute mvar.m (fun () ->
		match !(mvar.data) with
		| None -> None
		| Some x ->
			mvar.data := None;
			Condition.signal mvar.c;
			Some x)

(* Wait until the mvar is empty, then put x in the mvar. *)
let put mvar x =
	let rec check () =
		match !(mvar.data) with
		| None ->
			mvar.data := (Some x);
			Condition.signal mvar.c
		| Some _ ->
			Condition.wait mvar.c mvar.m;
			check ()
	in
	Mutex.execute mvar.m (fun () -> check ())

(* Non-blocking put - if the mvar is empty the put x in the mvar, otherwise return false. *)
let try_put mvar x =
	Mutex.execute mvar.m (fun () ->
		match !(mvar.data) with
		| Some _ ->
			false
		| None ->
			mvar.data := (Some x);
			Condition.signal mvar.c;
			true)

(* Test whether the mvar is empty. *)
let is_empty mvar =
	Mutex.execute mvar.m (fun () ->
		match !(mvar.data) with
		| Some _ -> false
		| None -> true)

(* Wait until the mvar is populated, then set its value to x and return the previous value. *)
let swap mvar x =
	let rec check () =
		match !(mvar.data) with
		| None ->
			Condition.wait mvar.c mvar.m;
			check ()
		| Some y ->
			mvar.data := (Some x);
			Condition.signal mvar.c;
			y
	in
	Mutex.execute mvar.m (fun () -> check ())

(* Wait until the mvar is populated, then use the supplied function to modify its value.*)
let modify mvar f =
	let rec check () =
		match !(mvar.data) with
		| None ->
			Condition.wait mvar.c mvar.m;
			check()
		| Some x ->
			mvar.data := (Some (f x));
			Condition.signal mvar.c
	in
	Mutex.execute mvar.m (fun () -> check ())
