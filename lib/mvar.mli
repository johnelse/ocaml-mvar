type 'a t

val create_empty : unit -> 'a t

val create : 'a -> 'a t

val take : 'a t -> 'a

val try_take : 'a t -> 'a option

val put : 'a t -> 'a -> unit

val try_put : 'a t -> 'a -> bool

val is_empty : 'a t -> bool

val swap : 'a t -> 'a -> 'a

val modify : 'a t -> ('a -> 'a) -> unit
