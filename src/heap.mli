type 'key t

val create : unit -> 'key t

val length : 'key t -> int

(* membership test *)
val mem : 'key t -> 'key -> bool

val get_min : 'key t -> ('key * int) option

(* Only valid if the key is not present in the heap *)
val insert : 'key t -> ('key * int) -> unit

(* Only valid if the key is already present in the heap, *)
(* and if the new priority is less than the old one *)
val decrease_priority : 'key t -> ('key * int) -> unit

(* Valid whether or not the key is already in the heap *)
(* If it is already there, the new priority must be less *)
(* than the old one. *)
val insert_or_decrease : 'key t -> ('key * int) -> unit

val extract_min : 'key t -> ('key * int) option
