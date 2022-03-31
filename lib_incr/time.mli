(* Based on https://github.com/let-def/grenier/
   By Frédéric Bour. Relicensed to Apache 2.0 with permission. *)

(** {1 Basic ordering operations} *)

(** An element of an ordering. *)
type t

(** Create a new ordering with a single element. O(1) *)
val root : unit -> t

(** [after ~depth t] inserts a new element to the ordering, greater than [t] but
    less than all existing elements greater than [t].

    The [depth] is used by [splice_out] to determine if the created element is
    a child of [t]. The children of [t] are all the elements directly following
    [t] and having a larger [depth] than it.

    O(1) amortized. *)
val after  : ?on_forget:(unit -> unit) -> depth:int -> t -> t

(** Check if two elements belong to the same order. O(1) *)
val same_order : t -> t -> bool

(** Compare two elements. O(1) *)
val compare : t -> t -> int

(** How many elements are ordered. O(1) *)
val cardinal : t -> int

(** {1 Memory management} *)

(** Memory of every element is retained. When you know you are not going to use
    an element any longer, [forget] it to release memory. O(1). *)
val forget : t -> unit

(** After calling [forget], an element should not be used.
    You can check if it is the case with [is_valid]. *)
val is_valid : t -> bool

(** [splice_out t] forgets all times immediately following and having a larger
    depth than [t]. *)
val splice_out : t -> unit

val set_forget : t -> (unit -> unit) -> unit
(** [set_forget t fn] sets [t]'s forget function to [fn].
    This is called when [t] is forgotten.
    It is an error if [t] already has a forget function. *)

val clear_forget : t -> unit
(** [clear_forget t] reverses the effect of [set_forget]. *)

val next : t -> t
(** [next t] is the time immediately after [t] (which must not be the last time). *)

val prev : t -> t
(** [prev t] is the time immediately before [t] (which must not be the first time). *)

val depth : t -> int
(** [depth t] is the depth of [t]. *)

(* Algorithm due to:
   Two Simplified Algorithms for Maintaining Order in a List
   Bender et al., 2002 *)
