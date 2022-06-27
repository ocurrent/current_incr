(** A queue holding values of type ['a]. *)
type 'a t

(** Create a new empty queue. *)
val create : unit -> 'a t

(** Iterate on all the elements of the queue. *)
val iter : ('a -> unit) -> 'a t -> unit

(** An element of type ['a] present in the queue. *)
type 'a elt

(** Add a new value at the end of the queue, return its corresponding element. *)
val add : 'a -> 'a t -> 'a elt

(** Remove a previously added element from the queue. *)
val remove : 'a elt -> unit
