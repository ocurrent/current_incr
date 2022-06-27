(* A queue implemented as a circular double-linked list:
   - The root of type ['a t] is represented by a node holding no value.
   - The [prev]/[next] pointers forms a loop, with the root acting as a sentinel. *)

type 'a value =
  | Root
  | Value of 'a

type 'a elt = {
  value: 'a value;
  mutable prev: 'a elt;
  mutable next: 'a elt;
}

type 'a t = 'a elt (* Such that its value = Root *)

let is_root q = q.value = Root

let create () =
  let rec root = { value = Root; prev = root; next = root } in
  root

let add x q =
  assert (is_root q);
  let elt = { value = Value x; prev = q.prev; next = q } in
  q.prev.next <- elt;
  q.prev <- elt;
  elt

let remove elt =
  assert (not (is_root elt));
  let prev, next = elt.prev, elt.next in
  prev.next <- next;
  next.prev <- prev

let iter f q =
  assert (is_root q);
  let rec iter f elt =
    match elt.value with
    | Root -> ()
    | Value x ->
      f x;
      iter f elt.next
  in
  iter f q.next
