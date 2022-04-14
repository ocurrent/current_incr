(* Generate random expressions containing sums, conditionals and binds,
   then compare the results with a naive evaluation without SAC.

   The naive evaluations produce a [log] of all the [read] operations
   performed.  The diff of this log with their previous run produces a minimal
   patch describing which steps the incremental version should have performed
   to optimally compute the new result. *)

module Incr = Current_incr

(* Unique identifiers, to track the [read] and values accross different evaluations. *)
module Uid = struct
  type t = int
  let equal = Int.equal
  let gen = ref 0
  let fresh () =
    let u = !gen in
    gen := u + 1 ;
    u
  let pp h u = Format.fprintf h "%i" u
end

(* We will compare the incremental SAC evaluation against the naive result. *)
module Id = struct
  type 'a t  = T  of Uid.t * 'a
  type 'a cc = Cc of Uid.t * 'a

  let of_cc ~uid (Cc (_, x)) = T (uid, x)
  let write ~uid x = Cc (uid, x)
  let read (T (_, x)) f = f x

  let equal (T (x, _)) (T (y, _)) = Uid.equal x y
end


type (_, _) eq = Refl : ('a, 'a) eq

(* The generator will produce both incremental expression of type ['a0 Incr.t]
   and naive ['a1 Id.t] values, where intuitively ['a0 = 'a1] -- but due
   to the potential nesting of [t], this equality isn't enforceable.

   This is why the following GADTs have two components, one for the incremental type
   and one for the identity value. *)
module Type = struct
  type (_, _) t =
    | Int : (int, int) t
    | T  : ('a, 'b) t -> ('a Incr.t,  'b Id.t) t
    | Cc : ('a, 'b) t -> ('a Incr.cc, 'b Id.cc) t

  let rec eq : type a1 b1 a2 b2. (a1, b1) t -> (a2, b2) t -> ((a1, a2) eq * (b1, b2) eq) option
  = fun a b ->
    match a, b with
    | Int, Int -> Some (Refl, Refl)
    | T  a, T  b -> (match eq a b with Some (Refl, Refl) -> Some (Refl, Refl) | None -> None)
    | Cc a, Cc b -> (match eq a b with Some (Refl, Refl) -> Some (Refl, Refl) | None -> None)
    | _ -> None

  (* Expression generators must be able to refer the stack of values available in a nested read. *)
  type (_, _) stack =
    | [] : (unit, unit) stack
    | (::) : ('x, 'y) t * ('xs, 'ys) stack -> ('x * 'xs, 'y * 'ys) stack
end

module Value = struct
  type 'a t = Value : (_, 'a) Type.t * 'a -> 'a t

  let eq' : type x a. (x, a) Type.t -> a -> a -> bool
  = fun t x y ->
    match t, x, y with
    | Int, x, y -> Int.equal x y
    | T _,  Id.T  (x, _), Id.T  (y, _) -> Uid.equal x y
    | Cc _, Id.Cc (x, _), Id.Cc (y, _) -> Uid.equal x y

  let eq' : type x y a b. (x, a) Type.t -> a -> (y, b) Type.t -> b -> bool
  = fun tx x ty y ->
    match Type.eq tx ty with
    | None -> false
    | Some (Refl, Refl) -> eq' tx x y

  let eq (Value (tx, x)) (Value (ty, y)) = eq' tx x ty y

  let rec stack_eq
  : type sa1 sb1 sa2 sb2. (sa1, sb1) Type.stack -> sb1 -> (sa2, sb2) Type.stack -> sb2 -> bool
  = fun typ1 s1 typ2 s2 ->
    match typ1, typ2 with
    | [], [] -> true
    | t1::typ1, t2::typ2 ->
        let x, s1 = s1 in
        let y, s2 = s2 in
        eq' t1 x t2 y && stack_eq typ1 s1 typ2 s2
    | _ -> false

  type stack = Stack : (_, 's) Type.stack * 's -> stack

  let stack_eq (Stack (typ1, s1)) (Stack (typ2, s2)) =
    stack_eq typ1 s1 typ2 s2
end

module Trace = struct
  type t = {
    read: Uid.t;   (* Identifier of the corresponding [read] *)
    stack: Value.stack;  (* Current environment inside the [read] *)
  }

  let eq a b =
    Uid.equal a.read b.read
    && Value.stack_eq a.stack b.stack

  (* A log of all [read] operations performed by the naive evaluation. *)
  let log : t list ref = ref []

  let grab_log () =
    let t = !log in
    log := [] ;
    List.rev t

  let read ~typ_stack ~s ~uid ~typ t fn =
    Id.read t @@ fun x ->
      let stack = Value.Stack (typ :: typ_stack, (x, s)) in
      log := { read = uid; stack } :: !log;
      fn x
end

module Patch = struct
  type op = Add | Rem

  let diff traces1 traces2 =
    let n, m = Array.length traces1, Array.length traces2 in
    let cache = Array.make_matrix n m None in
    let opt_cons v = function
      | None -> None
      | Some (cost, vs) -> Some (cost + 1, v :: vs)
    in
    let opt_min a b =
      match a, b with
      | Some (a_cost, _), Some (b_cost, _) ->
          if a_cost <= b_cost then a else b
      | None, opt | opt, None -> opt
    in
    let rec go i j =
      if i >= n && j >= m
      then Some (0, [])
      else if i >= n
      then opt_cons (Add, traces2.(j)) (go i (j + 1))
      else if j >= m
      then opt_cons (Rem, traces1.(i)) (go (i + 1) j)
      else match cache.(i).(j) with
           | None ->
             let v = compute i j in
             cache.(i).(j) <- v ;
             v
           | opt -> opt
    and compute i j =
      if Trace.eq traces1.(i) traces2.(j)
      then go (i + 1) (j + 1)
      else opt_min
             (opt_cons (Rem, traces1.(i)) @@ go (i + 1) j)
             (opt_cons (Add, traces2.(j)) @@ go i (j + 1))
    in
    go 0 0

  let diff t1 t2 =
    match diff (Array.of_list t1) (Array.of_list t2) with
    | None -> []
    | Some (_, lst) -> lst

  type t = (op * Uid.t) list

  (* Check that applying [patch] to [traces1] produce [traces2]. *)
  let rec check patch traces1 traces2 =
    match patch, traces1, traces2 with
    | [], [], [] -> true
    | _, t1 :: traces1, t2 :: traces2 when Trace.eq t1 t2 ->
        (* Checking this first guarantees that [patch] is minimal. *)
        check patch traces1 traces2
    | (Rem, uid) :: patch, t1 :: traces1, _ ->
        Uid.equal uid t1.Trace.read
        && check patch traces1 traces2
    | (Add, uid) :: patch, _, t2 :: traces2 ->
        Uid.equal uid t2.Trace.read
        && check patch traces1 traces2
    | _ -> false

  (* An incremental patch produced by [Incr] evaluation *)
  let log : t ref = ref []

  let grab_log () =
    let tr = !log in
    log := [];
    List.rev tr

  let incr_read ~uid t fn =
    Incr.read t @@ fun x ->
      log := (Add, uid) :: !log ;
      Incr.on_release (fun () -> log := (Rem, uid) :: !log);
      let v = fn x in
      v
end


open Crowbar

(* We want to generate a pair of functions: one will process a collection of plain values using Current_incr,
   and the other will process tagged [Id] values (non-incrementally). They should both give equivalent
   results, given equivalent inputs. *)

(* Find all values of type [target] available in the typed stack. *)
let all_values : type a b sa sb. (a, b) Type.t -> (sa, sb) Type.stack -> ((sa -> a) * (sb -> b)) list
= fun target typ_stack ->
  let rec go : type sa2 sb2. (sa2, sb2) Type.stack -> ((sa2 -> a) * (sb2 -> b)) list
  = function
    | [] -> []
    | ty::typ_stack ->
        let rest = go typ_stack in
        let rest = List.map (fun (va, vb) -> (fun (_, s) -> va s), (fun (_, s) -> vb s)) rest in
        match Type.eq target ty with
        | Some (Refl, Refl) -> (fst, fst) :: rest
        | None -> rest
  in
  go typ_stack

(* All values, as well as their total (if the type is [Int] and there is more than one). *)
let all_values : type a b sa sb. (a, b) Type.t -> (sa, sb) Type.stack -> ((sa -> a) * (sb -> b)) list
= fun target typ_stack ->
  let values = all_values target typ_stack in
  match target with
  | Type.Int when List.compare_length_with values 1 > 0 ->
      let left  = (fun s -> List.fold_left (fun acc (x, _) -> acc + x s) 0 values) in
      let right = (fun s -> List.fold_left (fun acc (_, y) -> acc + y s) 0 values) in
      (left, right) :: values
  | _ ->
      values

(* Wrap [all_values] results as crowbar generators. *)
let all_values : type a b sa sb. (a, b) Type.t -> (sa, sb) Type.stack -> ((sa -> a) * (sb -> b)) gen list
= fun target typ_stack ->
  List.map const (all_values target typ_stack)

(* A generator that chooses from [all_values] (if anything is available).
   Logically, this returns an option, but it's convenient to use a list type. *)
let gen_value : type a b sa sb. (a, b) Type.t -> (sa, sb) Type.stack -> ((sa -> a) * (sb -> b)) gen list
= fun target typ_stack ->
  match all_values target typ_stack with
  | [] -> []
  | xs -> [choose xs]

type ('a, 'b, 'sa, 'sb, 'x, 'y) generator =
     ('a, 'b) Type.t         (* Target type *)
  -> ('sa, 'sb) Type.stack   (* Available stack *)
  -> (('sa -> 'x) * ('sb -> 'y)) gen Lazy.t  (* Outcome *)

let rec gen_cc
: type a b sa sb. (a, b, sa, sb, a Incr.cc, b Id.cc) generator
= fun target typ_stack ->
  lazy (
    choose (
      [unlazy (gen_cc'' target typ_stack)]      (* Generate a new value and then process it to get a [Cc target] *)
      @ all_values (Cc target) typ_stack        (* Choose a [Cc target] from the stack *)
      @ (gen_value target typ_stack             (* Choose a [target] from the stack and wrap it as a CC *)
         |> List.map (fun gen ->
              map [gen]
                (fun (a, b) ->
                  let uid = Uid.fresh () in
                  (fun s -> Incr.write (a s)),
                  (fun s -> Id.write ~uid (b s)))))
    )
  )

and gen_cc'
: type a b sa sb. (a, b, sa, sb, a Incr.cc, b Id.cc) generator
= fun target typ_stack ->
  lazy (
    choose (
      [unlazy (gen_cc target typ_stack)]       (* [gen_cc], as above *)
      @ (gen_value Int typ_stack               (* if is_even c then a else b. *)
         |> List.map (fun gen ->
              map [gen; unlazy (gen_cc target typ_stack); unlazy (gen_cc target typ_stack)]
              (fun (cond_a, cond_b) (left_a, left_b) (right_a, right_b) ->
                (fun s -> if cond_a s mod 2 = 0 then left_a s else right_a s),
                (fun s -> if cond_b s mod 2 = 0 then left_b s else right_b s))))
    )
  )

and gen_cc''
: type a b sa sb. (a, b, sa, sb, a Incr.cc, b Id.cc) generator
= fun target typ_stack ->
  lazy (
    choose [
      (* Generate an [int], add it to the stack, and run a CC on that stack.
         We also generate a fresh Uid and log an action using it on the incremental version. *)
      map [unlazy (gen_t Int typ_stack); unlazy (gen_cc' target (Int :: typ_stack))]
        (fun (src_a, src_b) (body_a, body_b) ->
          let uid = Uid.fresh () in
          (fun s -> Patch.incr_read ~uid (src_a s) (fun v -> body_a (v, s))),
          (fun s -> Trace.read ~typ_stack ~s ~uid ~typ:Int (src_b s) (fun v -> body_b (v, s))));

      (* Same, but generate an [int Incr.t] instead of a plain [int]: *)
      map [unlazy (gen_t (T Int) typ_stack); unlazy (gen_cc target (T Int :: typ_stack))]
        (fun (src_a, src_b) (body_a, body_b) ->
          let uid = Uid.fresh () in
          (fun s -> Patch.incr_read ~uid (src_a s) (fun v -> body_a (v, s))),
          (fun s -> Trace.read ~typ_stack ~s ~uid ~typ:(T Int) (src_b s) (fun v -> body_b (v, s))));
    ]
  )

and gen_t
: type a b sa sb. (a, b, sa, sb, a Incr.t, b Id.t) generator
= fun target typ_stack ->
  lazy (
    choose (
      unlazy (gen_t' target typ_stack)          (* Generate a CC, then convert it to an [Incr.t] *)
      :: gen_value (T target) typ_stack         (* Pick an [Incr.t] from the stack *)
    )
  )

and gen_t'
: type a b sa sb. (a, b, sa, sb, a Incr.t, b Id.t) generator
= fun target typ_stack ->
  lazy (
    map [unlazy (gen_cc target typ_stack)]      (* Pick an [Incr.cc] from the stack and convert to [Incr.t] *)
      (fun (a, b) ->
        let uid = Uid.fresh () in
        (fun s -> Incr.of_cc (a s)),
        (fun s -> Id.of_cc ~uid (b s)));
  )

let gen_t typ_stack = unlazy (gen_t' Int typ_stack)


let check_int x y = check_eq ~pp:pp_int ~eq:Int.equal x y

let typ_stack = Type.[ T Int; T Int; T Int ]

let () =
  Crowbar.add_test Crowbar.[ gen_t typ_stack ] @@ fun (incr_eval, id_eval) ->

    (* Initialise with three constants available for use *)
    let id0, id1, id2 = Uid.fresh (), Uid.fresh (), Uid.fresh () in

    let v0 = Incr.var 666666 in
    let v1 = Incr.var 111111 in
    let v2 = Incr.var 424242 in
    let incr_stack = (Incr.of_var v0, (Incr.of_var v1, (Incr.of_var v2, ()))) in

    let id_stack0 = (Id.T (id0, 666666), (Id.T (id1, 111111), (Id.T (id2, 424242), ()))) in

    (* Perform the initial evaluations *)
    assert (Trace.grab_log () = []) ;
    let Id.T (_, r0) = id_eval id_stack0 in
    let trace0 = Trace.grab_log () in

    assert (Patch.grab_log () = []);
    let t = incr_eval incr_stack in
    let patch0 = Patch.grab_log () in

    (* Same result and trace as the naive evaluation *)
    check_int r0 (Incr.observe t);
    check_int (List.length trace0) (List.length patch0);
    List.iter2
      (fun tr (op, read) ->
        assert (op = Patch.Add);  (* No [on_release] on the first run *)
        assert (Uid.equal tr.Trace.read read))
      trace0
      patch0 ;

    let test_change x0 x1 x2 trace0 =
      (* Update and incrementally recompute *)
      Incr.change v0 x0;
      Incr.change v1 x1;
      Incr.change v2 x2;
      Incr.propagate ();
      let patch = Patch.grab_log () in

      let Id.T (_, r1) = id_eval (Id.T (id0, x0), (Id.T (id1, x1), (Id.T (id2, x2), ()))) in
      let trace1 = Trace.grab_log () in

      check_int r1 (Incr.observe t);

      assert (Patch.check patch trace0 trace1);
      (* This [check] should be enough! The following are mostly for sanity. *)

      let optimal_patch = Patch.diff trace0 trace1 in
      check_int (List.length patch) (List.length optimal_patch);

      (* The [optimal_patch] and [patch] are in the same order, however
         the interleaving of the [Add] and [Rem] operations can differ. *)
      let id_add,   id_rem   = List.partition (fun (op, _) -> op = Patch.Add) optimal_patch in
      let incr_add, incr_rem = List.partition (fun (op, _) -> op = Patch.Add) patch in

      check_int (List.length id_add) (List.length incr_add);
      List.iter2
        (fun (op1, tr) (op2, read) ->
          assert (op1 = Patch.Add);
          assert (op2 = Patch.Add);
          assert (Uid.equal tr.Trace.read read))
        id_add
        incr_add ;

      check_int (List.length id_rem) (List.length incr_rem);
      List.iter2
        (fun (op1, tr) (op2, read) ->
          assert (op1 = Patch.Rem);
          assert (op2 = Patch.Rem);
          assert (Uid.equal tr.Trace.read read))
        id_rem
        incr_rem ;

      trace1
    in

    let trace1 = test_change 666666 222222 434343 trace0 in
    let trace2 = test_change 222222 222222 222222 trace1 in
    let trace3 = test_change 666666 434343 424242 trace2 in

    Format.printf "Traces: %i -> %i -> %i -> %i@."
      (List.length trace0)
      (List.length trace1)
      (List.length trace2)
      (List.length trace3)
