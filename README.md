[![docs][docs-shield]][docs]
[![OCaml-CI Build Status][ocaml-ci-shield]][ocaml-ci]

* [Current_incr API docs](https://ocurrent.github.io/current_incr/current_incr/Current_incr/index.html)


## Current_incr - Self-adjusting computations

This is a small, self-contained library for self-adjusting (incremental) computations.
It was written for OCurrent, but can be used separately too.

It is based on the paper [Adaptive Functional Programming](https://www.cs.cmu.edu/~guyb/papers/popl02.pdf).

It is similar to Jane Street's incremental library, but much smaller and
has no external dependencies.

It is also similar to the react library, but the results do not depend on the
behaviour of the garbage collector. In particular, functions stop being called
as soon as they are no longer needed.

## Installation

Use `opam install current_incr` to install the library.

## Example

```ocaml
# #require "current_incr";;

# let total = Current_incr.var 10;;
val total : int Current_incr.var = <abstr>
# let complete = Current_incr.var 5;;
val complete : int Current_incr.var = <abstr>

# let status =
    Current_incr.of_cc begin
      Current_incr.read (Current_incr.of_var total) @@ function
      | 0 -> Current_incr.write "No jobs"
      | total ->
        Current_incr.read (Current_incr.of_var complete) @@ fun complete ->
        let frac = float_of_int complete /. float_of_int total in
        Printf.sprintf "%d/%d jobs complete (%.1f%%)"
                       complete total (100. *. frac)
        |> Current_incr.write
    end;;
val status : string Current_incr.t = <abstr>
```

This defines two input variables (`total` and `complete`) and a "changeable computation" (`status`) whose output depends on them.
At the top-level, we can observe the initial state using `observe`:

```ocaml
# print_endline @@ Current_incr.observe status;;
5/10 jobs complete (50.0%)
- : unit = ()
```

Unlike a plain ref cell, a `Current_incr.var` keeps track of which computations depend on it.
After changing them, you must call `propagate` to update the results:

```ocaml
# Current_incr.change total 12;;
- : unit = ()
# Current_incr.change complete 4;;
- : unit = ()
# print_endline @@ Current_incr.observe status;;      (* Not yet updated *);;
5/10 jobs complete (50.0%)
- : unit = ()

# Current_incr.propagate ();;;
- : unit = ()
# print_endline @@ Current_incr.observe status;;
4/12 jobs complete (33.3%)
- : unit = ()
```

Computations can have side-effects, and you can use `on_release` to run some compensating action if the computation needs to be undone later. Here's a function that publishes a result, and also registers a compensation for it:

```ocaml
let publish msg =
  Printf.printf "PUBLISH: %s\n%!" msg;
  Current_incr.on_release @@ fun () ->
  Printf.printf "RETRACT: %s\n%!" msg
```

It can be used like this:

```ocaml
# let display = Current_incr.map publish status;;
PUBLISH: 4/12 jobs complete (33.3%)
val display : unit Current_incr.t = <abstr>

# Current_incr.change total 0;;;
- : unit = ()
# Current_incr.propagate ();;
RETRACT: 4/12 jobs complete (33.3%)
PUBLISH: No jobs
- : unit = ()
```

A major difference between this and the react library is that current_incr does not depend on the garbage collector to decide when to stop a computation. In react, you'd have to be careful to make sure that `display` didn't get GC'd (even though you don't need to refer to it again) because if it did then the output would stop getting updated. Also, setting `total` to `0` in react might cause the program to crash with a division-by-zero exception, because the `frac` computation will continue running until it gets GC'd, even though it isn't needed for anything now.

Current_incr's API is pretty small. You might want to wrap it to provide extra features, e.g.

- Use of a result type to propagate errors.
- Integration with Lwt to allow asynchronous computations.
- Static analysis to render your computation with graphviz.
- Persistence of state to disk.

If you need that, consider using the [OCurrent](https://github.com/ocurrent/ocurrent) library,
which extends current_incr with these features.

## Contributing

You will need to run `dune runtest` to validate your changes. A more extensive verification is provided with [`crowbar`](https://github.com/stedolan/crowbar) to generate arbitrary computations and check their behavior and results. This step requires [`afl-fuzz`](https://lcamtuf.coredump.cx/afl/) and an ocaml compiler with the `+afl` option enabled, for example:

```shell
$ opam switch create 4.12.1-afl --packages=ocaml-variants.4.12.1+options,ocaml-option-afl --repos=default,beta
```

Before running the tests, `afl-fuzz` requires an `input/` folder with some initial random data:

```shell
$ mkdir input
$ head -c 512 /dev/urandom > input/rnd
```

We recommend leaving `afl-fuzz` running for hours before concluding that your patch is fine:

```shell
$ dune build test/exhaust.exe
$ afl-fuzz -m 500 -i input -o output _build/default/test/exhaust.exe @@
```

Any issue will be reported in the `output/crashes/` folder. The crashing test cases can then be reproduced with:

```shell
$ ./_build/default/test/exhaust.exe -i ./output/crashes/id:000000,...

$ find output/crashes/ -name 'id:*' -exec ./_build/default/test/exhaust.exe -i '{}' ';'
```

Note that the fuzzer may report crashes due to memory consumption or a bad input file (*"testcase was invalid: premature end of file"*), which aren't indicative of an issue with your changes.

[docs-shield]:https://img.shields.io/badge/doc-online-blue.svg
[docs]: https://ocurrent.github.io/current_incr/

[ocaml-ci]: https://ci.ocamllabs.io/github/ocurrent/current_incr
[ocaml-ci-shield]: https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focurrent%2Fcurrent_incr%2Fmaster&logo=ocaml
