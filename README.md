## Current_incr - Self-adjusting computations

This is a small, self-contained library for self-adjusting (incremental) computations.
It was written for OCurrent, but can be used separately too.

It is similar to Jane Street's incremental library, but much smaller and
has no external dependencies.

It is also similar to the react library, but the results do not depend on the
behaviour of the garbage collector. In particular, functions stop being called
as soon as they are no longer needed.
