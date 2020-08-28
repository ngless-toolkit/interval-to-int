# IntervalIntMap

An interval map that works only with `Int`s. Unlike the more general purpose
_interval map_ packages, it is optimized for memory (each interval is
represented by about 3 words) and has semantics that are appropriate for
genomic intervals (namely, intervals can overlap and queries will return
**all** matches together).

This is not a general purpose package, it serves mostly as support for
[NGLess](https://ngless.embl.de) and is used there.

Do get [in touch](mailto:luis@luispedro.org) if you want to use it more
generally, but the plans for this repo is to develop it only in so far as it
helps with NGLess' goals.


