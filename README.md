# IntervalIntMap

An interval map structure that is optimized for low memory (each interval is
represented by about 3 words + whatever the cargo is) and has semantics that
are appropriate for genomic intervals (namely, intervals can overlap and
queries will return **all** matches together). It also designed to be used in
two phases: a construction phase + query phase).

This is not a general purpose package, it serves mostly as support for
[NGLess](https://ngless.embl.de) and is used there.

Do get [in touch](mailto:luis@luispedro.org) if you want to use it more
generally, but the plans for this repo is to develop it only in so far as it
helps with NGLess' goals.

LICENSE: MIT

