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

## Citation

If you do use this repository, please cite the main [NGLess](https://ngless.embl.de) paper:

> _NG-meta-profiler: fast processing of metagenomes using NGLess, a
> domain-specific language_ by Luis Pedro Coelho, Renato Alves, Paulo Monteiro,
> Jaime Huerta-Cepas, Ana Teresa Freitas, Peer Bork, Microbiome (2019)
> [https://doi.org/10.1186/s40168-019-0684-8](https://doi.org/10.1186/s40168-019-0684-8)

LICENSE: MIT

