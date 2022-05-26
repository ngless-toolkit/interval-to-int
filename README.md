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

## Example Usage

### Step 1: construction

In the first phase, an `IntervalIntMapAccumulator` accumulator is used. This is
a mutable object and it must be used inside a `PrimMonad` (typically either
`IO` or `ST s`). Elements can be inserted into this object.

```haskell
import qualified Data.IntervalIntMap as IM

insertMany :: [(Int, Int)] -> IO (IM.IntervalIntMap Int)
insertMany elems = do
    acc <- IM.new
    forM_ (zip elems [0..]) $ \((s,p), ix) ->
        IM.insert (IM.Interval s p) ix
    IM.unsafeFreeze acc
```

The final step in _construction_ is freezing the accumulator to produce a
`IntervalIntMap`. If the original accumulator is not to be used again, then
`unsafeFreeze` can be used.

### Step 2: usage

The `IntervalIntMap` object is a pure object, with the typical container
functions: `map`, `lookup`, `elems`,...

Do note that the signature for `lookup` is

```haskell
lookup ::  Storable a => Int -> IntervalIntMap a -> [a]
```

Thus, a list is always returned: `[]` if nothing is found, but multiple
intervals can independently overlap with the query.

## Citation

If you do use this repository, please cite the main [NGLess](https://ngless.embl.de) paper:

> _NG-meta-profiler: fast processing of metagenomes using NGLess, a
> domain-specific language_ by Luis Pedro Coelho, Renato Alves, Paulo Monteiro,
> Jaime Huerta-Cepas, Ana Teresa Freitas, Peer Bork, Microbiome (2019)
> [https://doi.org/10.1186/s40168-019-0684-8](https://doi.org/10.1186/s40168-019-0684-8)

LICENSE: MIT

