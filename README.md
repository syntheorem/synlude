# prelude-prime

This package provides a more minimal Haskell prelude with the following goals:

* Only depend on `base` (and `deepseq`, which I feel should be in `base`).

* No partial functions.

* No functions that actually perform IO. These should be imported from `System.IO`.

* Most of the list-specific functions are not exported, and should be imported from `Data.List`.
This is because the function names conflict with function names from other data structures, so should be qualified.

* Export more functor/applicative/monad combinators.

* Export a few other types and functions that the base prelude doesn't but I consider fundamental.

* Only export the most general versions of functions, when possible. If existing functions could be more
general, redefine them.

* Rename certain `xxxM` functions to `xxxA` since they now only require `Applicative`.

* Provide categorized modules of additional base prelude declarations that are not exported by `PreludePrime`
under the `PreludePrime.*` modules.
