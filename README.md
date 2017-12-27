# prelude-prime

This package provides a Haskell prelude with the following goals:

* Only depend on base.

* Be far more minimal in what is exported than in the base prelude. Only export commonly used or very fundamental
types, functions, classes, and operators. I want it to be less surprising about what is in scope. In particular,
most list operations should just be imported from `Data.List`. Also avoid partial functions.

* At the same time, export some fundamental things that the base prelude doesn't export, particularly
some additional applicative and monadic functions.

* Define and only export more general versions of functions, when possible.

* Rename certain `xxxM` functions to `xxxA` since they now only require `Applicative`.

* Provide categorized modules of additional base prelude declarations that are not exported by `PreludePrime`
under the `PreludePrime.*` modules.
