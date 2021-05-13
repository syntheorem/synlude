# synlude

This package provides a more minimal Haskell prelude with the following differences from the standard `Prelude`:

* No partial functions.

* No functions that actually perform IO. These should be imported from `System.IO`.

* Most of the list-specific functions are not exported, and should be imported from `Data.List`. This is because the function names conflict with function names from other data structures, so should be qualified.

* Export more functor/applicative/monad combinators.

* Export a few other types and functions that the base prelude doesn't but I consider fundamental.

* Only export the most general versions of functions, when possible. If existing functions could be more general, redefine them.

* Rename certain `xxxM` functions to `xxxA` since they now only require `Applicative`.

* More generalized exception support plus better assertions, found in `Synlude.Exception`.

To be honest, I don't really expect anyone else to use this, but I think it makes Haskell more pleasant to use for my own projects. I know there are a lot of alternative preludes out there, but most of them go the direction of putting far *more* stuff in the prelude, which isn't what I was looking for.
