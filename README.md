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

## Assertions

`PreludePrime` also exports several assert functions. Unlike `assert` from `base`, these are not controlled
by `-fignore-asserts`, but rather are enabled or disabled by the `assert` flag that this package provides.
There are two reasons for this:

* The `assert` provided by `base` isn't flexible; you can't pass a custom message, and you can't define
assert functions on top of it (such as `assertIO`) because the location in the printed message will be
based on the call to `assert` rather than the call to the function mapping it. Using GHC's `HasCallStack`,
we can do better.

* GHC automatically disables asserts when optimizations are enabled at all, which in my opinion is the
wrong behavior. Generally I want my test builds to be optimized but also have assertions enabled. You
can set `-fno-ignore-asserts` to get around this, but that will only work for the current project, not
its dependencies. On the other hand, everything using assertions from `prelude-prime` will have them
enabled or disabled based on the flag, which you can set in your `stack.yaml` or via the command line.
