# nonempty-alternative

Non-empty lists from `semigroups` can be naturally extended to an
`Alternative` interface since it contains the `empty` element. Many
instances arise naturally from this interface, like:

- `Applicative`
- `Monad`
- `Comonad`

The instances have the goal of being classes morphisms between
different representation of the same structure. This probably depends
on the structure.

This generalization allows the use of non-empty `List`, `ZipList`,
`Vector`, etc...

This module contains both `Left` and `Right` variants of `NonEmpty`.

## TODO

- Make test suite to test the classes laws.
