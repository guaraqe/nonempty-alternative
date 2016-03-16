# nonempty-alternative

This package extends `NonEmpty` from `semigroups` to arbitrary
`Alternative` types. The method is the same as for lists, by
separating an element from the rest.

There are two natural ways to merge an element `x` to the rest of the
structure `xs`. The first gives rise to `NonEmptyL`:

    flattenL :: NonEmptyL f a -> f a
    flattenL (x :<: xs) = pure x <|> xs

The second gives rise to `NonEmptyR`:

    flattenR :: NonEmptyR f a -> f a
    flattenR (xs :>: x) = xs <|> pure x

The instances are made so that `flattenL` gives a type class morphism
between `NonEmptyL List` and `List`, and `flattenR` gives the same for
`NonEmptyR RList` and `RList` from the package `rlist`.

## TODO

- Make test suite to test the classes laws.
