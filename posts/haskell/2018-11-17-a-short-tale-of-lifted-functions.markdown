---
title: A short tale of lifted functions
author: futtetennista
tags: lazy+evaluation, haskell
---

This is another short tale about lazy evaluation but this time it is about
a lack thereof, a moment of brief despair and confusion and how the
underlying simplicity of a purely functional language like Haskell comes
to the rescue.
<!--more-->
I guess every software developer is familiar with the boolean `AND` which is
the `(&&)` function in Haskell. In an effectful context, we can lift `(&&)`
and use it without having to rewrite it using combinators like `liftA2` or
`liftM2`. Let's take some simple example usages:
```haskell
import Data.Maybe (fromMaybe)

ƛ: liftA2 (&&) (pure False) (const True <$> fromMaybe undefined Nothing)
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:10:53 in interactive:Ghci3
```

If you are puzzled by that welcome to the club. If not, bravo: you have not
being tricked by the nuances of the interaction of applicative functors and
lazy functions. What's going on here? One of the awesome things about a purely
functional language is that the way expressions are evaluated is simple, namely
by using reduction rules so we can use those rules and understand why the
exception was thrown

```haskell
-- definition of liftA2
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = fmap f x <*> y

-- evaluation
1. liftA2 (&&) (pure False) (const True <$> fromMaybe undefined Nothing)
2. fmap (&&) (pure False) <*> (const True <$> fromMaybe undefined Nothing)
3. pure (&& False) <*> (const True <$> fromMaybe undefined Nothing)
4. pure (&& False) <*> (const True <$> fromMaybe undefined Nothing)
5. liftA2 id (pure (&& False)) (const True <$> fromMaybe undefined Nothing)
6. fmap id (pure (&& False)) (const True <$> fromMaybe undefined Nothing)
7. pure (&& False) (const True <$> undefined)
*** Exception: Prelude.undefined
```

The catch here is that applicative functors are "dumb" when it comes to
evaluate computations: they are always evaluated. The lifted `(&&)` will be
lazy then in the evaluation of the **values** but not in the
evaluation of the **effects**. This might be counterintuitive or surprising
at a first glance since `(&&)` appears to "lose some of its laziness" when
it is lifted to an applicative functor. A deeper analysis of what we want to
achieve reveals that in this situation we don't really want an applicative
functor, we want something a bit more powerful, i.e. a monad:

```haskell
(.&&) f g = do
  p <- f
  if p
  then do { p' <- g ; pure (p && p') }
  else pure p

ƛ: (.&&) (pure False) (const True <$> fromMaybe undefined Nothing)
False
```

## Wrapping up
Lifting a pure function adds a "effectful dimension" to it:
when it comes to how the lifted function evaluates _values_ think about
the semantics of the pure function, when it comes to how the lifted
function evaluates _side effects_ think about the semantics of the
underlying type class.
