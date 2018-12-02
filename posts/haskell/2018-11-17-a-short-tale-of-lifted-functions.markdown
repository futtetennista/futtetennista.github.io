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

What's going on? Let's apply the reduction rules to
understand why the exception was thrown

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

The catch here is that effects are always evaluated. The lifted `(&&)`
will be lazy then in the evaluation of the **values** but not in the
evaluation of the **effects**. This might be counterintuitive or surprising
at first glance: `(&&)` appears to "lose some of its laziness" when
it's lifted. If effects need to be lazily evaluated, we need to
write our own "lazy lifted" version of `(&&)`:

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
Lifting a pure function adds an "effectful dimension" to it:
when it comes to how the lifted function evaluates _values_ think about
the semantics of the pure function, when it comes to how the lifted
function evaluates _effects_ think about the semantics of the
underlying type class.
