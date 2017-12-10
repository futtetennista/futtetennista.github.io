---
title: Free Finite-State Machines
author: futtetennista
tags: fsm, free+monads, haskell
---

In a series of [blog][1] [posts][2] [Oskar Wickström][3] explains how to make state
transitions explicit using types. Those are brilliant articles in my opinion and
I encourage you to read them first before diving into this article; particularly
in the second installment he shows how to separate a *program* from its *protocol*
in such a way that

<blockquote>
<p>the set of states, and the associated state transitions for certain events,
will be encoded separately from the implementation of the automaton</p>
<\blockquote>

to have compile-time guarantees that the program respects the given protocol, and
that transitioning to illegal states doesn't type-check. He does it using a mix
of type classes, monad transformers stacks, GADTs and type-level functions.
An alternative way to achieve the same goal is to build a finite-state machine on
top of a free monad: it's just an alternative way of expressing effects in Haskell.
One of the nice consequences of this choice is that not only we'll have compile-time
guarantees of the correctness of our code, but by expressing the protocol as a
grammar, its actions and user interactions are expressed in a clean and explicit
way.
<!--more-->

There are [many][4] [very][5] [good][6] [articles][7] about free monads and I won't
describe them in too much detail, but essentially they are a way to build monads
from functors with the help of a special type, the `Free` type.
When writing a program using free monads usually we think of it as a grammar, and
need to define:

* the data type that expresses its symbols - this data type will be our functor instance
* the program itself and usually a dsl to build it using smart constructors and
* one or more interpreters to "translate" the program in a specific way, for example
there could be an interpreter for a command-line app, one for tests, one for a remote API etc.

Back to the original example, we can look at the
[state machine diagram](https://wickstrom.tech/generated/uml/checkout.svg)
that describes the protocol and think of it in terms of a grammar, legal symbols are

* `select`
* `checkout`
* `cancel`
* `selectCard`
* `confirm`
* `placeOrder`
* `start`
* `end`

The `start` and `end` symbols are not explictly written down in the aforementioned diagram, but it's
useful to have them around in our grammar because we need a way to kick off and terminate the state
machine. We can now define the following data type to express this grammar:

\ignore{
\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
import Control.Monad.Free
import Data.List.NonEmpty ((<|), NonEmpty(..))
import Control.Monad (void)


main :: IO ()
main = void $ terminalInterpreter checkoutProgram

type CartItem = String

type Card = String

type OrderId = Integer
\end{code}
}

\begin{code}
data CheckoutProtocolF s r
  = Start r
  | Select (CartItem -> r)
  | Checkout (CheckoutState HasItems) r
  | Cancel (CancelState s) (CheckoutState HasItems -> r)
  | SelectCard (CheckoutState NoCard) (Card -> r)
  | Confirm (CheckoutState CardSelected) r
  | PlaceOrder (CheckoutState CardConfirmed) (OrderId -> r)
  | Finish r
  deriving Functor
\end{code}

If you read that [second article][2], you'll remember that in two places the program
needed  to read from standard input and decide how to proceed based on it.
Now this is not part of the protocol itself, but we need to model it somehow.
Let's call it interactions and model it with the following data type:

\begin{code}
data CheckoutInteractionF s r
  = AskSelectMore (SelectState s) (Bool -> r)
  | AskConfirmCard Card (Bool -> r)
  deriving Functor
\end{code}

The checkout now can be modelled as:

\begin{code}
data CheckoutF s r
  = Protocol (CheckoutProtocolF s r)
  | Interaction (CheckoutInteractionF s r)
  deriving Functor

type CheckoutM s = Free (CheckoutF s)
\end{code}

`CheckoutF` models protocol and user interactions and it's the functor that we'll
use to create our free monad `CheckoutM`. The program is amost identical to
the one described in [Oskar's article][2]:


\ignore
\begin{code}
initial :: CheckoutM s (CheckoutState NoItems)
initial = liftF $ Protocol (Start NoItems)

askSelectMore :: SelectState s -> CheckoutM s Bool
askSelectMore sst =liftF $ Interaction (AskSelectMore sst id)

select :: SelectState s -> CheckoutM s (CheckoutState HasItems)
select (NoItemsSelect NoItems) = liftF $ Protocol $ Select (\i -> HasItems (i :| []))
select (HasItemsSelect (HasItems is)) = liftF $ Protocol $ Select (\i -> HasItems (i <| is))

checkout :: CheckoutState HasItems -> CheckoutM s (CheckoutState NoCard)
checkout items@(HasItems is) = liftF $ Protocol $ Checkout items (NoCard is)

selectCard :: CheckoutState NoCard -> CheckoutM s (CheckoutState CardSelected)
selectCard cst@(NoCard is) = liftF $ Protocol $ SelectCard cst (CardSelected is)

askConfirm :: CheckoutState CardSelected -> CheckoutM s Bool
askConfirm (CardSelected _ cc) = liftF $ Interaction $ AskConfirmCard cc id

confirm :: CheckoutState CardSelected -> CheckoutM s (CheckoutState CardConfirmed)
confirm st@(CardSelected is cc) = liftF $ Protocol $ Confirm st (CardConfirmed is cc)

placeOrder :: CheckoutState CardConfirmed -> CheckoutM s (CheckoutState OrderPlaced)
placeOrder st = liftF $ Protocol $ PlaceOrder st OrderPlaced

cancel :: CancelState s -> CheckoutM s (CheckoutState HasItems)
cancel (NoCardCancel (NoCard items)) =
  select $ HasItemsSelect (HasItems items)
cancel (CardSelectedCancel (CardSelected items _card)) =
  select $ HasItemsSelect (HasItems items)
cancel (CardConfirmedCancel (CardConfirmed cart _)) =
  select $ HasItemsSelect (HasItems cart)

end :: CheckoutState OrderPlaced -> CheckoutM s OrderId
end (OrderPlaced oid) = liftF $ Protocol (Finish oid)
\end{code}
}

\begin{code}
checkoutProgram :: CheckoutM s OrderId
checkoutProgram =
  initial >>= fillCart >>= startCheckout >>= end
  where
    fillCart :: CheckoutState NoItems -> CheckoutM s (CheckoutState HasItems)
    fillCart st = select (NoItemsSelect st) >>= selectMoreItems

    selectMoreItems :: CheckoutState HasItems -> CheckoutM s (CheckoutState HasItems)
    selectMoreItems st = do
      more <- askSelectMore (HasItemsSelect st)
      if more then select (HasItemsSelect st) >>= selectMoreItems else return st

    startCheckout :: CheckoutState HasItems -> CheckoutM s (CheckoutState OrderPlaced)
    startCheckout (HasItems items) = do
      st@(CardSelected items cc) <- selectCard (NoCard items)
      useCard <- askConfirm st
      if useCard then confirm st >>= placeOrder else redo (CardSelectedCancel st)
      where
        redo st = cancel st >>= selectMoreItems >>= startCheckout
\end{code}

And the types are also essentially the same with one difference: when using free
monads the state doesn't need to be expressed as a type-level function because
the generality of this solution comes from implementing different interpreters.

\begin{code}
data NoItems
data HasItems
data NoCard
data CardSelected
data CardConfirmed
data OrderPlaced

data SelectState s
  = NoItemsSelect (CheckoutState NoItems)
  | HasItemsSelect (CheckoutState HasItems)

data CancelState s
  = NoCardCancel (CheckoutState NoCard)
  | CardSelectedCancel (CheckoutState CardSelected)
  | CardConfirmedCancel (CheckoutState CardConfirmed)

data CheckoutState s where
  NoItems :: CheckoutState NoItems
  HasItems :: NonEmpty CartItem -> CheckoutState HasItems
  NoCard :: NonEmpty CartItem -> CheckoutState NoCard
  CardSelected :: NonEmpty CartItem -> Card -> CheckoutState CardSelected
  CardConfirmed :: NonEmpty CartItem -> Card -> CheckoutState CardConfirmed
  OrderPlaced :: OrderId -> CheckoutState OrderPlaced
\end{code}

Let's have a partial look at an interpreter to run our program in a terminal:

\begin{code}
terminalInterpreter :: CheckoutM s res -> IO res
terminalInterpreter =
  foldFree morph
  where
    morph :: CheckoutF s res -> IO res
    morph (Protocol (Start next)) = print "Welcome!" >> return next
    morph (Interaction (AskSelectMore s next)) =
      print "More items? (y/n)" >> getLine >>= return . next . (=="y")
    morph (Protocol (Select next)) =
      loop
      where
        loop = do
          print "Enter item:"
          xs <- getLine
          if null xs
            then print "Invalid item" >> loop
            else print ("'" ++ xs ++ "' selected") >> return (next xs)
    morph (Protocol (Confirm _ next)) = return next
    morph (Protocol (Checkout (HasItems is) next)) =
      print (show is) >> return next
    morph (Protocol (Cancel st next)) =
      case st of
        NoCardCancel (NoCard is) -> return $ next (HasItems is)
        CardSelectedCancel (CardSelected is _) -> return $ next (HasItems is)
        CardConfirmedCancel (CardConfirmed is _) -> return $ next (HasItems is)
    morph (Interaction (AskConfirmCard cc next)) =
      print ("Confirm use of '" ++ cc ++ "' (y/n)?") >> getLine >>= return . next . (== "y")
    morph (Protocol (PlaceOrder (CardConfirmed is cc) next)) = do
      oid <- placeOrderApi
      print $ "Order nr. " ++ show oid ++ " placed! Congrats you just bought: "
         ++ show is ++ " (using your credit card: " ++ cc ++ ")"
      return $ next oid
      where
        placeOrderApi :: IO Integer
        placeOrderApi = return 6
    morph (Protocol (SelectCard (NoCard items) next)) =
      print "Enter card:" >> getLine >>= return . next
    morph (Protocol (Finish next)) = print "Goodbye!" >> return next
\end{code}

And here we go: we have the same compile-time guarantees of the correctness of
our code and we can leverage the power of free monads to easily create different
ways of running it.


Wrapping up
---

This post show an alternative way to express what [the original article][2] calls
"explicit typed state transitions" using finite-state machines on top of free
monads. Keeping in mind [the][8] [tradeoffs][9] of solutions based
on free monads, I must admit that it does appeal to me more than a
solution based on type classes and monad transformers because I feel it
expresses the protocol more explicitly and it enforces a better separation
between protocol and interactions with the real-world; or maybe I just got
influenced too deeply by [John De Goes'][6] [articles][7] where he vigorously
exposes why he thinks free monads are the way modern functional programs should
be written. That said, this solution is not necessarily better than the one based
on type classes and monad transformer stacks and doesn't change the core benefit of
this solution: having *compile-time* guarantees about the correctness of our code.


This article is written in literate Haskell, feel free to [download][10] it and
try it out (GHC and the
[free package](https://hackage.haskell.org/package/free-4.12.4)
need to be installed in your system)!

[1]: https://wickstrom.tech/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html
[2]: https://wickstrom.tech/finite-state-machines/2017/11/19/finite-state-machines-part-2.html
[3]: https://twitter.com/owickstrom
[4]: http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
[5]: http://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html
[6]: http://degoes.net/articles/modern-fp
[7]: http://degoes.net/articles/modern-fp-part-2
[8]: https://markkarpov.com/post/free-monad-considered-harmful.html
[9]: http://tech.frontrowed.com/2017/09/28/benching-free/
[10]: https://github.com/futtetennista/futtetennista.github.io/blob/master/posts/haskell/2017-12-22-free-fsm.lhs
