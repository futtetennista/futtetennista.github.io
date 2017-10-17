---
title: Functional programming with graphs
tags: haskell, graphs
categories: data+strutures, algorithms
---

Graphs are a fundamental data structure in Computer Science because they allow
to model lots of problem in a natural and elegant way. There is plenty of
literature available on problems related to graphs, i.e. traversing,
finding the shortest path between two vertices, minimum spanning tree etc.
Plenty of literature if you look at the imperative world that is, what about the
functional world?

<!--more-->

Perhaps surprisingly, graphs algorithms have been something that functional
programming had a hard time to deal with, All those aforementioned algorithms
heavily rely on state and side effects to achieve an efficient solution and this
is obviously a problem in a purely functional langauge. The use of monads might
put a patch on this issue since they make it possible to mutate state by retaining
referential transparency but the code looks imperative and it doesn't take advantage
of the goodness that functional programming offers. In the [Haskell wiki](https://wiki.haskell.org/Research_papers/Data_structures#Graphs)
there are a few links to research papers that tackle this problem and I would like
to give an overview of two of those, namely
["Structuring Depth First Search Algorithms in Haskell"](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.52.6526)
by David King and John Launchbury and
["Inductive Graphs and Functional Graph Algorithms"](http://web.engr.oregonstate.edu/~erwig/papers/abstracts.html#JFP01)
by Martin Erwig.

## The imperative approach with monads
It is possible to literally translate imperative algorithms using the monads and it's
also definitely possible to make the following code more efficient - i.e. by using
data structures other that lists and unboxed types - but the core of the
algorithm will look similar.

``` haskell
#!/usr/bin/env stack
{-
stack script
--resolver lts-9.6
--package mtl
--package containers
--package vector
--package primitive
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Foldable (foldlM)
import qualified Data.Vector.Mutable as MV
import qualified Data.Sequence as Seq
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify', get, lift)
import Control.Monad.ST (ST, runST)
import Control.Applicative (liftA2)
import Control.Monad.Primitive (PrimMonad)

data Graph w a = Graph [(a, [EdgeNode w a])] Directed Int deriving Eq

type EdgeNode w a = (a, w)

type VertexState s = MV.MVector s VState

data DFSState s a w =
  DFSState { dfsVertex :: a
           , dfsConnectedComponent :: ConnectedComponent a w
           , dfsVertexState :: VertexState s
           }

data VState = Undiscovered | Discovered | Processed deriving (Show, Eq, Ord)

type ConnectedComponent w a = Tree w a

data Tree w a = Nil | Node !a ![(w, Tree w a)] deriving (Show, Eq)

-- Let's assume for simplicity that vertices and weights are integers
dfs :: Graph Int Int -> [ConnectedComponent Int Int]
dfs g =
  runST $ do
    vstates <- MV.replicate (verticesCount g) Undiscovered
    loop (vertices g) vstates
  where
    loop :: forall s. [Int]
         -> MV.MVector s VState
         -> ST s [ConnectedComponent Int Int]
    loop vs vstates = do
      mv <- findNextUndiscoveredVertex vstates
      maybe (return []) processVertex mv
        where
          processVertex v =
            liftA2 (:) (evalStateT dfs' (DFSState v (Node v []) vstates))
                       (loop vs vstates)

    dfs' :: StateT (DFSState s Int Int) (ST s) (ConnectedComponent Int Int)
    dfs' = do
      DFSState v tree vstates' <- get
      MV.write vstates' v Discovered
      tree' <- foldlM (\tree' edge@(v', _) -> do
                          vstate <- MV.read vstates' v'
                          lift $ processEdgeNode v tree' vstate edge)
                      tree
                      (adjacent v g)
      MV.write vstates v Processed
      modify' (\s -> s{ dfsConnectedComponent = tree' })
      gets dfsConnectedComponent

    processEdgeNode :: Int
                    -> Tree Int Int
                    -> VState
                    -> EdgeNode Int Int
                    -> ST s (Tree Int Int)
    processEdgeNode v tree Undiscovered edgeNode@(v', _) =
      evalStateT dfs' (DFSState v' (buildTree v edgeNode tree) vstates)
    processEdgeNode _ tree Discovered _ = return tree
    processEdgeNode _ tree Processed _ = return tree

    findNextUndiscoveredVertex :: forall (m :: * -> *). PrimMonad m
                               => MV.MVector (PrimState m) VState
                               -> m (Maybe Int)
    findNextUndiscoveredVertex vstates =
      go 0 (MV.length vstates)
      where
        go idx size
          | idx == size = return Nothing
          | otherwise = do
              vstate <- MV.read vstates idx
              case vstate of
                Undiscovered -> return (Just idx)
                _ -> go (idx + 1) size

```

You can make up your mind about the elegance and clarity of this solution.

## Depth-fist search & non-strict semantics
In ["Structuring Depth First Search Algorithms in Haskell"](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.52.6526)
David King and John Launchbury aim to express depth-first search using the functional
style to achieve comparable performance but with greater composability and easiness
to formally proof the critical properties of their algorithms. They reduce the
problem of traversing a graph to a combinatorial problem and use a common technique
in languages with non-strict semantics: generate and prune. Their depth-first
search algorithm first generates (on-demand) all possible trees from the input
graph but discards the ones that are not valid thus making sure that no unnecessary
evaluation will take place and it uses a mutable array to keep track of the state
of each vertex.

``` haskell
#! /usr/bin/env stack
{-
stack script
--resolver lts-9.6
--package array
--package containers
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

import Data.Array (accumArray, bounds, indices)
import qualified Data.Array.ST as MA
import Control.Monad.ST

type Table = Array Vertex

type Graph = Table [EdgeNode]

-- Again let's assume for simplicity that vertices and weights are integers
type Vertex = Int

type Weight = Int

type Bounds = (Vertex, Vertex)

buildG :: Bounds -> [(Vertex, EdgeNode)] -> Graph
buildG = accumArray (flip (:)) []

generate :: Graph -> Vertex -> Tree Vertex
generate g v = Node v (map (generate g . fst) (g ! v))

mkEmpty :: (Ix i, MA.MArray (MA.STUArray s) Bool m)
        => (i, i)
        -> m (MA.STUArray s i Bool)
mkEmpty bnds = MA.newArray bnds False

contains :: (Ix i, MA.MArray (MA.STUArray s) Bool m)
         => MA.STUArray s i Bool
         -> i
         -> m Bool
contains = MA.readArray

include :: (Ix i, MA.MArray a Bool m) => a i Bool -> i -> m ()
include arr v = MA.writeArray arr v True

dfs :: Graph -> [Vertex] -> Forest Vertex
dfs g = prune (bounds g) . map (generate g)
  where
    prune :: Bounds -> Forest Vertex -> Forest Vertex
    prune bnds ts =
      runST $ do
        s <- mkEmpty bnds :: forall s. ST s (MA.STUArray s Vertex Bool)
        chop ts s

    chop :: (MA.MArray (MA.STUArray s) Bool m)
         => Forest Vertex
         -> MA.STUArray s Vertex Bool
         -> m (Forest Vertex)
    chop [] arr = return []
    chop (Node v ts:ns) arr = do
      visited <- contains arr v
      if visited
        -- ignore ts
        then chop ns arr
        else do
          include arr v
          -- traverse left-to-right
          ts' <- chop ts arr
          -- traverse top-to-bottom
          ns' <- chop ns arr
          return $ Node v ts' : ns'
```

A few things that can be pointed out: the graph is implemented using an adjacency
list - usually the preferred way of representing graphs in the imperative world -
so that should not be alien to imperative programmers. The authors note that the
mutable array could be replaced by `Set` (this will add a logarithmic factor to
its complexity though). Using that definition of depth-first search the authors
define other graph algorithms using composition:


``` haskell
-- add imports
import Data.List ((\\))
import Data.Sequence (Seq, (><), (|>))
import Data.Sequence
import Data.Foldable (toList)

topsort :: Graph -> [Vertex]
topsort =
  reverse . postOrd

postorder :: Tree a -> Seq a
postorder (Node x ts) = postorderF ts |> x

postorderF :: Forest a -> Seq a
postorderF = foldr ((><) . postorder) Seq.empty

postOrd :: Graph -> [Int]
postOrd = toList . postorderF . dff

preorder :: [a] -> Tree a -> [a]
preorder vs (Node v ts) = preorderF (v:vs) ts

preorderF :: [a] -> Forest a -> [a]
preorderF = reverse . foldr (flip preorder)

preOrd :: Graph -> [Vertex]
preOrd = preorderF [] . dff

-- build graph of edge types
-- O(T)
mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f tree = array (bounds tree) [(v, f v (tree ! v)) | v <- indices tree]

-- O(TS^2)
tree :: Bounds -> Forest Vertex -> Graph
tree bnds ts = buildG bnds (concatMap flat ts)
  where
    -- #ts + #ts^2 = O(TS^2)
    flat (Node v ts) =
      [(v, (v', 0)) | Node v' _ <- ts] ++ concatMap flat ts

-- O(V * E)
back :: Graph -> Table Vertex -> Graph
back g postord = mapT select g
  where
    select v es =
      [(v', w) | (v', w) <- es, postord ! v < postord ! v']


-- O(V * E)
cross :: Graph -> Table Vertex -> Table Vertex -> Graph
cross g preord postord = mapT select g
  where
    select v es =
      [(v', w) | (v', w) <- es, postord ! v > postord ! v' && preord ! v > preord ! v']


-- O(V * E{g} * E{treeG})
forward :: Graph -> Graph -> Table Vertex -> Graph
forward g treeG preord = mapT select g
  where
    -- #edges{g} + #edges{g} * #edges{treeG} = O(E{g} * E{treeG})
    select v es =
      [(v', w) | (v', w) <- es, preord ! v < preord ! v'] \\ treeG ! v
```

The algorithms to classify edges might become clearer by looking at this diagram
![Classifying edges diagram](/images/classifying_edges_diagram.png)

One last note: section 5 (Implementing depth-first search) also states that:

> The choice of pruning patterns determines whether the forest ends up being
> depth-first (traverse in a left-most, top-most fashion) or breadth-first (
> top-most, left-most)

but without providing any code for it and I honestly could not wrap my head around
on how to write a breadth-first traversal with the algorithm they propose. If
anybody has some pointers please [let me know]("mailto:futtetennista@gmail.com")!

## Functional algorithms on inductive graphs
