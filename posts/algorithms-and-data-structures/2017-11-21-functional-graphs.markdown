---
title: Functional programming with graphs
tags: haskell, graphs
categories: data+strutures, algorithms
---

Graphs are a fundamental data structure in Computer Science because *a lot* of
problems can be modelled with them. Plenty of literature available on graphs and
graph algorithms i.e. graph traversal, shortest path between two vertices,
minimum spanning trees etc. Plenty of literature when we consider imperative
languages that is, but what about the functional world?
<!--more-->

In order to have an idea of how pervasive graphs are, have a look at the
following table

![Image taken from a slide of the Algorithms part 2 MOOC on
[Coursera](https://www.coursera.org/learn/algorithms-part2/)
by Bob Sedgwick and Kevin Wayne](/images/graph_applications.png)

It's also not atypical to being asked to solve a problem involving graphs when
doing job interviews and this is where my curiosity about functional graph algorithms
really started: I was eager to learn how to approach those kind of problems
functionally. When I started searching I honestly didn't expect to have such a
hard time finding material, and I do not even mean good material but any material
at all! Maybe I didn't look for it hard enough - if that's the case
please [let me know](/about.html)! - but basically the only book on the subject
of functional data structures out there is
[Purely Functional Data Structures](https://www.goodreads.com/book/show/594288.Purely_Functional_Data_Structures)
by Chris Okasaki, released in 2008 (and it's pretty advanced material)
and I couldn't find any book that focused on functional algorithms. Graphs and
graph algorithms are no exception: there is a massive amount of literature
available for imperative languages but it takes some [DuckDuckGo](http://duckduckgo.com/)-fu
to find literature on the topic for purely functional languages, and more often
than not that literature comes in the form of academic papers. After a decent amount
of digging my understanding is that lots of purely functional algorithms do exist
but they are not as efficient as the imperative counterparts; this might be
one of the reasons why they are basically shovelled under the carpet and not used
in practice. So how can we deal with graphs using a purely functional language?
One possibility could be "translating" graph algorithms from the imperative
world to the functional world but that turns out to be (unsurprisingly) unsatisfactory,
one of the main reasons being that imperative graph algorithms rely heavily on
state and side effects (sometimes for efficiency reasons) making the task hard
and the outcome far from being optimal.

## The imperative approach with monads
To show what "translating" an imperative algorithm in a functional context, let's
try to implement one the most fundamental graph algorithms in Haskell:
[depth-first search (DFS)](https://en.wikipedia.org/wiki/Depth-first_search).
The following code is an almost literal translation of the DFS algorithm as in
[The Algorithm Design Manual](https://www.goodreads.com/book/show/425208.The_Algorithm_Design_Manual)
by Steven S. Skiena:

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

Not *that* elegant or modular isn't it? Also it is definitely **not** the most
efficient implementation but I doubt that making it more efficient will fix the
other concerns just mentioned. It probably is in some respect better than an
imperative-style implementation - for example it makes the presence of state and
side effects explicit and union types are handy - but one might argue that monadic
code makes the algorithm even harder to follow.

## Moving towards a functional solution
My [DuckDuckGo](http://duckduckgo.com/)-ing around pointed me at some point to
the [Haskell wiki](https://wiki.haskell.org/Research_papers/Data_structures#Graphs)
where a few links to research papers that approach graphs and graph
algorithms using a functional style can be found. I found particularly interesting
two of those papers, namely
["Structuring Depth First Search Algorithms in Haskell"](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.52.6526)
by David King and John Launchbury and
["Inductive Graphs and Functional Graph Algorithms"](http://web.engr.oregonstate.edu/~erwig/papers/abstracts.html#JFP01)
by Martin Erwig and I'd like to give some highlights of their content.

## Functional depth-first search using adjacency lists
In ["Structuring Depth First Search Algorithms in Haskell"](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.52.6526)
David King's and John Launchbury's main goals are: implementing depth-first search
and related algorithms using a functional style without any performance penalty
- that is graph traversal in linear time, achieving greater code modularity and
formally prooving the critical properties of the algorithms. I would like to
highlight this last aspect: it's probably the first time I encounter some material
on graph algorithms that takes it into consideration and it can be really useful
in property testing for example. The paper approaches graph traversal as a combinatorial
problem and employs a common technique in languages with non-strict semantics:
generate and prune. In a nutshell: the generate step describes how to create all
possible trees from a given vertex and the prune step discards the trees that do
not respect the invariants of DFS, namely (sub-)trees whose root is a vertex
that has already been discovered. This approach guarantees the efficiency of the
algorithm because the evaluation strategy of languages with non-strict semantics
(call-by-need or lazy evaluation) assures that an expression is evaluated on-demand
and since the discarded trees will never be used (traversed) they will never
be created in the first place.

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

-- Let's assume for simplicity that vertices and weights are integers
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
    prune bnds ts = runST $ do
      s <- mkEmpty bnds :: forall s. ST s (MA.STUArray s Vertex Bool)
      chop ts s

    chop :: (MA.MArray (MA.STUArray s) Bool m)
         => Forest Vertex
         -> MA.STUArray s Vertex Bool
         -> m (Forest Vertex)
    chop [] _arr = return []
    chop (Node v ts:ns) arr = do
      visited <- contains arr v
      if visited
        -- prune ts
        then chop ns arr
        else do
          include arr v
          -- traverse left-to-right
          ts' <- chop ts arr
          -- traverse top-to-bottom
          ns' <- chop ns arr
          return $ Node v ts' : ns'
```

For performance reasons the algorithm uses a mutable array to keep track
of the state of each vertex, but the paper points out that it could be replaced
by a `Set` in order to avoid the need for monadic code; the price to
pay is a logarithmic increase in its time complexity though.
Also, note that even if the algorithm uses a functional
style the data structure used to represent a graph is an
[adjacency list](https://en.wikipedia.org/wiki/Adjacency_list), which
is usually the preferred way of representing graphs in the imperative world.
The paper also shows how to implement other common graph algorithms using a
functional style:

``` haskell
-- add imports
import Data.List ((\\))
import Data.Sequence (Seq, (><), (|>))
import Data.Sequence
import Data.Foldable (toList)

-- TOPOLOGICAL SORTING
topsort :: Graph -> [Vertex]
topsort = reverse . postOrd

-- GRAPH TRAVERSAL
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

-- EDGE CLASSIFICATION
-- build graph of edge types
-- O(T)
mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f tree = array (bounds tree) [(v, f v (tree ! v)) | v <- indices tree]

-- O(TS^2)
tree :: Bounds -> Forest Vertex -> Graph
tree bnds ts = buildG bnds (concatMap flat ts)
  where
    -- #ts + #ts^2 = O(TS^2)
    flat (Node v ts) = [(v, (v', 0)) | Node v' _ <- ts] ++ concatMap flat ts

-- O(V * E)
back :: Graph -> Table Vertex -> Graph
back g postord = mapT select g
  where
    select v es = [(v', w) | (v', w) <- es, postord ! v < postord ! v']


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
    select v es = [(v', w) | (v', w) <- es, preord ! v < preord ! v'] \\ treeG ! v
```

The algorithms to classify edges might become clearer by looking at this diagrams
```
    Pre-order graph traversal:                Post-order graph traversal:

      --- Tree / Forward -->                    ----------- Back ------------>
    v                        w                v                                w
      <--- Back / Cross ----                    <-- Tree / Forward / Cross ---

```

Two closing notes: section 5 (*Implementing depth-first search*) states that

> The choice of pruning patterns determines whether the forest ends up being
> depth-first (traverse in a left-most, top-most fashion) or breadth-first (
> top-most, left-most)

but without providing any code for it and I honestly could not wrap my head around
on how to write a breadth-first traversal with the algorithm proposed in the paper.
If anybody has some pointers again please [let me know](/about.html)!

The code snippets above have been mostly copy and pasted from the paper, they only
needed some tweaks when dealing with th `ST` monad.

## Functional graph algorithms using inductive graphs
I found the introduction of the paper
["Inductive Graphs and Functional Graph Algorithms"](http://web.engr.oregonstate.edu/~erwig/papers/abstracts.html#JFP01)
by Martin Erwig just brilliant. It really clicked with me because it addressed
most of the questions and perplexities I had, starting from the very first line:

> How should I implement a graph algorithm in a functional programming language?

The paper acknowledges lots of the functional implementations out there but also
finds them all unsatisfactory: they either use concepts not currently available
in today's programming languages or they entail some imperative-style strategy -
i.e. keeping track of visited nodes by somehow labelling them -
that contaminates the clarity of the algorithm and makes it harder to reason about
it and to proof its correctness. The solution the paper proposes is thinking
about graphs in a new way.

### Enter inductive graphs
Lists and trees algorithms instead are much more simple
and modular and do not require additional bookkeeping, why? Their definition is
inductive and function definitions using those data structures are also inductive;
moreover, pattern matching helps a great deal when it comes to clarity and
succinctness. Now let's take graphs: a graph is usually defined as a pair
`G = (V, E)` where `V` is the set of vertices and `E` the set of edges,
where edge is defined as a pair of vertices in `V`.
Imperative-style algorithms on graphs discover edges and vertices incrementally
and usually need to keep track of the visited vertices either using a separate
data structure or  by defining the graph slightly differently to add new fields.
In this sense the usual definition of graphs is monolithical. These are identified
as the reasons why algorithms centered around this API are doomed if what they
want to achieve is clarity and modularity.
A valid definition for a graph defined inductively might have the following:

``` haskell
infixr 5 :&:
data Graph w l = Empty | (Context w l) :&: (Graph w l) deriving Show

type Context w l =
  ( Adj w  -- inbound edges
  , Vertex
  , l      -- label
  , Adj w  -- outbound edges
  )

-- adjacent weighted edges
type Adj w = [(w, Vertex)]

type Vertex = Int
```

A graph is either empty or it's a function that accepts a context and a graph.
The context describes a given vertex, namely its value, label (if any) and its
adjacent edges classified as inbound or outbound. So far so good, how can we build
an inductive graph? This is easier to understand with an example:

![Sample graph](/images/sample_graph.png)

```haskell
ƛ: read "mkG [('a', 1), ('b', 2), ('c', 3)] [(1, 2, 5), (2, 1, 3), (2, 3, 1), (3, 1, 4)]"
([(4,3),(3,2)],1,'a',[(5,2)]) :&: (([],2,'b',[(1,3)]) :&: (([],3,'c',[]) :&: Empty))

```

![An inductive graph based on the given sample graph](/images/sample_inductive_graph123.png)

Note that given a set of input vertices and edges, multiple inductive graphs
can be built depending on the order of insertion of its vertices.

``` haskell
ƛ: read "mkG [('c', 3), ('b', 2), ('a', 1)] [(1, 2, 5), (2, 1, 3), (2, 3, 1), (3, 1, 4)]"
([(1,2)],3,'c',[(4,1)]) :&: (([(5,1)],2,'b',[(3,1)]) :&: (([],1,'a',[]) :&: Empty))
```

![Another inductive graph based on the given sample graph](/images/sample_inductive_graph321.png)

Looking back at the definition of the `Graph` type, it looks quite
similar to the one of lists but it's not quite the same because there are
rules for the construction of a graph, namely that the context of a given vertex
contains the adjacent inbound and outbound edges only if the pair of vertices
has *already been discovered*.
