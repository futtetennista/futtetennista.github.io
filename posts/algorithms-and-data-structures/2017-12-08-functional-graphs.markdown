---
title: Functional programming with graphs
tags: haskell, graphs
categories: data+strutures, algorithms
---

Graphs are a fundamental data structure in computer science because *a lot* of
problems can be modelled with them. Plenty of literature available on graphs and
graph algorithms (graph traversal, shortest path between two vertices,
minimum spanning trees etc.). Plenty of literature when we consider imperative
programming languages that is, but when we consider functional programming languages
the scenario changes dramatically for the worst. So let's start a journey to try to
answer the following question:
"How should I implement a graph algorithm in a functional programming language?".

<!--more-->

The following table gives a pretty good idea of how pervasive graphs are and why
anyone should care to answer the question in the first place.

![Graph applications (image taken from a slide of the Algorithms part 2 MOOC on
[Coursera](https://www.coursera.org/learn/algorithms-part2/)
by Bob Sedgwick and Kevin Wayne)](/images/graph_applications.png)

Problems involving graphs are also not unusual during job interviews and this is
actually where my curiosity about functional graph algorithms really took off:
I was eager to learn how to approach those kind of problems functionally.
When I started searching I honestly didn't expect to have such a
hard time finding material, and I do not even mean *good* material but any material
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
in practice.
So let's try to answer a slightly different question first: ""How can I
implement a graph algorithm in a functional programming language?"

## Imperative-style algorithms with monads

One option could be "translating" graph algorithms from the imperative
world to the functional world but that turns out to be unsurprisingly
unpleasant: one of the main reasons is that imperative graph algorithms
rely heavily on state and side effects (sometimes for efficiency reasons).
To prove this, let's translate
[depth-first search (DFS)](https://en.wikipedia.org/wiki/Depth-first_search)
as in
[The Algorithm Design Manual](https://www.goodreads.com/book/show/425208.The_Algorithm_Design_Manual)
by Steven S. Skiena using Haskell:

``` haskell
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

data Graph weight a = Graph [(a, [EdgeNode weight a])] Directed Int deriving Eq

type EdgeNode weight a = (a, weight)

type VertexState s = MV.MVector s VState

data DFSState s a weight =
  DFSState { dfsVertex :: a
           , dfsConnectedComponent :: ConnectedComponent a weight
           , dfsVertexState :: VertexState s
           }

data VState = Undiscovered | Discovered | Processed deriving (Show, Eq, Ord)

type ConnectedComponent weight a = Tree weight a

data Tree weight a = Nil | Node !a [(weight, Tree weight a)] deriving (Show, Eq)

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

    processEdgeNode :: Int -> Tree Int Int -> VState -> EdgeNode Int Int -> ST s (Tree Int Int)
    processEdgeNode v tree Undiscovered edgeNode@(v', _) =
      evalStateT dfs' (DFSState v' (buildTree v edgeNode tree) vstates)
    processEdgeNode _ tree _ _ = return tree

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

If you stopped reading I would not blame you. It is definitely **not** the best
piece of code ever written using a functional programming languange.
It probably is in some aspects better than an imperative-style implementation
- for example state and side effects are now explicit and pattern matching makes
the code a bit clearer in some places - but one
might argue that monadic code makes the algorithm even harder to follow.

This first attempt left me unsatisfied so I started doing some online research
on the subject and at some point I found
[this page](](https://wiki.haskell.org/Research_papers/Data_structures#Graphs)
in the Haskell wiki with a few links to research papers that approach graphs and
graph algorithms using a functional programming language. The rest of the blog
post will focus on describing the solution proposed in two of those papers:
["Structuring Depth First Search Algorithms in Haskell"](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.52.6526)
by David King and John Launchbury and
["Inductive Graphs and Functional Graph Algorithms"](http://web.engr.oregonstate.edu/~erwig/papers/abstracts.html#JFP01)
by Martin Erwig.

## Functional depth-first search using adjacency lists

In ["Structuring Depth First Search Algorithms in Haskell"](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.52.6526)
David King's and John Launchbury's main goals are:

1. implementing depth-first search and related algorithms using a functional
style without any performance penalty - this means traversing the graph in linear
time
2. achieving greater code modularity
3. being able to formally prove the critical properties of the considered algorithms

I would like to
highlight this last aspect: it's probably the first time I encounter some material
on graph algorithms that takes it into consideration and it can be really useful
in property testing for example. The paper approaches graph traversal as a combinatorial
problem and employs a common technique in that kind of problems: generate and prune.
Before illustrating the gist of that technique, let's define some types
and auxiliary functions:

``` haskell
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

mkEmpty :: (Ix i, MA.MArray (MA.STUArray s) Bool m)
        => (i, i) -> m (MA.STUArray s i Bool)
mkEmpty bnds = MA.newArray bnds False

contains :: (Ix i, MA.MArray (MA.STUArray s) Bool m)
         => MA.STUArray s i Bool -> i -> m Bool
contains = MA.readArray

include :: (Ix i, MA.MArray a Bool m) => a i Bool -> i -> m ()
include arr v = MA.writeArray arr v True
```

Let's consider this very simple graph:

<img class="figure centered" src="/images/sample_graph.png" alt="Sample graph" />

and now let's look at the generate and prune technique at a high level:
the "generate" step describes how to create all possible trees
from a given vertex. The following picture illustrates it for the sample graph
above, notice that the greyed-out nodes are not yet generated but will be on-demand
if necessary:

<img class="figure centered" src="/images/generate_prune_1.png" alt="Inductive graph" />

The "prune" step discards the sub-trees that violate to the invariants of DFS,
namely those that have already been discovered. Back to our example, after the
trees labelled `a` and `b` are traversed, the tree with root `a` is discarded
because a tree with the same label has already been discovered and the algorithm
traverses the tree labelled `c`:

<img class="figure centered" src="/images/generate_prune_3.png" alt="Inductive graph" />

The same thing happens after `c` is traversed leaving the final DFS spanning tree:

<img class="figure centered" src="/images/generate_prune_4.png" alt="Inductive graph" />

The approach guarantees the efficiency of the algorithm because the evaluation
strategy of languages with non-strict semantics (call-by-need or lazy evaluation)
assures that an expression is evaluated once, on-demand and since the discarded
trees will never be used - that is traversed - they will never be created in the
first place. Let's have a look now at the code:

``` haskell
dfs :: Graph -> [Vertex] -> Forest Vertex
dfs g = prune (bounds g) . map (generate g)
  where
    -- create all possible trees for each vertex...
    generate :: Graph -> Vertex -> Tree Vertex
    generate g v = Node v (map (generate g . fst) (g ! v))

    -- ...and discard the ones that are unused
    prune :: Bounds -> Forest Vertex -> Forest Vertex
    prune bnds ts = runST $ do
      s <- mkEmpty bnds :: forall s. ST s (MA.STUArray s Vertex Bool)
      chop ts s
```

Notice that the type signature for the `mkEmpty bnds` is mandatory, more info
can be found [here](https://stackoverflow.com/a/9469942/).
The `chop` function does the pruning of those trees that have already been discovered:

``` haskell
chop :: (MA.MArray (MA.STUArray s) Bool m)
     => Forest Vertex -> MA.STUArray s Vertex Bool -> m (Forest Vertex)
chop [] _arr = return []
chop (Node v ts:ns) arr = do
  visited <- contains arr v
  if visited
    -- prune ts
    then chop ns arr
    else do
      -- label vertex
      include arr v
      -- traverse left-to-right
      ts' <- chop ts arr
      -- traverse top-to-bottom
      ns' <- chop ns arr
      return $ Node v ts' : ns'
```

I would like to highlight two qualities of this solution:

- for performance reasons it uses a mutable array to keep track
of the state of each vertex. The paper points out that this is not strictly necessary
and if a logarithmic increase in the time complexity of the algorithm is acceptable,
a `Set` data structure can be used to avoid the need for monadic code.
- the algorithm does use a functional style but the data structure chosen to
represent a graph is an [adjacency list](https://en.wikipedia.org/wiki/Adjacency_list),
which is usually the preferred way of representing graphs in the imperative
programming languages. Why this is important will become apparent in the next paragraph.

##### A little remark

Section "5. Implementing depth-first search" states that

> The choice of pruning patterns determines whether the forest ends up being
> depth-first (traverse in a left-most, top-most fashion) or breadth-first (
> top-most, left-most)

but without providing any code for it and I honestly could not wrap my head around
on how to write a breadth-first traversal with the algorithm proposed in the paper.
If anybody has some pointers again please [let me know](/about.html)!

## Functional graph algorithms using inductive graphs

Martin Erwig's paper
["Inductive Graphs and Functional Graph Algorithms"](http://web.engr.oregonstate.edu/~erwig/papers/abstracts.html#JFP01)
asks the following question at the very beginning:

> How should I implement a graph algorithm in a functional programming language?

which was exactly the one that started my journey. The main goals of the paper
are:

- describing an inductive definition of graphs and graph algorithms as recursive
functions
- providing efficient implementations of graph algorithms that can be used in
real-world scenarios
- providing clear algorithms that can be used to teach graph algorithms

It acknowledges lots of the functional graph algorithms already developed but also
considers them all not completely satisfactory either because they introduce
constructs that are not currently available in today's programming languages
or because they entail some imperative-style strategy - i.e. keeping track of
visited nodes by labelling them - that contaminates the clarity of the algorithm,
makes it harder to reason about it and to proof its correctness.
The solution the paper proposes is to think about graphs in a new way.

### Enter inductive graphs

The paper makes a very interesting observation: lists and trees
algorithms are much simpler and more modular than graph algorithms
and do not require additional bookkeeping: why is that? The answer is two-fold:
their definition and the definitions of functions on them  are inductive and
besides that pattern matching helps a great deal when it comes to clarity and
succinctness. Now let's have a look at the definition of graphs: they are usually
defined as a pair `G = (V, E)` where `V` is the set of vertices and `E` the set
of edges, where edge is defined as a pair of vertices in `V`.
Imperative algorithms on graphs discover edges and vertices incrementally
and usually need to keep track of the visited vertices either using a separate
data structure or by storing more data in the graph itself.
In this sense the usual definition of graphs is monolithical and this is the
reasons why algorithms that use this API are doomed if what they strive for is
clarity and modularity. Would it be possible to define graphs inductively? If so
how? A valid definition for a graph data structure defined inductively might look
like the following:

``` haskell
infixr 5 :&:
data Graph weight label
  = Empty
  | (Context weight label) :&: (Graph weight label)
  deriving Show

type Context weight label =
  ( Adj weight  -- inbound edges
  , Vertex
  , label
  , Adj weight  -- outbound edges
  )

-- adjacent weighted edges
type Adj weight = [(weight, Vertex)]

type Vertex = Int
```

The definition should look familiar if you've already seen one for trees or lists:
a graph is either empty or it has a context and another graph.
A `Context` contains information about a given vertex, namely its value,
label (if any) and its adjacent edges classified as inbound or outbound.
So far so good: now taking the following graph as an example:

<img class="figure centered" src="/images/sample_graph.png" alt="Sample graph" />

how can we build an inductive graph from a list of vertices and edges?
One possible way of building the inductive graph would be the following:

<img class="figure centered" src="/images/sample_inductive_graph123.png" alt="Inductive graph" />

```haskell
ƛ: read "mkG [('a', 1), ('b', 2), ('c', 3)] [(1, 2, 5), (2, 1, 3), (2, 3, 1), (3, 1, 4)]"
([(4,3),(3,2)],1,'a',[(5,2)]) :&: (([],2,'b',[(1,3)]) :&: (([],3,'c',[]) :&: Empty))

```

But that's not the only valid representation of an inductive graph, another valid
inductive graph is the following:

<img class="figure centered" src="/images/sample_inductive_graph321.png" alt="Inductive graph" />

``` haskell
ƛ: read "mkG [('c', 3), ('b', 2), ('a', 1)] [(1, 2, 5), (2, 1, 3), (2, 3, 1), (3, 1, 4)]"
([(1,2)],3,'c',[(4,1)]) :&: (([(5,1)],2,'b',[(3,1)]) :&: (([],1,'a',[]) :&: Empty))
```

That brings to defining some of the properties of inductive graphs:

- given a list of vertices and a list of edges, multiple inductive
graphs can be built depending on the order of insertion of its vertices
- equality is not defined by their "shapes" but rather by the set of vertices
and edges they represent
- the adjacent inbound and outbound edges in a `Context` are lists of vertices
that have *already been discovered*
- inductive graphs are fully persistent data structures

### Active graph patterns

Pattern matching was identified as one of the ingredients that made lists and
trees algorithms clean and succint, the paper refers to an extension of pattern
matching for graphs named *"active graph pattern"* whose main goal is as far as
I understood to make the notation more compact, augmenting the classic pattern
matching by allowing a function to be called before the matching is applied. It
is very similar to
[view patterns](https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns)
but it's more powerful because the patter can access the outer scope.
This is not currently available in Haskell as far as I know, the following code
is made up to hopefully provide an intuition to the reader and **will not** type-check:

``` haskell
deg :: Vertex -> Graph weight label -> Int
deg v ((ins, _, _, outs) (:&: <!> v) g) = length ins + length out
```

The expression `(:&: <!> v)` can be interpreted as: *"find the `Context` for the
vertex `v` in the graph `g` if it exists and try to match the given pattern"*.
Active graph patterns are not essential when implementing inductive graphs and
it is possible do pattern matching without them, all that is needed is a function
`match`. An extremely naive implementation might look like:

``` haskell
match :: Vertex -> Graph weight label -> Maybe (Context weight label, Graph weight label)
match qv = matchHelp ([], [])
  where
    matchHelp _ Empty = Nothing
    matchHelp (lvs, wes) ((ins, v, l, outs) :&: g)
      | qv == v =
          -- rebuild the graph inserting `v` last
          let (:&:) !ctx !g' = mkG g ((l, v):lvs) es'
          -- return `v`'s context and the new inductive graph
          in Just (ctx, g')
      | otherwise = matchHelp ((l, v):lvs, es') g
      where
        -- build a list of edges to rebuild the graph
        es' =
          map (\(w, fromv) -> (fromv, v, w)) ins
            ++ map (\(w, tov) -> (v, tov, w)) outs
            ++ wes
```

### Functional graph algorithms

Having our inductive definitions of graphs, it's time to show how that can be
leveraged to write clear, recursive graph algorithms.
Let's have a look at some fundamental graph algorithms: depth-first search (DFS),
breadth-first search (BFS), Dijkstra's shortest path and Prims' algorithm to
find the minimum spanning tree (MST).

#### Depth-first search

Using a depth-first search strategy to visit a graph essentially means: traverse
each vertex **once** and visit **successors before siblings**.
Here's what the algorithm looks like:

``` haskell
dfs :: [Vertex] -> Graph weight label -> [Vertex]
dfs _ Empty = []
dfs [] _ = []
dfs (v:vs) g = case v `match` g of
  Nothing -> dfs vs g
  Just ((_,vtx,_,outs), g') -> vtx : dfs (destvs outs ++ vs) g'

destvs :: Context label weight -> [Vertex]
```

`dfs` is a recursive function that takes a list of input vertices and a graph
and returns a list of vertices sorted by traversing the graph in DFS-style.
If the graph or their input vertices are empty it returns the empty list,
otherwise it `match`es the current vertex `v` against the graph.
If `v` is a vertex in the graph, `match` will first return its context and a new
graph without it, append `v` to the results list and finally the recursion will
happen using as input the list of destination vertices for all outbound edges of
`v` appended to the remaining source vertices and the new graph returned by the
`match` function; if `v` is not a vertex in the graph then it is simply ignored
and the algorithm recursively calls itself.
There key observations about the algorithm are:

1. destination vertices are appended *in front of* the current vertex: this is
what makes the algorithm traversing the input graph depth-first. This is exactly
what the second invariant of DFS dictates: visit successors before sibilings.
2. the `match` function returns a new graph *without* the query vertex: this is
what the first invariant of DFS dictates: visit each vertex exectly once.
Since the new graph doesn't contain the query vertex there is no need for keeping
track of the visited vertices therefore no bookkeeping is necessary.

Let's have a look at a very simple example using one of the sample graphs above:

``` haskell
ƛ: let g = read "mkG [('c', 3), ('b', 2), ('a', 1)] [(1, 2, 5), (2, 1, 3), (2, 3, 1), (3, 1, 4)]"
([(1,2)],3,'c',[(4,1)]) :&: (([(5,1)],2,'b',[(3,1)]) :&: (([],1,'a',[]) :&: Empty))
ƛ: dfs (vertices g) g
[1, 2, 3]
```

One of the applications of DFS is finding the spanning forest (set of trees) of
a graph. The algorithm needs to build the spanning forest by traversing the graph
in such a way that only when DFS traversal is completed for a
[connected component](https://en.wikipedia.org/wiki/Connected_component_(graph_theory))
it will proceed with the next one. Let's define some types first:

``` haskell
import Control.Arrow (second)

data Tree a = Nil | Node !a (Forest a) deriving Show

type Forest a = [Tree a]

dff :: [Vertex] -> Graph weight label -> Forest Vertex
dff vs = fst . dff' vs
```

The `dff` function calls an auxiliary function `dff'` that does the heavy lifting,
let's have a look at it:

```haskell
dff' :: [Vertex] -> Graph weight label -> (Forest Vertex, Graph weight label)
dff' [] g = ([], g)
dff' (v:vs) g = case v `match` g of
  Nothing -> dff' vs g
  Just (ctx, g') -> (Node v ts : forest, g'')
  where
    -- `second` applies the function `dff' vs` to the second element of
    -- the pair returned by `dff' (destvs ctx) g'`
    (ts, (forest, g'')) = second (dff' vs) (dff' (destvs ctx) g')

destvs :: Context label weight -> [Vertex]
```

The `dff'` function is recursive: if `match`ing the vertex `v` with
the graph `g` succeeds, `dff'` calls itself passing its siblings and
the new graph as arguments until the list of vertices is empty; when the list is
empty the recursion continues using the remaining vertices - if any - and the
most recent version of the graph.
Again let's have a look at a very simple example built on top of the previous one:

``` haskell
ƛ: let g = read "mkG [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)] [(1, 2, 5), (2, 1, 3), (2, 3, 1), (3, 1, 4), (4, 5, 7)]" :: Graph Int Char
([(4,3),(3,2)],1,'a',[(5,2)]) :&: (([],2,'b',[(1,3)]) :&: (([],3,'c',[]) :&: (([],4,'d',[(7,5)]) :&: (([],5,'e',[]) :&: Empty))))
ƛ: dff (vertices g) g
[Node 1 [Node 2 [Node 3 []]], Node 4 [Node 5 []]]
```

#### Breadth-first search

Using a breadth-first search strategy to visit a graph essentially means: traverse
each vertex **once** and visit **siblings before successors**.
Here's what the algorithm looks like:

``` haskell
bfs :: Graph weight label -> [Vertex] -> [Vertex]
bfs gr vs = bfs' gr vs
  where
    bfs' g svs
      | isEmpty g || null svs = []
      | otherwise = case v `match` g of
          Nothing -> bfs' g vs
          Just ((_,vtx,_,outs), g') -> vtx : dfs (vs ++ destvs outs) g'

destvs :: Context label weight -> [Vertex]
```

There key facts to notice about the algorithm are:

1. siblings are appended *at the end of* the source vertices: this is what makes
the algorithm traversing the input graph breadth-first. This is exactly what the
second invariant of BFS dictates : visit siblings before the successor.
2. the `match` function returns a new graph *without* the current vertex: this is
what the first invariant of BFS dictates: traverse each vertex exectly once.
Since the new graph doesn't contain the current vertex there is no need for keeping
track of the visited vertices.
3. the algorithm is mostly the same as `dfs`, the only thing that changes is
where siblings are appended: in case of BFS they're appended at the end of the
list, in case of DFS in front of it. To fully appreciate
this it might be useful to think of these algorithms in terms of the defining
strategies of the data structures they internally use: LIFO in case of DFS and a
FIFO in case of BFS.

One of the applications of BFS is finding the shortest path in a unweighted graph.
This time the paper chooses a different representation for the spanning forest,
the reason is that it's harder to build a tree of nodes because of the way
BFS works: **its recursion logic delivers nodes in a top-down fashion whereas the one
in DFS delivers them in a bottom-up fashion, which is the natural way of building
a tree** [EXPLAIN THIS BETTER].
The paper argues that considering some of the applications of BFS spanning trees
- for example finding the  shortest path between two vertices in a unweighted
graph - a list of paths can be used to represent the BFS spanning tree.
Let's have a look at the implementation of the shortest path algorithm:

``` haskell
import Control.Arrow (first)

type Path = [Vertex]

-- Roots tree
type RTree = [Path]

esp :: Vertex -> Vertex -> Graph weight label -> Path
esp src dst = reverse . pathTo ((==dst) . head) . bft src

pathTo :: (a -> Bool) -> [a] -> a
pathTo p = head . filter p
```

The `esp` function requires a source vertex and a destination vertex, filters the
path to the destination and reverses it (why this is necessary will become clear
in a moment). Notice that since Haskell has non-strict sematics,
`esp` stops as soon as the path to the target destination vertex is found. Now
let's have a look at the implementation of the `bft` function:

``` haskell
bft :: Vertex -> Graph weight label -> RTree
bft v =  bf [[v]]

bf :: [Path] -> Graph weight label -> RTree
bf paths = bf' paths
  where
    bf' :: [Path] -> Graph weight label -> RTree
    bf' ps g
      | null ps || isEmpty g = []
      | otherwise = case v `match` g of
          Nothing -> bf' ps' g
          Just (ctx, g') -> p : bf' (ps' ++ map (:p) (destvs out)) g'

    -- gets the current vertex from the first path in the list and the remaining paths
    -- paths will never be empty because `bf` is called using a non-empty list
    (p@(v:_), ps') = first head (splitAt 1 ps)

destvs :: Context label weight -> [Vertex]
```

Instead of explaining what the function does step-by-step, let's have a look
at an example on a simple graph as it might be easier to understand:

``` haskell
ƛ: let g = read "mkG [('a', 1), ('b', 2), ('c', 3)] [(1, 2, ()), (2, 1, ()), (2, 3, ()), (3, 1, ())]" :: Graph () Char
([(4,()),(3,())],1,'a',[(5,())]) :&: (([],2,'b',[(1,())]) :&: (([],3,'c',[]) :&: Empty))
ƛ: bf [[1]] g
[[1],[2,1],[3,2,1]]
```

Notice that an unweighted graph is a graph whose weight is `()` and that the
resulting spanning tree contains the shortest path from the source vertex to all
other vertices in *reverse order*. The `bf` function builds complete paths from
a source to a destination vertex but doesn't waste any memory because list prefixes
are shared.

#### Dijkstra's shortest path

Finding the shortest path between two vertices means finding the cheapest path
between them (this means that there must exist a partial ordering for edge weights).
Dijkstra's algorithm to find the shortest path in a weighted graph essentially
chooses always the cheapest edge taking into account the distance traversed so
far. First let's define two new auxiliary types:

``` haskell
-- Labelled vertex
type LVertex label = (label, Vertex)

newtype LPath label = LPath { getLPath :: [LVertex label] }

-- Labelled R-Tree (or Root Tree)
type LRTree label = [LPath label]

instance Eq label => Eq (LPath label) where ...

instance Ord label => Ord (LPath label) where ...
```

The algorithm uses a min-heap and some auxiliary functions to keep track of the
cheapest path:

``` haskell
import qualified Data.Heap as Heap

getPath :: Vertex -> LRTree label -> Path
getPath v = reverse . map snd . getLPath . pathTo ((==v) . lv2v)
  where
    lv2v = snd . head . getLPath

expand :: Monoid weight
       => weight -> LPath weight -> Context weight label -> [LPath weight]
expand d (LPath p) (_, _, _, outs) = map (\(w, v) -> LPath ((w `mappend` d, v):p)) outs

mergeAll :: (Monoid weight, Ord weight)
         => [LVertex weight]
         -> Heap.Heap (LPath weight)
         -> Context weight label
         -> Heap.Heap (LPath weight)
mergeAll p@((dist, _):_) h = foldr Heap.insert h . expand dist (LPath p)
```

The `expand` function builds new `LPath`s whose label is the sum - let's assume
weights are positive integers for simplicity - of the distance walked so far and
the weight of the outbound edge. The `mergeAll` function takes these paths and
inserts them in the heap. The `getPath` function just extracts the path to the
given destination vertex from the list of paths.
Now let's have a look at the core of the algorithm:

``` haskell
dijkstra :: (Monoid weight, Ord weight)
         => Heap.Heap (LPath weight) -> Graph weight label -> LRTree weight
dijkstra h g
  | isEmpty g = []
  | otherwise = case Heap.viewMin h of
      Nothing -> []
      Just (lpath, h') -> dijkstra' (lpath, h')
  where
    dijkstra' (LPath p@((_, v):_), h') =
      case v `match` g of
        Nothing -> dijkstra h' g
        Just (ctx, g') -> LPath p : dijkstra (mergeAll p h' ctx) g'

-- shortest path tree
spt :: (Monoid weight, Ord weight)
    => Vertex -> Graph weight label -> LRTree weight
spt src = dijkstra (Heap.singleton $ LPath [(mempty, src)])

sp :: (Monoid weight, Ord weight)
   => Vertex -> Vertex -> Graph weight label -> Path
sp src dst = getPath dst . spt src
```

The `sp` function kicks off the algorithm by providing the source node to
the `spt` function which in turn calls `dijkstra` with a singleton min-heap that
contains a path to the source vertex with weight 0 - this is how expensive it is
to walk from the source vertex to the source vertex. The `dijkstra` function is
a recursive function that peeks the cheapest path from the min-heap, and if the
current vertex `v` is contained in the graph - that is, the vertex hasn't been
already visited - appends it to the resulting `LRTree` and calls itself recursively
with a new min-heap that contains up-to-date paths and a new graph that doesn't
contain `v`. The recursion stops if the graph is empty - that is all vertices has
been visited - or the min-heap is empty - that is all edges have been traversed.
This is definitely a bit more complex than the other algorithms but it's quite
elegant and modular. Let's have a look at an example on the following graph:

<img class="figure centered" src="/images/sample_graph2.png" alt="Sample graph 2" />

``` haskell
ƛ: let g = read "mkG [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5), ('f', 6), ('g', 7)] [(1,2,12),(1,3,7),(1,4,5),(2,3,4),(2,7,7),(3,4,9),(3,5,4),(3,7,3),(4,5,7),(5,6,5),(5,7,2),(6,7,2)]" :: Graph Int Char
-- Make weights a monoid for addition on Ints
ƛ: let g' = gmap (\(ins, l, v, outs) -> (map (first Sum) ins, l, v, map (first Sum) outs)) (undir g)
```

The `undir` function simply transforms a directed graph to an undirected one.

```haskell
ƛ: spt 1 g'
-- forget about the `Sum` constructors around weights for a moment and the list
-- will be something like
[LPath {getLPath = [(0,1)]},LPath {getLPath = [(5,2),(0,1)]},LPath {getLPath = [(6,3),(5,2),(0,1)]}]
ƛ: sp 1 3 mg
[1,2,3]
```

### Minimum spanning tree

Prim's algorithm to find the minimum spanning tree (MST) essentially chooses
always the cheapest edge among the known edges (it's a greedy algorithm).
Prim's and Dijkstra's algorithms are notoriously very similar: this similarity
becomes evident using recursive functions. We'll re-use the same types defined
for the shortest path algorithm but define different auxiliary functions:

``` haskell
mergeAll :: (Monoid weight, Ord weight)
         => [LVertex weight]
         -> Heap.Heap (LPath weight)
         -> Context weight label
         -> Heap.Heap (LPath weight)
mergeAll p h = foldr Heap.insert h . addEdges (LPath p)

addEdges :: Monoid weight => LPath weight -> Context weight label -> [LPath weight]
addEdges (LPath p) (_, _, _, outs) = map (LPath . (:p)) outs
```

The `addEdges` function is very similar to the `expand` function but it doesn't
take into account the distance walked so far, only the weight of the edges.
The core of the algorithm shouldn't be anything new, it's basically the same as
Dijkstra's:

``` haskell
mst :: (Monoid w, Ord w) => Vertex -> Graph w l -> LRTree w
mst src = prim $ Heap.singleton (LPath [(mempty, src)])

prim :: (Monoid w, Ord w) => Heap.Heap (LPath w) -> Graph w l -> LRTree w
prim h g
  | isEmpty g = []
  | otherwise = case Heap.viewMin h of
      Nothing -> []
      Just (lpath, h') -> prim' lpath h'
  where
    prim' (LPath p@((_, v):_), h') =
      case v `match` g of
        Nothing -> prim h' g
        Just (ctx, g') -> LPath p : prim (mergeAll p h' ctx) g')
```

Now that the MST can be build, let's find the path between two vertices:

```haskell
mstPath :: Vertex -> Vertex -> LRTree w -> Path
mstPath src dst t = joinPaths (getPath src t) (getPath dst t)

joinPaths :: Path -> Path -> Path
joinPaths p2src p2dst = joinAt (head p2src) (tail p2src) (tail p2dst)

joinAt :: Vertex -> Path -> Path -> Path
joinAt _src (v:vs) (v':vs')
  | v == v' = joinAt v vs vs'
joinAt src ps ps' = reverse ps ++ (src:ps')
```

Let's try these algorithms out using this graph:

<img class="figure centered" src="/images/sample_graph2.png" alt="Sample graph 2" />

``` haskell
ƛ: let g = read "mkG [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5), ('f', 6), ('g', 7)] [(1,2,12),(1,3,7),(1,4,5),(2,3,4),(2,7,7),(3,4,9),(3,5,4),(3,7,3),(4,5,7),(5,6,5),(5,7,2),(6,7,2)]" :: Graph Int Char
-- Make weights a monoid for addition on Ints
ƛ: let g' = gmap (\(ins, l, v, outs) -> (map (first Sum) ins, l, v, map (first Sum) outs)) (undir g)
```

One of the existing minimum spanning trees for that graph is the following:

<img class="figure centered" src="/images/prim.png" alt="Prim's MST" />

``` haskell
ƛ: let mstree = mst 1 g'
[LPath {getLPath = [(0,1)]},LPath {getLPath = [(5,4),(0,1)]},LPath {getLPath = [(7,5),(5,4),(0,1)]},LPath {getLPath = [(2,7),(7,5),(5,4),(0,1)]},LPath {getLPath = [(2,6),(2,7),(7,5),(5,4),(0,1)]},LPath {getLPath = [(3,3),(2,7),(7,5),(5,4),(0,1)]},LPath {getLPath = [(4,2),(3,3),(2,7),(7,5),(5,4),(0,1)]}]
ƛ: mstPath 3 5 mstree
[3,7,5]
```

## A word on efficiency

So far when talking about inductive graphs and related algorithms the word efficiency
wasn't really mentioned. This is because the implementations shown so far are
hopelessly inefficient but hopefully they provide an intuition about inductive
graphs. An efficient, real-world implementation relies on more efficient data
structures than lists and more importantly a key aspect to make the algorithms
having asymptotically optimal running times is that active patterns must
match in linear times. A real-world implementation based on Martin Erwig's paper
is available on
[Stackage](https://www.stackage.org/lts-9.14/package/fgl-5.5.3.1)
and if you're curious to know how it is possible to implement inductive graphs
efficiently I'll encourage to look at the source code.
This could be a very topic for a separate blog post.


## Wrapping up

One of the tradeoffs to achieve clear and elegant graph algorithms seems to be
shifting the complexity from the algorithm itself to the supporting data
structures: implementing an inductive graph and active patterns is more complex
than implementing an adjacency lists, using a min-heap in the shortest path
algorithm eliminates the need for bookkeeping when looking for the next cheapest
path.

The journey into graphs and related algorithm in functional programming
started with a simple question that was surprisingly hard to answer:
*"How should I implement a graph algorithm in a functional programming language?"*.
The plethora of resource about graphs in for imperative languages is not matched
in the functional world, where adequate solution to the problem have surfaced
only in the last 20 years or so and are restricted to the academic world.
We started with an unsatisfactory solution based on monads,
then illustated one that leverages ?? of functional programming languages but
sill grounded in imperative programming and finally described a solution
based on inductive graphs that manages to achieve an elegant, clear and modular
solution by choosing a different representation for graphs. Also, graph
algorithms based on inductive graphs guarantee asymptotically optimal running
times for most graph algorithms, provided that the implementation of some key
operations is done efficiently.
