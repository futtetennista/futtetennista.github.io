---
title: Functional programming with graphs
tags: haskell, graphs
categories: data+strutures, algorithms
---

Graphs are a fundamental data structure in computer science because *a lot* of
problems can be modelled with them. Plenty of literature available on graphs and
graph algorithms i.e. graph traversal, shortest path between two vertices,
minimum spanning trees etc. Plenty of literature when we consider imperative
languages that is, but what about functional languages?
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
in practice. So how can we deal with graphs using a purely functional language?
One possibility could be "translating" graph algorithms from the imperative
world to the functional world but that turns out to be (unsurprisingly) unsatisfactory,
one of the main reasons being that imperative graph algorithms rely heavily on
state and side effects (sometimes for efficiency reasons) making the task hard
and the outcome far from being optimal.

## Imperative-style algorithms with monads
To show what "translating" an imperative algorithm in a functional context, let's
try to implement one the most fundamental graph algorithms in Haskell:
[depth-first search (DFS)](https://en.wikipedia.org/wiki/Depth-first_search).
The following code is an almost literal translation of the DFS algorithm as in
[The Algorithm Design Manual](https://www.goodreads.com/book/show/425208.The_Algorithm_Design_Manual)
by Steven S. Skiena:

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

data Tree w a = Nil | Node !a [(w, Tree w a)] deriving (Show, Eq)

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
other concerns just mentioned. It probably is in some aspects better than an
imperative-style implementation - for example state and side effects are now
explicit and pattern matching makes the code a bit clearer - but one
might argue that monadic code makes the algorithm even harder to follow.

## Towards a functional solution
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

-- generate all possible trees for each node (on-demand)
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

postorderForest :: Forest a -> Seq a
postorderForest = foldr ((><) . postorder) Seq.empty

postOrd :: Graph -> [Int]
postOrd = toList . postorderForest . dff

preorder :: [a] -> Tree a -> [a]
preorder vs (Node v ts) = preorderForest (v:vs) ts

preorderForest :: [a] -> Forest a -> [a]
preorderForest = reverse . foldr (flip preorder)

preOrd :: Graph -> [Vertex]
preOrd = preorderForest [] . dff

```

And how to build graphs by classifying the different types of edges in a graph
leveraging pre- and post-order graph traversals:

```haskell
mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f tree = array (bounds tree) [(v, f v (tree ! v)) | v <- indices tree]

tree :: Bounds -> Forest Vertex -> Graph
tree bnds ts = buildG bnds (concatMap flat ts)
  where
    flat (Node v ts) = [(v, (v', 0)) | Node v' _ <- ts] ++ concatMap flat ts

back :: Graph -> Table Vertex -> Graph
back g postord = mapT select g
  where
    select v es = [(v', w) | (v', w) <- es, postord ! v < postord ! v']

cross :: Graph -> Table Vertex -> Table Vertex -> Graph
cross g preord postord = mapT select g
  where
    select v es =
    [(v', w) | (v', w) <- es, postord ! v > postord ! v' && preord ! v > preord ! v']

forward :: Graph -> Graph -> Table Vertex -> Graph
forward g treeGraph preord = mapT select g
  where
    select v es = [(v', w) | (v', w) <- es, preord ! v < preord ! v'] \\ treeGraph ! v
```

#### Notes

Section "5. Implementing depth-first search" states that

> The choice of pruning patterns determines whether the forest ends up being
> depth-first (traverse in a left-most, top-most fashion) or breadth-first (
> top-most, left-most)

but without providing any code for it and I honestly could not wrap my head around
on how to write a breadth-first traversal with the algorithm proposed in the paper.
If anybody has some pointers again please [let me know](/about.html)!

The code snippets above have been mostly copy and pasted from the paper, they only
needed some tweaks when dealing with th `ST` monad.

## Functional graph algorithms using inductive graphs
The introduction of the paper
["Inductive Graphs and Functional Graph Algorithms"](http://web.engr.oregonstate.edu/~erwig/papers/abstracts.html#JFP01)
by Martin Erwig starts with this line

> How should I implement a graph algorithm in a functional programming language?

I must confess that this paper really clicked with me from the git-go because it
asked the same questions I had on the topic and provided answers for most of them.
It acknowledges lots of the functional algorithms already developed but also
considers them all not completely satisfactory either because they use concepts
not currently available
in today's programming languages or because they entail some imperative-style
strategy - i.e. keeping track of visited nodes by somehow labelling them -
that contaminates the clarity of the algorithm, makes it harder to reason about
it and to proof its correctness. The solution the paper proposes is to think
about graphs in a new way.

### Enter inductive graphs

The paper makes a very interesting observation at some point: lists and trees
algorithms are much simpler and more modular than graph algorithms
and do not require additional bookkeeping: why is that? The answer is two-fold:
for once, their definition is inductive and function definitions using those
data structures are also inductive and besides that pattern matching helps a
great deal when it comes to clarity and succinctness.
Now let's have a look at the definition of graphs: they are usually
defined as a pair `G = (V, E)` where `V` is the set of vertices and `E` the set
of edges, where edge is defined as a pair of vertices in `V`.
Imperative-style algorithms on graphs discover edges and vertices incrementally
and usually need to keep track of the visited vertices either using a separate
data structure or by storing more data in the data structure representing the graph.
In this sense the usual definition of graphs is monolithical and this is the
reasons why algorithms that use this API are doomed if what they strive for
clarity and modularity. Would it be possible to define graphs inductively? If so
how? A valid definition for a graph data structure defined inductively might look
like the following:

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

The definition should look familiar if you've already seen one for trees or lists:
a graph is either empty or it has a context and another graph.
The context contains information about a given vertex, namely its value,
label (if any) and its adjacent edges classified as inbound or outbound.
So far so good, how can we build
an inductive graph? This is easier to understand with an example:

<div class="figure centered">
  <img src="/images/sample_graph.png" alt="Sample graph">
  <p class="caption">An inductive graph based on the given sample graph</p>
</div>

```haskell
ƛ: read "mkG [('a', 1), ('b', 2), ('c', 3)] [(1, 2, 5), (2, 1, 3), (2, 3, 1), (3, 1, 4)]"
([(4,3),(3,2)],1,'a',[(5,2)]) :&: (([],2,'b',[(1,3)]) :&: (([],3,'c',[]) :&: Empty))

```

<div class="figure centered">
  <img src="/images/sample_inductive_graph123.png" alt="Inductive graph">
  <p class="caption">An inductive graph based on the given sample graph</p>
</div>

Note that given a set of input vertices and edges, multiple inductive graphs
can be built depending on the order of insertion of its vertices.

``` haskell
ƛ: read "mkG [('c', 3), ('b', 2), ('a', 1)] [(1, 2, 5), (2, 1, 3), (2, 3, 1), (3, 1, 4)]"
([(1,2)],3,'c',[(4,1)]) :&: (([(5,1)],2,'b',[(3,1)]) :&: (([],1,'a',[]) :&: Empty))
```

<div class="figure centered">
  <img src="/images/sample_inductive_graph321.png" alt="Inductive graph">
  <p class="caption">Another inductive graph based on the given sample graph</p>
</div>

Looking back at the definition of the `Graph` type, it might look quite
similar to the one of lists but it's not quite the same because there are
rules for the construction of a graph: the context of a given vertex
contains the adjacent inbound and outbound edges only if the pair of vertices
has *already been discovered*.

### Active graph patterns

Pattern matching was identified as one of the ingredients that made lists and
trees algorithms clean and succint, the paper refers to an extension of pattern
matching for graphs named *"active graph pattern"* whose main goal is as far as
I understood to make the notation more compact, augmenting the classic pattern
matching by allowing a function to be called before the matching is applied. It
is very similar to [view patterns](???) but it's more powerful because the patter
can access the outer scope.
This is not currently available in Haskell so the following code is something
I made up to hopefully provide an intuition to the reader and **will not** type-check:

``` haskell
deg :: Vertex -> Graph w l -> Int
deg v ((ins, _, _, outs) (:&: ! v) g) = length ins + length out
```

The expression `(:&: ! v)` can be interpreted as: *"find the `Context` for the
vertex `v` in `g` if it exists and try to match the given pattern"*.
Active graph patterns are not essential when implementing inductive graphs and
it is possible do pattern matching without them, all that is needed is a function
`match`. An extremely naive implementation might look like:

``` haskell
match :: Vertex -> Graph w l -> Maybe (Context w l, Graph w l)
match qv = matchHelp ([], [])
  where
    matchHelp _ Empty = Nothing
    matchHelp (vs, es) ((ins, v, l, outs) :&: g)
      | qv == v = let (:&:) !ctx !g' = mkG g ((l, v):vs) es'
                  in Just (ctx, g')
      | otherwise = matchHelp ((l, v):vs, es') g
      where
        es' =
          map (\(w, fromv) -> (fromv, v, w)) ins
            ++ map (\(w, tov) -> (v, tov, w)) outs
            ++ es
```

### Functional graph algorithms

Having our inductive definitions of graphs, it's time to show how that can be
leveraged to write graph algorithms that are elegant and composable.
We'll have a look at three fundamental graph algorithms, in the paper you can find more of
them: depth-first search (or DFS), breadth-first search (or BFS) and shortest path.

##### Terminology

Before digging into the description of the algorithms, these are some of the terms
to keep in mind since I'll use frequently use them:

- query vertex: the vertex the algorithm is currently processing
- outbound edge: an edge whose source vertex is the query vertex
- source vertex: the vertex at the left of an edge
- destination vertex: the vertex at the right of an edge
- successor (or successor vertex): the destination vertex of the next outbound
edge (edge order is unspecified)
- siblings (referring to vertices): all destination vertices of outbound edges
except for the successor

#### Depth-first search

Using a depth-first search strategy to visit a graph essentially means: visit
each vertex **once** and **visit successors before siblings**.
Here's what the algorithm looks like:

``` haskell
dfs :: [Vertex] -> Graph w l -> [Vertex]
dfs _ Empty = []
dfs [] _ = []
dfs (v:vs) g = case v `match` g of
  Nothing -> dfs vs g
  Just ((_,vtx,_,outs), g') -> vtx : dfs (nextvs outs ++ vs) g'

nextvs :: Context l w -> [Vertex]
```

`dfs` is a recursicve function that takes a list of input vertices and a graph
and returns a list of vertices sorted by traversing the graph in DFS-style.
If the graph or their input vertices are empty it returns the empty list,
otherwise it takes the current vertex `v` and `match`es it against the graph.
If `v` is a vertex in the graph,
`match` will first return its context and a new graph without it, append
`v` to the results list and finally the recursion will happen using as input
the list of destination vertices for all outbound edges of `v` appended to
the remaining source vertices and the new graph returned by the `match` function;
if `v` is not a vertex in the graph then it is simply ignored and the algorithm
recursively calls itself.
There key facts to understand about the algorithm are:

1. destination vertices are appended *in front of* the source vertices: this is what makes
the algorithm traversing the input graph depth-first. This is exactly what the
second invariant of DFS dictates : visit a successor before the sibilings.
2. the `match` function returns a new graph *without* the query vertex: this is
what the first invariant of DFS dictates - visit each vertex exectly once.
Since the new graph doesn't contain the query vertex there is no need for keeping
track of the visited vertices.

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
it will proceed with the next one. Here's what the algorithm looks like:

``` haskell
import Control.Arrow (second)

data Tree a = Nil | Node !a (Forest a) deriving Show

type Forest a = [Tree a]

dff :: [Vertex] -> Graph w l -> Forest Vertex
dff vs = fst . dff' vs

dff' :: [Vertex] -> Graph w l -> (Forest Vertex, Graph w l)
dff' [] g = ([], g)
dff' (v:vs) g = case v `match` g of
  Nothing -> dff' vs g
  Just (ctx, g') -> (Node v ts : forest, g'')
  where
    (ts, (forest, g'')) = second (dff' vs) (dff' (destvs ctx) g')

destvs :: Context l w -> [Vertex]
```

The `dff` function calls an auxiliary function `dff'` that does most of the work.
`dff` is as `dfs` a recursive function: it tirst it matches the source vertex `v`
and if `match`ing succeeds the function
calls itself with the successor vertices and the new graph as its input until the
list of source vertices is empty; when that happens `dff'` recurses using the
remaining source vertices and the graph resulted from running `dff'` for the
previous source vertex.
Again let's have a look at a very simple example built on top of the previous one:

``` haskell
ƛ: let g = read "mkG [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5)] [(1, 2, 5), (2, 1, 3), (2, 3, 1), (3, 1, 4), (4, 5, 7)]"
([(4,3),(3,2)],1,'a',[(5,2)]) :&: (([],2,'b',[(1,3)]) :&: (([],3,'c',[]) :&: Empty))
ƛ: dff (vertices g) g
[Node 1 [Node 2 [Node 3 []]], Node 4 [Node 5 []]]
```

#### Breadth-first search

Using a breadth-first search strategy to visit a graph essentially means: traverse
each vertex **once** and **visit siblings before successors**.
Here's what the algorithm looks like:

``` haskell
bfs :: Graph w l -> [Vertex] -> [Vertex]
bfs gr vs = bfs' gr vs
  where
    bfs' g svs
      | isEmpty g || null svs = []
      | otherwise = case v `match` g of
          Nothing -> bfs' g vs
          Just ((_,vtx,_,outs), g') -> vtx : dfs (vs ++ destvs outs) g'

destvs :: Context l w -> [Vertex]
```

There key facts to notice about the algorithm are:

1. destination vertices are appended *at the end of* the source vertices: this is what makes
the algorithm traversing the input graph breadth-first. This is exactly what the
second invariant of DFS dictates : visit siblings before the successor.
2. the `match` function returns a new graph *without* the query vertex: this is
what the first invariant of BFS dictates - visit each vertex exectly once.
Since the new graph doesn't contain the query vertex there is no need for keeping
track of the visited vertices.
3. the algorithm is basically the same as the one in `dfs`, the only thing that
changes is where destination vertices are appended: in case of BFS they're
appended at the end of the list, in case of DFS in front of it. To fully appreciate
this it might be useful to think of these algorithms in terms of the defining
strategies of the data structures they internally use: LIFO in case of DFS and a
FIFO in case of BFS.

One of the applications of BFS is finding the shortest path in a unweighted graph.
This time the algorithm chooses a different representation for the spanning forest,
the reason is that it's harder to build a tree of nodes because of the way
BFS works: **its recursion logic delivers nodes in a top-down fashion whereas the one
in DFS delivers them in a bottom-up fashion, which is the natural way of building
a tree** [EXPLAIN THIS BETTER].
The paper argues that representation is not that useful in some
applications of BFS spanning trees - like for example finding the shortest path
between two vertices in a unweighted graph - and that can be achieved representing
the spanning tree with a list of paths. Let's have a look at the implementation:

``` haskell
import Control.Arrow (first)

type Path = [Vertex]

-- Roots tree
type RTree = [Path]

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

    -- gets the query vertex from the first path in the list and the remaining paths
    -- paths will always be non-empty because the initial call to `bf` uses a
    -- non-empty list
    (p@(v:_), ps') = first head (splitAt 1 ps)

destvs :: Context label weight -> [Vertex]

esp :: Vertex -> Vertex -> Graph weight label -> Path
esp src dst = reverse . pathTo ((==dst) . head) . bft src
 where
   pathTo :: (a -> Bool) -> [a] -> a
   pathTo p = head . filter p
```

Instead of explaining what the `bf` function does step-by-step, let's have a look
at an example on a simple graph:

``` haskell
ƛ: let g = read "mkG [('a', 1), ('b', 2), ('c', 3)] [(1, 2, ()), (2, 1, ()), (2, 3, ()), (3, 1, ())]" :: Graph () Char
([(4,()),(3,())],1,'a',[(5,())]) :&: (([],2,'b',[(1,())]) :&: (([],3,'c',[]) :&: Empty))
ƛ: bf [[1]] g
[[1],[2,1],[3,2,1]]
```

The resulting spanning tree contains the shortest path from the source vertex to all
other vertices in reverse order. The `esp` function looks for the path in which
the first vertex is the destination of the path and reverses it. Notice that
since Haskell has non-strict sematics, `esp` stops as soon as the path to the target
destination vertex is found and that `bf` builds complete paths from a source to
a destination vertex without wasting any memory because list prefixes are shared.

#### Shortest path

The last algorithm I want to mention is a bit more convoluted and it's
Dijkstra's shortest path. First let's define two new auxiliary types:

``` haskell
-- Labelled vertex
type LVertex label = (label, Vertex)

newtype LPath label = LPath { unwrap :: [LVertex label] }

-- Labelled R-Tree (or Root Tree)
type LRTree label = [LPath label]

instance Eq label => Eq (LPath label) where ...

instance Ord label => Ord (LPath label) where ...
```

The algorithm uses a min-heap and some auxiliary functions to keep track of the
cheapest path

``` haskell
import qualified Data.Heap as Heap

getPath :: Vertex -> LRTree label -> Path
getPath v = reverse . map snd . unwrap . pathTo ((==v) . lv2v)
  where
    lv2v = snd . head . unwrap

expand :: Monoid weight
       => weight -> LPath weight -> Context weight label -> [LPath weight]
expand d (LPath p) (_, _, _, outs) =
  map (\(w, v) -> LPath ((w `mappend` d, v):p)) outs

mergeAll p@((dist, _):_) h0 =
  foldr Heap.insert h0 . expand dist (LPath p)
```

The `expand` function builds new `LPath`s whose label is the sum - let's assume
weights are positive integers for simplicity - of the distance walked so far and
the weight of the outbound edge. The `mergeAll` function takes these paths and
inserts them in the heap. The `getPath` function just extracts the path to the
given destination vertex from the list of paths.
Now let's have a look at the core of the algorithm:

``` haskell
dijkstra :: (Monoid weight, Ord weight)
         => Heap.Heap (LPath w) -> Graph weight label -> LRTree weight
dijkstra h g
  | isEmpty g = []
  | otherwise = case Heap.viewMin h of
      Nothing -> []
      Just (p, h') -> dijkstra' (p, h')
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
elegant and modular.

### A word on efficiency

If you didn't get lost in the woods of recursion and made it this far you might
have noticed that when talking about inductive graph and related algorithms I didn't
mention efficiency. This is because the implementations I've shown so far are
hopelessly inefficient but hopefully they provide an intuition about inductive
graphs, the motivation behind them and their goals. An efficient, real-world
implementation relies on more efficient data structures than lists and more
importantly a key aspect to make the algorithms run with asymptotically optimal
running times is that active patterns matching must have linear running times.
It's quite easy to notice that the toy implementation shown so far is not.
A real-world implementation based on Martin Erwigs' paper is available on
[Stackage](https://www.stackage.org/lts-9.14/package/fgl-5.5.3.1)
and if you're curious to know how it is possible to implement inductive graphs
efficiently I'll encourage to look at the source code.
I'll possibly cover the topic in a future blog post.


## Wrapping up
