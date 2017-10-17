---
title: Migrating from lazy IO to Conduit
tags: [haskell]
---

\ignore{
https://wiki.haskell.org/Literate_programming

> -- HIDDEN
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> import qualified Data.ByteString.Lazy.Char8 as Lazy
> import qualified Data.ByteString.Char8 as Strict
> import Control.Monad.IO.Class
> import Conduit
> import qualified Data.Set as Set
> import Control.Concurrent.STM
> import Control.Monad.State

}

Lazy IO is so tricky to get right and has some intrinsic limitations that the usual recommendation is to simply avoid it. On the other hand sometimes it's not desirable (or even possible) to use strict IO, mostly for memory efficiency reasons. This is the kind of problems that streaming libraries like [conduit](https://hackage.haskell.org/package/conduit) or [pipes]("https://hackage.haskell.org/package/pipes") are designed to solve. In this post I want to show how I refactored a piece of code that uses lazy IO to use the conduit library (for those not familiar with it, please read this [conduit tutorial](https://haskell-lang.org/library/conduit) first).
<!--more-->
The example is based on the URL checker developed in chapter 28 of Real World Haskell: the URL checker parses some command line arguments - input files containing the urls to be checked and the number of worker threads that will concurrently check those urls - creates a `Job` that extracts all the well-formed urls, a `Task` for each url that needs to be checked and puts it in a job queue that worker threads poll to get new urls to check until the job queue is empty. After all urls are checked the URL checker prints out some statistics about those URLs. The types are the following:

> data Task = Done | Check URL

> type URL = Lazy.ByteString

> data JobState =
>   JobState { linksSeen :: Set.Set URL
>            , linksFound :: !Int
>            , linkQueue :: TChan Task
>            }

> newtype Job a =
>   Job { runJob :: StateT JobState IO a }
>   deriving (Functor, Applicative, Monad, MonadState JobState, MonadIO)

The `checkUrls` function glues together a few things: extracting the urls from the input file, filtering out duplicates, enqueueing the tasks in the job queue and updating the statistics

> checkURLs :: FilePath -> Job ()
> checkURLs f = do
>   src <- liftIO $ Lazy.readFile f
>   let
>     urls = extractLinks src
>   uniqueUrls <- filterM seenURI urls
>   mapM_ insertURI uniqueUrls
>   enqueueTasks uniqueUrls
>   updateStats (length urls)

> updateStats :: Int -> Job ()
> updateStats numUrls =
>   modify $ \s -> s { linksFound = linksFound s + numUrls }
> seenURI :: URL -> Job Bool
> seenURI url =
>   (not . Set.member url) <$> gets linksSeen
> insertURI :: URL -> Job ()
> insertURI url =
>   modify $ \s -> s { linksSeen = Set.insert url (linksSeen s) }

> enqueueTasks :: [URL] -> Job ()
> enqueueTasks urls = do
>   q <- gets linkQueue
>   liftIO . atomically $ mapM_ (writeTChan q . Check) urls

> extractLinks :: Lazy.ByteString -> [URL]
> extractLinks =
>   Lazy.lines -- filtering of invalid urls omitted

For the version using conduits we'll aim to remove everything that relies on lazy IO, using strict `ByteString`s and conduits. There is just one change needed in the types, namely re-defining the `URL` type alias

> data Task' = Done' | Check' URL'

> type URL' = Strict.ByteString

> data JobState' =
>   JobState' { linksSeen' :: Set.Set URL'
>             , linksFound' :: !Int
>             , linkQueue' :: TChan Task'
>             }

> newtype Job' a =
>   Job' { runJob' :: StateT JobState' IO a }
>   deriving (Functor, Applicative, Monad, MonadState JobState', MonadIO)

The `checkURL` function is - as you might expect - quite different given how the conduit library is designed. In conduit "everything is driven by the downstream" so I found it useful to ask myself this question: what output does the function need to produce? In this case `checkURLs` needs to do essentially two things: 1) creating and enqueuing `Task`s to be picked up by worker threads and 2) updating some statistics in `JobState`. The first shift in thinking is that I found necessary is to think only in terms of pipelines and leave out `let` bindings. This poses a challenge though: the extracted urls are needed for both 1) and 2) but once they go through 1) urls are transformed into a job and that's not what 2) expects as an input. I found two to three possible solutions to the problem: changing the signatures of the helper functions so that the input urls are always returned wrapped in a monad (this reminded me of the "fluent" style used for example for builders in languages like Java) to allow the stream to "keep flowing downstream", using zipped conduits and a mix of the two. The `ZipCounduit` is a handy type that makes it possible to split the stream into two identical streams that can be consumed by two different downstream conduits: this way both 1) and 2) can get the input data they expect. I'm not entirely sure what's more idiomatic or elegant or - more importantly - clear though.

First let's start with the helper functions (I'll just write type signatures for their variations in the following snippets), their implementation is the same but the type signature of most of them is slightly different - more on this below

> extractLinks' :: Strict.ByteString -> [URL']
> extractLinks' =
>   Strict.lines -- filtering of invalid urls omitted

> updateStats' :: MonadState JobState' m => Int -> m ()
> updateStats' numUrls =
>   modify $ \s -> s { linksFound' = linksFound' s + numUrls }

> seenURI' :: MonadState JobState' m => URL' -> m Bool
> seenURI' url = do
>   (not . Set.member url) <$> gets linksSeen'

> insertURI' :: MonadState JobState' m => URL' -> m ()
> insertURI' url = do
>   modify $ \s -> s { linksSeen' = Set.insert url (linksSeen' s) }

> enqueueTasks' :: (MonadState JobState' m, MonadIO m) => [URL'] -> m ()
> enqueueTasks' urls = do
>   queue <- gets linkQueue'
>   liftIO . atomically $ mapM_ (writeTChan queue . Check') urls

The the first solution - no `ZipConduits`s involved - looks like this

\begin{code}
checkURLs' :: FilePath -> Job' ()
checkURLs' fp =
  Job' $
    runConduitRes $ sourceFileBS fp
      .| mapC extractLinks'
      .| filterMCE seenURI'
      .| mapMCE insertURI'
      .| mapMC enqueueTasks'
      .| mapM_C (updateStats' . length)

updateStats' :: MonadState JobState' m => Int -> m ()
insertURI' :: MonadState JobState' m => URL' -> URL' ()
enqueueTasks' :: (MonadState JobState' m, MonadIO m) => [URL'] -> m [URL']
\end{code}

The second solution uses two `ZipConduit`s

> checkURLs' :: FilePath -> Job' ()
> checkURLs' fp =
>   Job' $
>     runConduitRes $ sourceFileBS fp
>       .| mapC extractLinks'
>       .| setupJob
>   where
>     setupJob :: Consumer [URL'] (ResourceT (StateT JobState' IO)) ()
>     setupJob =
>       getZipConduit $
>         ZipConduit insertAndEnqueue
>         *> ZipConduit (mapM_C (updateStats' . length))

>     insertAndEnqueue :: Consumer [URL'] (ResourceT (StateT JobState' IO)) ()
>     insertAndEnqueue =
>       filterMCE seenURI' .| (getZipConduit $
>                                ZipConduit (mapM_CE insertURI')
>                                <* ZipConduit (mapM_C enqueueTasks'))

Finally the third solution uses one `ZipConduits`s and modifies `insertURI'` to return a `URL'` so that the stream can "keep flowing down"

\begin{code}
checkURLs' :: FilePath -> Job' ()
checkURLs' fp =
  Job' $
    runConduitRes $ sourceFileBS fp
      .| mapC extractLinks'
      .| setupJob
  where
    setupJob :: Consumer [URL'] (ResourceT (StateT JobState' IO)) ()
    setupJob =
      getZipConduit $
        ZipConduit (filterMCE seenURI'
                      .| mapM_CE insertURI'
                      .| mapM_C enqueueTasks')
        *> ZipConduit (mapM_C (updateStats' . length))

insertURI' :: MonadState JobState' m => URL' -> URL' ()
\end{code}

The type signatures of most of this helper functions is slightly different - namely it's more general: why is this needed? If the type signature of `updateStats'` was `updateStats' :: Int -> Job' ()` the compiler would complain with the following error: `Couldn't match type ‘Job’ with ‘ResourceT (StateT JobState IO)’`. It took me a bit to fix this and make the compiler happy, again I'm not entirely sure that's the best way of solving the issue but it works. My first try was - following compiler errors - to make `Job` an instance of `MonadThrow`, `MonadBase` but I stopped before implementing an instance for `MonadBaseControl` since it couldn't be derived atomatically and I was under the impression that it was too much of a hassle giving that `Job'` is just a `newtype` wrapper for `StateT`, which is already an instance of `MonadBaseControl`. If I could take the `StateT` transformer and then just wrap it in a `Job'` constructor then that would do the job...and that's made possible by modifying the type signatures of those functions. Actually if I had just type inference do its job it'd have inferred the types correctly, but I'm used to write type signatures first and then write an implementation and that bit me this time.

To check that all this works as expected, let's try it out in GHCI

```haskell
ƛ queue <- newTChanIO :: IO TChan Task'
queue :: TChan Task'
ƛ let job = checkURLs' "urls.txt" -- urls.txt contains a list of urls
job :: Job' ()
ƛ st <- execStateT (runJob' job) (JobState' Set.empty 0 queue)
st :: JobState'
ƛ :m +Control.Exception
ƛ assert (linksFound' st > 0) ("Found " ++ linksFound' st ++ " links")
"Found 2 links"
ƛ assert (linksSeen' st > 0) ("Seen " ++ linksSeen' st ++ " links")
"Seen 3 links"
ƛ emptyQueue <- atomically $ isEmptyTChan queue
emptyQueue :: Bool
ƛ assert (not emptyQueue) "Queue not empty"
"Queue not empty"
```

Wrapping up
---

In this post I shown how to refactor a piece of code using lazy IO to use the conduit library to write a little program that reads data from files efficiently when it comes to memory usage, illustrated some of the challenges I faced while doing that and explained some of possible solutions I found.
