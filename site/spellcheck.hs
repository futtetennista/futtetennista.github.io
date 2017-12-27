#!/usr/bin/env stack
{- stack
script
--no-install-ghc
--resolver lts-9.14
--package conduit-combinators,text,filepath,mtl,turtle,foldl
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
import Conduit
import qualified Data.Text as T
import System.FilePath (replaceExtension, takeExtension)
import System.Environment (getArgs)
import qualified Control.Monad.State.Strict as S
import Turtle
import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import GHC.IO (FilePath)
import Control.Monad (foldM, when)
import System.Exit (exitFailure)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty(..))
import Data.Bifunctor


main :: IO ()
main = program

program
  :: (MonadHunspell m, MonadGit m, MonadPrint m, MonadBaseControl IO m, MonadExit m, MonadIO m)
  => m ()
program = do
  n <- foldM countTypos 0 =<< getContentFiles
  when (n > 0) $ printTotalTyposAndFail n
  where
    printTotalTyposAndFail :: (Monad m, MonadExit m, MonadPrint m) => Int -> m ()
    printTotalTyposAndFail n
      | n > 1 = printit (show n ++ " typos found") >> exitFail
      | otherwise = printit "1 typo found" >> exitFail

    countTypos n fp = printit ("Spellchecking: " ++ fp)
      >> runSpellchecker fp
      >>= count
      where
        count m = let !tot = n + m in pure tot

class Monad m => MonadExit m where
  exitFail :: m ()

instance MonadExit IO where
  exitFail = exitFailure

type Args = [Text]

type InputLine = Maybe Text

class MonadThrow m => MonadGit m  where
  git :: Args -> InputLine -> m [String]

instance MonadThrow Shell where
  throwM _ = empty

instance MonadGit IO where
  git args = exec . maybe empty toLine
    where
      exec ins = let res = inproc "git" args ins in toListOfStrings <$> fold res Fold.list

      toListOfStrings = map (T.unpack . lineToText)

      toLine = maybe (throwM NewlineForbidden) pure . textToLine

getContentFiles :: MonadGit m => m [GHC.IO.FilePath]
getContentFiles = filter markdown <$> stagedFiles
  where
    markdown = (`elem` [".markdown", ".md"]) . takeExtension

    stagedFiles :: MonadGit m => m [String]
    stagedFiles =  git (T.words "--no-pager diff --cached --name-only") Nothing

runSpellchecker
  :: (MonadBaseControl IO m, MonadPrint m, MonadHunspell m, MonadIO m)
  => GHC.IO.FilePath -> m Int
runSpellchecker fp = S.evalStateT spellcheckerState empty
  where
    empty = State False 1

    spellcheckerState = runConduitRes $ spellcheckFile fp

data State a b = State { isCode :: !a
                       , lineNum :: !b
                       }

instance Bifunctor State where
  bimap f g (State x y) = State (f x) (g y)

type StateT = S.StateT (State Bool LineNum)

-- it kinda defies the whole purpose of having task-specific type classes that
-- I have to add the MonadIO contet here but MonadResource needs it so not much
-- I can do about it or...?!
spellcheckFile
  :: (MonadHunspell m, MonadPrint m, MonadBase IO m, MonadIO m)
  => GHC.IO.FilePath -> ConduitM a c (ResourceT (StateT m)) Int
spellcheckFile fp = sourceFile fp
  .| toLinesC
  .| spellcheckC
  .| processResultsC
  -- sourceDirectory dir
  -- .| mapMC (\x -> liftIO $ print x >> return x)
  -- .| filterC (\fp -> takeExtension fp `elem` [".markdown", ".md", ".lhs"])
  -- .| awaitForever sourceFile

class Monad m => MonadPrint m where
  printit :: (Show a) => a -> m ()

instance MonadPrint IO where
  printit = print

processResultsC
  :: (MonadPrint m)
  => ConduitM Suggestion c (ResourceT (StateT m)) Int
processResultsC = mapMC printAndReturn .| lengthCE
  where
    printAndReturn xs = lift (lift $ printit xs) >> return xs

spellcheckC
  :: MonadHunspell m
  => Conduit Text (ResourceT (StateT m)) Suggestion
spellcheckC = mapMC (spellcheck liftHunspell)
  .| filterC (not . T.null . snd)
  .| mapMC toSugg
  where
    args = T.words "-d en_US -p custom_dict"

    liftHunspell n = lift . lift . hunspell args n

    toSugg :: MonadHunspell m => (Int, Text) -> (ResourceT (StateT m)) Suggestion
    toSugg (n, xs) = pure (n, (typo, suggs))
      where
        xs' = T.splitOn " " xs

        -- hunspell output ex: "& typo 1 12: sugg1 sugg2 sugg3\n"
        (typo, suggs) = (xs' !! 1,  T.unlines $ drop 4 xs')

toLinesC
  :: MonadThrow m
  => Conduit BS.ByteString (ResourceT (StateT m)) Text
toLinesC = decodeUtf8C .| linesUnboundedC

-- this is convenient since (,) has already an instance for MonoFoldable
type Suggestion = (LineNum, (Text, Text))

type LineNum = Int

type Spellchecker m = Args -> LineNum -> InputLine -> m (LineNum, Text)

class MonadThrow m => MonadHunspell m where
  hunspell :: Spellchecker m

instance MonadHunspell IO where
  hunspell args n ml = (,) <$> pure n <*> toText `fmap` fold outLine Fold.list
    where
      toText
        | "-w" `elem` args || "-l" `elem` args = T.unlines . map lineToText
        | otherwise = T.unlines . filter suggestion . map lineToText . drop 1

      outLine = inproc "hunspell" args inLine

      -- hunspell output ex: "& typo 1 12: sugg1 sugg2 sugg3\n"
      suggestion = (=="& ") . T.take 2

      inLine = maybe empty (pure . unsafeTextToLine) ml

spellcheck
  :: S.MonadState (State Bool LineNum) m
  => (LineNum -> InputLine -> m (LineNum, Text)) -> Text -> m (LineNum, Text)
spellcheck f l
  | isCodeSnippet l = do
      n <- S.gets lineNum
      S.modify' (bimap not (+1))
      return (n, T.empty)
  | otherwise = do
      State codeSnippet n <- S.get
      S.modify' (second (+1))
      if codeSnippet then return (n, T.empty) else f n (Just l)
  where
    isCodeSnippet l = isCodeSnippetMd l || isCodeSnippetLhs l

    isCodeSnippetMd = (=="```") . T.take 3

    isCodeSnippetLhs l = birdStyle l || latexStyle l
      where
        birdStyle = (=="> ") . T.take 2

        latexStyle = (`elem`["\\begin{code}", "\\end{code}"])
