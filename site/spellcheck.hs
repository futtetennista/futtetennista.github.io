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


main :: IO ()
main = program

program = do
  n <- foldM countTypos 0 =<< contentFiles
  when (n > 0) (explainFailure n)
  where
    explainFailure n = liftIO (print $ show n ++ " typos found") >> exitFailure

    countTypos n fp = runSpellchecker fp >> return 1
      where
        count m = let !tot = n + m in tot

type Args = [Text]

type InputLine = Maybe Text

class (MonadThrow m) => MonadGit m  where
  git :: Args -> InputLine -> m [String]

instance MonadThrow Shell where
  throwM _ = empty

instance MonadGit IO where
  git args = exec . maybe empty toLine
    where
      exec ins = let res = inproc "git" args ins in toListOfStrings <$> fold res Fold.list

      toListOfStrings = map (T.unpack . lineToText)

      toLine = maybe (throwM NewlineForbidden) pure . textToLine

contentFiles :: MonadGit m => m [String]
contentFiles = filter markdownFile <$> files
  where
    markdownFile = (`elem` [".markdown", ".md"]) . takeExtension

    files :: MonadGit m => m [String]
    files =  git (T.words "--no-pager diff --cached --name-only") Nothing

-- runSpellchecker
--   :: (MonadBaseControl IO m, MonadHunspell m, MonadResource m, MonadPrint m) =>
--      GHC.IO.FilePath -> m Integer
runSpellchecker fp = S.evalStateT state False
  where
    state = runConduitRes $ spellcheckFile fp

type State = S.StateT Bool

-- spellcheckFile
--   :: (MonadHunspell m, MonadResource m, MonadPrint m)
--   => GHC.IO.FilePath -> ConduitM a c (ResourceT (State m)) Integer
spellcheckFile fp = -- sourceDirectory dir
  -- .| mapMC (\x -> liftIO $ print x >> return x)
  -- .| filterC (\fp -> takeExtension fp `elem` [".markdown", ".md", ".lhs"])
  -- .| awaitForever sourceFile
  sourceFile fp
  .| toLinesC
  .| spellcheckC
  .| showAndFoldResultsC

class MonadPrint m where
  printit :: (Show a) => a -> m ()

instance MonadPrint IO where
  printit = print

showAndFoldResultsC
  :: (Monad m, MonadPrint m)
  => ConduitM [Text] c (ResourceT (State m)) Integer
showAndFoldResultsC = mapMCE printAndReturn .| lengthCE
  where
    printAndReturn xs = lift (lift $ printit xs) >> return xs

spellcheckC
  :: MonadHunspell m
  => Conduit [Text] (ResourceT (State m)) [Text]
spellcheckC = mapMCE (spellcheck liftedHunspell) .| filterCE (not . T.null)
  where
    args = T.words "-d en_US -p custom_dict"

    liftedHunspell = lift . lift . hunspell args -- aaarrrggghhh

-- TODO: use `linesUnboundedC` instead of `mapC T.lines` ?
toLinesC
  :: (MonadThrow m, MonadIO m)
  => Conduit BS.ByteString (ResourceT (State m)) [Text]
toLinesC = decodeUtf8C .| mapC T.lines .| filterCE (not . T.null)

-- newtype Suggestions a = Sugg (a, a)
-- type Spellchecker m a = Text -> m (Suggestions a)
type Spellchecker m = Args -> InputLine -> m Text

class (MonadThrow m) => MonadHunspell m where
  hunspell :: Spellchecker m

instance MonadHunspell IO where
  hunspell args l = toText <$> fold output Fold.list
    where
      toText
        | "-w" `elem` args || "-l" `elem` args = T.unlines . map lineToText
        | otherwise = T.unlines . filter isSuggestion . map lineToText . drop 1

      output = inproc "hunspell" args input

      isSuggestion = (=="& ") . T.take 2

      input = maybe empty (pure . unsafeTextToLine) l

spellcheck
  :: S.MonadState Bool m
  => (InputLine -> m Text) -> Text -> m Text
spellcheck s l
  | isCodeSnippet l = S.modify' not >> pure T.empty
  | otherwise = do isCode <- S.get; if isCode then pure T.empty else s (Just l)
  where
    isCodeSnippet l = isCodeSnippetMd l || isCodeSnippetLhs l

    isCodeSnippetMd = (=="```") . T.take 3

    isCodeSnippetLhs l = birdStyle l || latexStyle l
      where
        birdStyle = (=="> ") . T.take 2

        latexStyle = (`elem`["\\begin{code}", "\\end{code}"])
