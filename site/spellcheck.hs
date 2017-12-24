#!/usr/bin/env stack
{- stack
script
--no-install-ghc
--resolver lts-9.14
--package conduit-combinators,text,filepath,mtl,turtle,foldl
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Conduit
import qualified Data.Text as T
import System.FilePath (replaceExtension, takeExtension)
import System.Environment (getArgs)
import qualified Control.Monad.State.Strict as S
import Turtle
import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import GHC.IO (FilePath)


main :: IO ()
main = mapM_ spellcheck =<< contentFiles
  where
    spellcheck fp = do
      print $ "spellchecking '" ++ fp ++ "'"
      runSpellchecker fp

class (MonadThrow m, MonadIO m) => MonadGit m  where
  runGit :: Text -> [Text] -> Maybe Text -> m [String]

instance MonadThrow Shell where
  throwM _ = empty

instance MonadGit IO where
  runGit cmd args = runGit' . maybe empty toLine
    where
      runGit' ins = let res = inproc cmd args ins in toListOfStrings <$> fold res Fold.list

      toListOfStrings = map (T.unpack . lineToText)

      toLine = maybe (throwM NewlineForbidden) pure . textToLine

contentFiles :: MonadGit m => m [String]
contentFiles = filter markdownFile <$> files
  where
    markdownFile = (`elem` [".markdown", ".md"]) . takeExtension

    files :: MonadGit m => m [String]
    files =  runGit "git" ["--no-pager", "diff", "--cached", "--name-only"] Nothing

runSpellchecker fp = S.evalStateT state False
  where
    state = runConduitRes $ spellcheckFile fp

spellcheckFile fp = -- sourceDirectory dir
  -- .| mapMC (\x -> liftIO $ print x >> return x)
  -- .| filterC (\fp -> takeExtension fp `elem` [".markdown", ".md", ".lhs"])
  -- .| awaitForever sourceFile
  sourceFile fp
  .| contentToLines
  .| mapMCE (spellcheck spellcheckLine)
  .| outputTypos

contentToLines = decodeUtf8C .| mapC T.lines .| filterCE (not . T.null)

outputTypos = filterCE (not . T.null) .| mapC T.unlines .| encodeUtf8C .| stdoutC

type Spellchecker m = Text -> m Text

spellcheckLine l = toText <$> fold outlines Fold.list
  where
    toText = T.unlines . filter typos . map lineToText . drop 1

    outlines = inproc "hunspell" args inline

    typos = (=="& ") . T.take 2

    args = ["-d", "en_US", "-p", "custom_dict"]

    inline = pure $ unsafeTextToLine l

spellcheck s l
  | isCodeSnippet l = S.modify' not >> pure T.empty
  | otherwise = do isCode <- S.get; if isCode then pure T.empty else s l
  where
    isCodeSnippet l = isCodeSnippetMd l || isCodeSnippetLhs l

    isCodeSnippetMd = (=="```") . T.take 3

    isCodeSnippetLhs l = birdStyle l || latexStyle l
      where
        birdStyle = (=="> ") . T.take 2

        latexStyle = (`elem`["\\begin{code}", "\\end{code}"])
