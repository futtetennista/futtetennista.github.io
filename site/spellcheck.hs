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


main :: IO ()
main = mapM_ spellcheck =<< contentFiles
  where
    spellcheck fp = do
      print $ "spellchecking '" ++ fp ++ "'"
      massageFileForSpellchecker fp
      runSpellchecker fp
    -- spellcheck _ = print "Usage: ./site.hs /path/to/file/to/spellcheck"

contentFiles :: MonadIO io => io [String]
contentFiles = (toListOfStrings . filter markdownFile) <$> files
  where
    toListOfStrings = map (T.unpack . lineToText)

    markdownFile = (`elem` [".markdown", ".md"]) . takeExtension . T.unpack . lineToText

    files :: MonadIO io => io [Line]
    files = let changed = inproc "git" ["--no-pager", "diff", "--cached", "--name-only"] empty
            in fold changed Fold.list

mkFilePath = flip replaceExtension ".spellcheck"

runSpellchecker fp = stdout (inproc "hunspell" args empty) >> void (proc "rm" [file] empty)
  where
    args = ["-d en_GB", "-w", "-p custom_dict", file]

    file = T.pack $ mkFilePath fp

massageFileForSpellchecker fp = S.evalStateT (runConduitRes $ spellCheckFile fp) False

spellCheckFile fp = -- sourceDirectory dir
  -- .| mapMC (\x -> liftIO $ print x >> return x)
  -- .| filterC (\fp -> takeExtension fp `elem` [".markdown", ".md", ".lhs"])
  -- .| awaitForever sourceFile
  sourceFile fp
  .| decodeUtf8C
  .| mapC T.lines
  .| mapMCE removeCodeSnippet
  .| filterCE (not . T.null)
  .| mapC T.unlines
  .| encodeUtf8C
  .| sinkFile (mkFilePath fp)

removeCodeSnippet l
  | isCodeSnippet l = S.modify' not >> return T.empty
  | otherwise = do isCode <- S.get; return $ if isCode then T.empty else l
  where
    isCodeSnippet l = isCodeSnippetMd l || isCodeSnippetLhs l

    isCodeSnippetMd = (=="```") . T.take 3

    isCodeSnippetLhs l = birdStyle l || latexStyle l
      where
        birdStyle = (=="> ") . T.take 2

        latexStyle = (`elem`["\\begin{code}", "\\end{code}"])
