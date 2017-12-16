#!/usr/bin/env stack
{- stack
script
--no-install-ghc
--resolver lts-9.14
--package conduit-combinators,text,filepath,mtl,turtle,system-filepath
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Conduit
import qualified Data.Text as T
import System.FilePath (replaceExtension)
import System.Environment (getArgs)
import qualified Control.Monad.State.Strict as S
import Turtle
import Prelude hiding (FilePath)

main :: IO ()
main = do
  [fp] <- getArgs
  massageFileForSpellchecker fp
  runSpellchecker fp
  where
    mkFilePath = flip replaceExtension ".spellcheck"

    massageFileForSpellchecker fp = S.evalStateT (runConduitRes $ spellCheckFile fp) False

    runSpellchecker fp = stdout (inproc "hunspell" args empty) >> sh (inproc "rm" [file] empty)
      where
        args = ["-d en_GB", "-w", "-p custom_dict", file]

        file = T.pack $ mkFilePath fp

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
