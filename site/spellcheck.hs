#!/usr/bin/env stack
{- stack
script
--no-install-ghc
--resolver lts-9.14
--package conduit-combinators,text,filepath
-}

{-# LANGUAGE OverloadedStrings #-}
import Conduit
import qualified Data.Text as T
import System.FilePath (takeExtension)
import System.Environment (getArgs)

main :: IO ()
main = do
  [dir] <- getArgs
  runConduitRes $ getSpellCheckFiles dir >>= spellCheckFile >>= sinkSpellcheckFiles -- >>= runSpellchecker
  where
    runSpellchecker = undefined

    sinkSpellcheckFiles Nothing = return ()
    sinkSpellcheckFiles (Just (fp, xs)) = yield xs
      .| mapC (T.unlines . reverse)
      .| encodeUtf8C
      .| sinkFile (takeWhile (/='.') fp ++ ".spellcheck")

    getSpellCheckFiles dir = sourceDirectory dir
      .| mapMC (\x -> do liftIO $ print x ; return x)
      .| filterC (\fp -> takeExtension fp `elem` [".markdown", ".md", ".lhs"])
      .| await

    spellCheckFile Nothing = return Nothing
    spellCheckFile (Just fp) = sourceFile fp
      .| decodeUtf8C
      .| mapC T.lines
      .| foldlCE removeCodeSnippet (False, [])
      >>= \(_, xs) -> return $ Just (fp, xs)


removeCodeSnippet :: (Bool, [T.Text]) -> T.Text -> (Bool, [T.Text])
removeCodeSnippet (isCode, ls) l
  | isCodeSnippet l && isCode = (False, ls)
  | isCodeSnippet l = (True, ls)
  | isCode = (True, ls)
  | otherwise = (False, l:ls) -- FIX: bottle-neck
  where
    isCodeSnippet l = isCodeSnippetMd l || isCodeSnippetLhs l

    isCodeSnippetMd = (=="```") . T.take 3

    isCodeSnippetLhs l = birdStyle l || latexStyle l
      where
        birdStyle = (=="> ") . T.take 2

        latexStyle = (`elem`["\\begin{code}", "\\end{code}"])
