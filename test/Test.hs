{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Turtle
import Turtle.Line (lineToText)
import qualified Control.Foldl as Fold
import Prelude hiding (FilePath)
import Data.Text (isInfixOf)


main :: IO Counts
main =
  runTestTT tests


tests :: Test
tests =
  TestList [ TestLabel "check links" checkLinks ]


checkLinks :: Test
checkLinks =
  TestCase $ do
    let
      cmd =
        "stack exec site rebuild && stack exec site check"
    brokenLinks <- fold (inshell cmd empty) $ Fold.any checkBrokenLinkMsg
    assertBool "Oh no! Broken links found" (not brokenLinks)
      where
        checkBrokenLinkMsg =
          isInfixOf "[ERROR]" . lineToText
