{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Hakyll.Web.Sass (sassCompiler)

main :: IO ()
main = hakyll $ do
  match (fromGlob "images/**" .||. fromGlob "js/**" .||. fromGlob "lib/**") $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*.scss"$ do
    route $ setExtension "css"
    let
      compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList ["about.rts", "contact.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  -- create ["archive.html"] $ do
  --     route idRoute
  --     compile $ do
  --         posts <- recentFirst =<< loadAll "posts/*"
  --         let archiveCtx =
  --                 listField "posts" postCtx (return posts) `mappend`
  --                 constField "title" "Archives"            `mappend`
  --                 defaultContext

  --         makeItem ""
  --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
  --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
  --             >>= relativizeUrls


  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let
        indexCtx =
          listField "posts" postCtx (return posts)
          <> constField "title" "Home"
          <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match (fromGlob "partials/*" .||. fromGlob "templates/*") $
    compile templateBodyCompiler


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
    <> teaserField "teaser" "content"
    <> mapContext (trim . take 160 . stripTags) (teaserField "teaser-short" "content")
    <> defaultContext
  where
    trim xs =
      map snd . filter trim' $ zip [0..] xs
      where
        trim' (ix, x)
          | ix == 0 || ix == (length xs - 1) =
              x `notElem` [' ' , '\n', '\t']
          | otherwise =
              True
