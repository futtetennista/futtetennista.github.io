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
    compile (fmap compressCss <$> sassCompiler)

  match (fromList ["about.rts", "contact.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let
        ctx =
          constField "title" title
          <> listField "posts" postCtx (return posts)
          <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
      >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
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


postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
  tagsField "tags" tags <> postCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
    <> teaserField "teaser" "content"
    -- create a short version of the teaser. Strip out HTML tags and trim.
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
