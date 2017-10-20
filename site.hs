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

  tags <- buildTags "posts/**" (fromCapture "tags/*.html")
  createTagsRules tags
                  (\xs -> "Posts tagged \"" ++ xs ++ "\"")
                  "templates/tag.html"

  categories <- buildCategories "posts/**" (fromCapture "categories/*.html")
  createTagsRules categories
                  (\xs -> "Posts categorised as \"" ++ xs ++ "\"")
                  "templates/category.html"

  match "posts/**" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" (ctxWithTags tags categories postCtx)
      >>= loadAndApplyTemplate "templates/default.html" (ctxWithTags tags categories postCtx)
      >>= relativizeUrls

  -- create ["archive.html"] $ do
  --     route idRoute
  --     compile $ do
  --         posts <- recentFirst =<< loadAll "posts/**"
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
      posts <- recentFirst =<< loadAll "posts/**"
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


ctxWithTags :: Tags -> Tags -> Context String -> Context String
ctxWithTags tags categories ctx =
  tagsField "tags" tags
  <> tagsField "categories" categories
  <> ctx


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


createTagsRules :: Tags -> (String -> String) -> Identifier -> Rules ()
createTagsRules tags mkTitle template =
  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let
        ctx =
          constField "title" (mkTitle tag)
          <> listField "posts" postCtx (return posts)
          <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate template ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
