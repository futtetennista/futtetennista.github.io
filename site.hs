{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Data.Monoid ((<>))
import Data.List (stripPrefix)
import qualified Data.Set as Set
import Hakyll.Web.Sass (sassCompiler)


main :: IO ()
main = hakyllWith hakyllConfig $ do
  match (fromGlob "images/**" .||. fromGlob "js/**" .||. fromGlob "lib/**") $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*.scss"$ do
    route $ setExtension "css"
    compile (fmap compressCss <$> sassCompiler)

  match (fromList [ "pages/about.markdown"
                  , "pages/contact.markdown"
                  , "pages/imprint.markdown"
                  ]) $ do
    route $
      customRoute (maybe (error "Expected pages to be in 'pages' folder")
                         id . stripPrefix "pages/" . toFilePath)
      `composeRoutes` setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  tags <- buildTags "posts/**" (fromCapture "tags/*.html")
  createTagsRules tags (\xs -> "Posts tagged \"" ++ xs ++ "\"")

  categories <- buildCategories "posts/**" (fromCapture "categories/*.html")
  createTagsRules categories (\xs -> "Posts categorised as \"" ++ xs ++ "\"")

  match "posts/**" $ do
    route $ setExtension "html"
    let
      namedTags =
        [("tags", tags), ("categories", categories)]
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" (ctxWithTags postCtx namedTags)
      >>= loadAndApplyTemplate "templates/default.html" (ctxWithTags postCtx namedTags)
      >>= relativizeUrls


  match "pages/index.html" $ do
    route (constRoute "index.html")
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

  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      let
        feedCtx =
          postCtx <> bodyField "description"
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/**" "content"
      renderAtom futtetennismoFeedConfiguration feedCtx posts


ctxWithTags :: Context String -> [(String, Tags)] -> Context String
ctxWithTags ctx =
  foldr (\(name, tags) baseCtx -> tagsField name tags <> baseCtx) ctx


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


createTagsRules :: Tags -> (String -> String) -> Rules ()
createTagsRules tags mkTitle =
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
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls


futtetennismoFeedConfiguration :: FeedConfiguration
futtetennismoFeedConfiguration =
  FeedConfiguration { feedTitle       = "Futtetennismo"
                    , feedDescription = ""
                    , feedAuthorName  = "futtetennista"
                    , feedAuthorEmail = "futtetennista@gmail.com"
                    , feedRoot        = "https://futtetennismo.io"
                    }


hakyllConfig :: Configuration
hakyllConfig =
  defaultConfiguration{ previewHost = "0.0.0.0" }
