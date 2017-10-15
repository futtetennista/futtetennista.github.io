{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Hakyll
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
        -- compile $ pandocCompilerWith defaultHakyllReaderOptions{ readerExtensions = Set.insert (Ext_literate_haskell) (readerExtensions defaultHakyllReaderOptions) } defaultHakyllWriterOptions
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
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
              <> field "title" (return . show . itemBody)
              <> field "excerpt" (return . show . itemBody)
              -- <> field "categories" (return . show . itemBody)
              -- <> field "tags" (return . show . itemBody)
              <> defaultContext

          getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    match (fromGlob "partials/*" .||. fromGlob "templates/*") $
      compile templateBodyCompiler


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
