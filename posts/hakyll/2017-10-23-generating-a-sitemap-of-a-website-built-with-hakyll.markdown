---
title: Generating a sitemap of a website built with Hakyll
author: futtetennista
tags: hakyll, seo, titbit
---
Adding `sitemap.xml` and `robots.txt` to a website built with Hakyll is not
explicitly documented but it ended up being quite easy with the help of some
DuckDuckGo-fu.
<!--more-->
A quick [research](https://duckduckgo.com/?q=hakyll+sitemap.xml) returns
[this post](https://www.rohanjain.in/hakyll-sitemap/). With a few amendments
the solution proposed in the post works like a charm, here's the revised version
I use for my website:

``` haskell
  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/**"
      pages <- loadAll "pages/*"
      let
        crawlPages =
          sitemapPages pages ++ posts
        sitemapCtx =
          mconcat [ listField "entries" defaultContext (return crawlPages)
                  , defaultContext
                  ]
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
        >>= relativizeUrls

  match (fromList ["robots.txt", "CNAME"]) $ do
    route idRoute
    compile $ getResourceBody >>= relativizeUrls
```

Notice how the `robots.txt` and `CNAME` files - the latter is needed by my
domain name registrar - are simply copied since there's no need to apply any
processing to them.
