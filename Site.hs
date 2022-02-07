{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Monoid ((<>), mappend, mconcat)

import Text.Pandoc
import Hakyll
import Hakyll.Web.Pandoc
import Hakyll.Web.Sass (sassCompiler)

navbar =  [ ("Home", "/index.html")
          , ("About", "/about.html")
          , ("Research", "/research.html")
          , ("Posts", "/archive.html")
          ]

main = hakyllWith defaultConfiguration $ do
  match "img/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*.sass" $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

  match "css/*" $ do
    route   idRoute
    compile copyFileCompiler --compressCssCompiler

  match "js/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "files/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler

  match "*.md" $ do
    route   $ setExtension "html"
    compile $ defaultPandocC
      >>= loadAndApplyTemplate "templates/default.html" context
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ postPandocC
      >>= loadAndApplyTemplate "templates/post.html"  context
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" context
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx = field "posts" (\_ -> postList recentFirst)
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html"
          (constField "title" "Archive" <> context)
        >>= relativizeUrls

  match "keybase.txt" $ do
    route $ constRoute ".well-known/keybase.txt"
    compile copyFileCompiler

  match "index.html" $ do
    route idRoute
    compile $ do
      let indexCtx = field "posts" $ \_ -> postList $ liftM (take 3) . recentFirst

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls

navbarCompiler item = do
  -- Get the url of this page
  url <- fmap (maybe empty toUrl) $ getRoute $ itemIdentifier item
  return $ generateNavbar url

generateNavbar url = concat $ do
  (t, l) <- navbar
  -- if the url matches, this tab is active
  let cl = if l == url
        then "class=\"active\""
        else ""
  return $ concat ["<li ", cl, "><a href=\"", l, "\">", t, "</a></li>\n"]

context :: Context String
context = mconcat
  [ mathjaxCtx
  , postImageCtx
  , field "navbar" navbarCompiler
  , dateField "date" "%B %e, %Y"
  , defaultContext
  ]

defaultPandocC = pandocCompilerWith defaultHakyllReaderOptions pandocOptions
postPandocC = pandocCompilerWith defaultHakyllReaderOptions withToc

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFunc = do
  posts    <- loadAll "posts/*"
  filtered <- sortFunc posts
  itemTpl  <- loadBody "templates/post-item.html"
  list     <- applyTemplateList itemTpl context filtered
  return list

pandocOptions = defaultHakyllWriterOptions
  { writerHTMLMathMethod = MathJax ""
  }

withToc = pandocOptions
  { writerTableOfContents = True
  , writerTemplate = Just tocTemplate
  , writerNumberSections  = True
  }

-- Compile template in-line. Why is this so complicated?
tocTemplate =
  either error id $ either (error . show) id $
  runPure $ runWithDefaultPartials $
  compileTemplate "" "\n<div id=\"toc\"><div class=\"tocbox\">Contents:\n$toc$</div></div>\n<div id=\"body\">$body$</div>"

myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Igor Babuschkin"
    , feedDescription = "babushk.in blog"
    , feedAuthorName  = "Igor Babuschkin"
    , feedAuthorEmail = "igor@babushk.in"
    , feedRoot        = "http://babushk.in/"
    }

postImageCtx = field "image" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ case lookupString "image" metadata of
    Just p -> "<img src=" ++ p ++ "></img>"
    Nothing -> ""

mathjaxCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ case lookupString "math" metadata of
    Just "true" -> "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({messageStyle: \"none\",});</script>\
                   \<script type=\"text/javascript\" src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />"
    Just _ -> ""
    Nothing -> ""
