{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.Monoid ((<>), mappend, mconcat)
import qualified Data.Map
import Hakyll
import Hakyll.Web.Pandoc
import Text.Pandoc

main = hakyll $ do
  match "img/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile copyFileCompiler --compressCssCompiler

  match "js/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "about.md" $ do
    route   $ setExtension "html"
    compile $ myPandocC
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ myPandocC
      >>= loadAndApplyTemplate "templates/post.html"  postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx = field "posts" (\_ -> postList recentFirst)

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html"
          archiveCtx
        >>= loadAndApplyTemplate "templates/default.html"
          (constField "title" "Archive" <> postCtx)
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      let firstThree = \x -> do
            posts <- recentFirst x
            return $ take 3 posts
      let indexCtx = field "posts" $ \_ -> postList firstThree

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx = mconcat
  [ mathjaxCtx
  , dateField "date" "%B %e, %Y"
  , defaultContext
  ]

myPandocC = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts    <- loadAll "posts/*"
  filtered <- sortFilter posts
  itemTpl  <- loadBody "templates/post-item.html"
  list     <- applyTemplateList itemTpl postCtx filtered
  return list

pandocOptions = defaultHakyllWriterOptions
  { writerHTMLMathMethod = MathJax ""
  }

mathjaxCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ case Data.Map.lookup "math" metadata of
    Just "true" -> "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({messageStyle: \"none\",});</script>\
                   \<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />"
    Just _ -> ""
    Nothing -> ""
  
