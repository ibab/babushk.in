{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Monoid ((<>), mappend, mconcat)
import qualified Data.Map

import Text.Pandoc
import Hakyll
import Hakyll.Web.Pandoc

myConfiguration = defaultConfiguration {
  deployCommand = "rsync -avz --delete ./_site/ igor@babushk.in:/srv/http/www"
}

main = hakyllWith myConfiguration $ do
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
      >>= saveSnapshot "content"
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
      let indexCtx = field "posts" $ \_ -> postList $ liftM (take 3) . recentFirst

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts

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

myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Igor Babuschkin"
    , feedDescription = "babushk.in blog"
    , feedAuthorName  = "Igor Babuschkin"
    , feedAuthorEmail = "igor@babushk.in"
    , feedRoot        = "http://babushk.in/"
    }

mathjaxCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ case Data.Map.lookup "math" metadata of
    Just "true" -> "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({messageStyle: \"none\",});</script>\
                   \<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" />"
    Just _ -> ""
    Nothing -> ""
  
