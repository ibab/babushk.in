{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Monoid ((<>), mappend, mconcat)
import qualified Data.Map
import System.Process

import Text.Pandoc
import Hakyll
import Hakyll.Web.Pandoc
import System.IO.Temp
import System.FilePath.Posix
import System.Directory

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

ipythonCompiler :: Compiler (Item String)
ipythonCompiler = do
    fp <- getResourceFilePath
    let base = takeBaseName fp

    let parts = wordsWhen (=='.') base

    if length parts /= 2
        then fail $ "Notebook " ++ fp ++ " doesn't have correct filename format"
        else return ()

    let [date, title] = parts

    contents <- unsafeCompiler $ withSystemTempDirectory "nbconvert" $ \dirname -> do
        copyFile fp (dirname </> base ++ ".ipynb")
        handle <- runProcess "ipython"
                             ["nbconvert", "--to", "html", "--template", "basic", base]
                             (Just dirname)
                             Nothing
                             Nothing
                             Nothing
                             Nothing
        waitForProcess handle
        let newName = dirname </> base ++ ".html"
        readFile newName
    makeItem $ contents

myConfiguration = defaultConfiguration {
  deployCommand = "rsync -avz --delete ./_site/ igor@babushk.in:/srv/http/www"
}

navbar =  [ ("Home", "/index.html")
          , ("About", "/about.html")
          , ("Posts", "/archive.html")
          ]

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

  match "files/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateCompiler

  match "about.md" $ do
    route   $ setExtension "html"
    compile $ myPandocC
      >>= loadAndApplyTemplate "templates/default.html" context
      >>= relativizeUrls

  match "posts/*.ipynb" $ do
    route $ setExtension "html"
    compile $ ipythonCompiler
      >>= loadAndApplyTemplate "templates/post.html" (context `mappend` (field "date" $ \i -> return "2015-07-13"))
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" context
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ myPandocC
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

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = context `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts

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
  , field "navbar" navbarCompiler
  , dateField "date" "%B %e, %Y"
  , defaultContext
  ]

myPandocC = pandocCompilerWith defaultHakyllReaderOptions pandocOptions

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts    <- loadAll "posts/*.md"
  filtered <- sortFilter posts
  itemTpl  <- loadBody "templates/post-item.html"
  list     <- applyTemplateList itemTpl context filtered
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
  
