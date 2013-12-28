--
-- (C) 2012-2013 Henry de Valence <hdevalence@hdevalence.ca>
-- This file available under the MIT licence.
--
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Prelude         hiding (id)
import           Data.Monoid            (mappend, (<>))
import qualified Text.Pandoc         as Pandoc
import           System.Cmd             (system)
import           System.FilePath        (replaceExtension, takeDirectory)

import Hakyll

-------------------
-- Config variables
-------------------

feedConfig = FeedConfiguration
    { feedTitle = "Henry de Valence :: Blog"
    , feedDescription = "Blog of Henry de Valence"
    , feedAuthorName = "Henry de Valence"
    , feedAuthorEmail = "hdevalence@hdevalence.ca"
    , feedRoot = "http://www.hdevalence.ca"
    }

config = defaultConfiguration
    { deployCommand = "./deploy.sh"
    }

postPattern = "blog/*.md" .||. "blog/*.markdown"

writerOptions = defaultHakyllWriterOptions {
                    Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
                  , Pandoc.writerNumberSections = False
                  , Pandoc.writerTeXLigatures   = True
                  }

myPandoc = pandocCompilerWith defaultHakyllReaderOptions writerOptions

-- JS modules needed by Foundation.
foundationMods :: [Identifier]
foundationMods = ["js/foundation/foundation.topbar.js"]

--------------------------------------
-- Helper functions for building pages
--------------------------------------

postContext :: Tags -> Context String
postContext tags = dateField "date" "%B %e, %Y" 
                <> tagsField "tags" tags
                <> defaultContext

-- Creates a list of posts with given tags, pattern, filter.
postList :: Tags 
         -> Pattern
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern sortFilter = do
    posts        <- sortFilter =<< loadAll pattern
    itemTemplate <- loadBody "templates/postshort.html"
    applyTemplateList itemTemplate (postContext tags) posts

-- Creates a page with a list of posts in it. We use this for the main 
-- blog index, as well as for the "posts tagged X" pages.
makeListPage :: Tags
             -> Pattern
             -> String
             -> Compiler (Item String)
makeListPage tags pattern title = do
    let listContext = field "postlist" (\_ -> postList tags 
                                                       pattern 
                                                       recentFirst)
                   <> constField "title" title
                   <> defaultContext
    makeItem ""
        >>= loadAndApplyTemplate "templates/postlist.html" listContext
        >>= loadAndApplyTemplate "templates/index.html" listContext
        >>= relativizeUrls

-- Create an RSS feed for a list of posts.
makeRssFeed :: Tags
            -> Pattern
            -> Compiler (Item String)
makeRssFeed tags pattern = do
    let feedContext = postContext tags <> bodyField "description"
    loadAllSnapshots pattern "content"
        >>= fmap (take 10) . recentFirst
        >>= renderRss feedConfig feedContext

sassCompiler :: Item String -> Compiler (Item String)
sassCompiler = withItemBody (unixFilter "sass" ["-s", "--trace", "--scss"])

jsCompiler   :: Item String -> Compiler (Item String)
jsCompiler   = withItemBody (unixFilter "jsmin" [])

concatItems :: [Item String] -> Compiler (Item String)
concatItems xs = makeItem $ concatMap itemBody xs

-- Hacky approach copied from Jasper's site
pdflatex :: Item String -> Compiler (Item TmpFile)
pdflatex item = do 
    TmpFile tex <- newTmpFile "pdflatex.tex"
    let dir = takeDirectory tex
        pdf = replaceExtension tex "pdf"
    unsafeCompiler $ do
        writeFile tex $ itemBody item
        _ <- system $ unwords [ "pdflatex"
                              , "-halt-on-error"
                              , "-output-directory"
                              , dir
                              , tex
                              , ">/dev/null"
                              , "2>&1"
                              ]
        return ()
    makeItem $ TmpFile pdf

------------------------
-- Main site description
------------------------

main :: IO ()
main = hakyllWith config $ do
    -- Images
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Stylesheets
    match "css/*.scss" $ do
        route   $ setExtension "css"
        compile $ getResourceString
            >>= sassCompiler
            >>= return . fmap compressCss

    -- Javascript
    match "js/vendor/*" $ do
        route idRoute
        compile $ getResourceString >>= jsCompiler

    -- Foundation Javascript: concatenate and minify
    match "js/foundation/*.js" $ compile getResourceString
    create ["js/foundation.min.js"] $ do
        route idRoute
        compile $ do
            -- We have to load modules after the main js
            core <- load "js/foundation/foundation.js"
            mods <- mapM load foundationMods
            concatItems (core:mods) >>= jsCompiler

    -- Compile templates
    match "templates/*" $ compile templateCompiler

    -- Build tags
    tags <- buildTags postPattern $ fromCapture "blog/tagged/*"
            
    -- Main index
    match "index.markdown" $ do
        route   $ setExtension ".html"
        compile $ do
            let indexContext = constField "title" "Henry de Valence"
                            <> defaultContext
            myPandoc
                >>= loadAndApplyTemplate "templates/index.html"
                                         indexContext
                >>= relativizeUrls

    -- Compile Posts
    match postPattern $ do
        route $ setExtension ""
        compile $ do
            let context = postContext tags
            myPandoc
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" context
                >>= loadAndApplyTemplate "templates/index.html" context
                >>= relativizeUrls

    -- Create blog index
    create ["blog/index.html"] $ do
        route idRoute
        compile $ makeListPage tags postPattern "Henry de Valence :: Blog"

    -- Create tag pages
    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ makeListPage tags pattern ("Posts tagged " ++ tag)

        version "rss" $ do
            route   $ setExtension "xml"
            compile $ makeRssFeed tags pattern

    -- Create RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ makeRssFeed tags postPattern

    match "cv.markdown" $ do
        route $ setExtension "pdf"
        compile $ getResourceBody
            >>= (return . readPandoc)
            >>= (return . fmap (Pandoc.writeLaTeX writerOptions))
            >>= loadAndApplyTemplate "templates/cv.tex" defaultContext
            >>= pdflatex

