{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Prelude         hiding (id)
import           Data.Monoid            (mappend, (<>))
import qualified Text.Pandoc         as Pandoc

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

postPattern = ("blog/*.md" .||. "blog/*.markdown")

myPandoc = pandocCompilerWith defaultHakyllReaderOptions
                              pandocOptions
           where pandocOptions = defaultHakyllWriterOptions {
                     Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
                     }
 
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
    list         <- applyTemplateList itemTemplate 
                                      (postContext tags)
                                      posts
    return list

-- Creates a page with a list of posts in it. We use this for the main 
-- blog index, as well as for the "posts tagged X" pages.
makeListPage :: Tags
             -> Pattern
             -> String
             -> Compiler (Item String)
makeListPage tags pattern title = do
    let listContext = (field "postlist" $ \_ -> postList tags 
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
    let feedContext = (postContext tags)
                   <> bodyField "description"
    loadAllSnapshots pattern "content"
        >>= fmap (take 10) . recentFirst
        >>= renderRss feedConfig feedContext

jsCompiler :: Item String -> Compiler (Item String)
jsCompiler = withItemBody (unixFilter "jsmin" [])

concatItems :: [Item String] -> Compiler (Item String)
concatItems xs = makeItem $ concatMap itemBody xs

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
            >>= withItemBody (unixFilter "sass" ["-s", "--trace", "--scss"]) 
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
            foundation <- load "js/foundation/foundation.js"
            modules <- loadAll "js/foundation/foundation.*.js"
            concatItems (foundation:modules) >>= jsCompiler

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

