--
-- (C) 2012-2014 Henry de Valence <hdevalence@hdevalence.ca>
-- This file available under the MIT licence.
--
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Prelude         hiding (id)
import           Data.Monoid            (mappend, (<>))
import           Data.Char              (toLower)
import           Data.List              (intercalate)
import           Data.List.Split        (split, splitOn, oneOf)
import           Text.Regex.Posix       ((=~))
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

config = defaultConfiguration { deployCommand = "s3_website push" }

postPattern = "blog/*.md" .||. "blog/*.markdown"
seminarPattern = "gradseminar/*.markdown"

writerOptions = defaultHakyllWriterOptions {
                    Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
                  , Pandoc.writerNumberSections = False
                  , Pandoc.writerTeXLigatures   = True
                  }

myPandoc = pandocCompilerWith defaultHakyllReaderOptions writerOptions

-- JS modules needed by Foundation.
foundationMods :: [Identifier]
foundationMods = ["js/foundation/foundation.topbar.js"]

------------------------
-- Main site description
------------------------

main :: IO ()
main = hakyllWith config $ do
    -- Images
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Other files
    match (fromList ["robots.txt", "favicon.ico"]) $ do
        route idRoute
        compile copyFileCompiler

    -- Stylesheets: concatentate, sass, minify.
    match "css/*.scss" $ compile getResourceString
    create ["css/style.css"] $ do
        route idRoute
        compile $ loadAll "css/*.scss"
            >>= concatItems
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

    match "seminar.markdown" $ do
        route   $ constRoute "gradseminar/index.html"
        compile $ do
            let context = field "talklist" (\_ -> seminarList)
                       <> constField "title" "Graduate Math Seminar"
                       <> defaultContext
            myPandoc
                >>= loadAndApplyTemplate "templates/gradseminar.html" context
                >>= loadAndApplyTemplate "templates/index.html" context
                >>= relativizeUrls

    match seminarPattern $ do
        compile $ myPandoc

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
            >>= return . readPandoc
            >>= return . fmap (Pandoc.writeLaTeX writerOptions)
            >>= return . fmap smallcaps
            >>= return . fmap (replace "LaTeX" "\\LaTeX")
            >>= loadAndApplyTemplate "templates/cv.tex" defaultContext
            >>= pdflatex

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

seminarList :: Compiler String
seminarList = do
    let talkContext = dateField "date" "%B %e, %Y"
                   <> defaultContext
    talks <- chronological =<< loadAll seminarPattern
    talkTemplate <- loadBody "templates/seminartalk.html"
    applyTemplateList talkTemplate talkContext talks

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
--jsCompiler   = withItemBody (unixFilter "jsmin" [])
jsCompiler   = withItemBody return

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
                              , ">/tmp/texlog"
                              , "2>&1"
                              ]
        return ()
    makeItem $ TmpFile pdf

-- Hacky smallcapsification for LaTeX.
-- Converts all words longer than two capital letters to \textsc
smallcaps :: String -> String
smallcaps = foldr1 (++) .
            map (\word -> if word =~ ("^[A-Z][A-Z]+$" :: String)
                          then "\\textsc{" ++ map toLower word
                                           ++ "}"
                          else word) .
            split (oneOf " \n,.()")

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

