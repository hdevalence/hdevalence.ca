{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)
import Data.List.Split (splitEvery)

import Text.Pandoc (HTMLMathMethod(..), WriterOptions(..), defaultWriterOptions)

import Hakyll

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

hakyllConfig :: HakyllConfiguration
hakyllConfig = defaultHakyllConfiguration
    { deployCommand = "s3cmd sync _site/ s3://www.hdevalence.ca/"
    }

main :: IO ()
main = hakyllWith hakyllConfig $ do
    -- CSS and Twitter Bootstrap
    match "css/bootstrap.css" $ route idRoute
    create "css/bootstrap.css" $ constA mempty
        >>> unixFilter "lessc" ["--compress", "bootstrap/less/bootstrap.less"]
    match "css/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Copy images
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ do 
        compile templateCompiler

    -- Blog posts
    match "blog/*" $ do
        route $ setExtension "html"
        compile $ pageCompilerWith defaultHakyllParserState pandocOptions
            >>> applyTemplateCompiler "templates/post.hamlet"
            >>> applyTemplateCompiler "templates/blog.hamlet"
            >>> relativizeUrlsCompiler

    -- Create the blog index page
    match "blog/index.html" $ route idRoute
    create "blog/index.html" $ constA mempty
        >>> arr (setField "title" "Henry de Valence :: Blog")
        >>> requireAllA "blog/*.md" (id *** arr (reverse . chronological) >>> addPostShortList)
        >>> applyTemplateCompiler "templates/postlist.hamlet"
        >>> applyTemplateCompiler "templates/blog.hamlet"
        >>> relativizeUrlsCompiler

    -- Create the main page
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Henry de Valence")
        >>> applyTemplateCompiler "templates/index.hamlet"
        >>> relativizeUrlsCompiler

-- Adds list of posts with just the title.
addPostShortList :: Compiler (Page String, [Page String]) (Page String)
addPostShortList = setFieldA "postlist" $
                require "templates/postshort.hamlet" (\p t -> map (applyTemplate t) p)
                >>> arr mconcat
                >>> arr pageBody
{-
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "postlist" $
                require "templates/post.hamlet" (\p t -> map (applyTemplate t) p)
                >>> arr mconcat
                >>> arr pageBody
-}

