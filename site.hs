{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)
import Data.List.Split (splitEvery)

import Hakyll

main :: IO ()
main = hakyll $ do
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
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/post.hamlet"
            >>> applyTemplateCompiler "templates/blog.hamlet"
            >>> relativizeUrlsCompiler

    -- Create the blog index page
    match "blog/index.html" $ route idRoute
    create "blog/index.html" $ constA mempty
        >>> arr (setField "title" "Henry de Valence :: Blog")
        >>> requireAllA "blog/*.md" (id *** arr (reverse . sortByBaseName) >>> addPostList)
        >>> applyTemplateCompiler "templates/postlist.hamlet"
        >>> applyTemplateCompiler "templates/blog.hamlet"
        >>> relativizeUrlsCompiler

    -- Create the main page
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Henry de Valence")
        >>> applyTemplateCompiler "templates/index.hamlet"
        >>> relativizeUrlsCompiler

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "list" $
                    require "templates/post.hamlet" (\p t -> map (applyTemplate t) p)
                >>> arr mconcat
                >>> arr pageBody

