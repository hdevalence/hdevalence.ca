{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))

import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ do 
        compile templateCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/post.hamlet"
            >>> applyTemplateCompiler "templates/blog.hamlet"
            >>> relativizeUrlsCompiler


