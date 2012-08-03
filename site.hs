{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Data.Monoid (mempty)

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Twitter Bootstrap
    match "css/bootstrap.css" $ route idRoute
    create "css/bootstrap.css" $ constA mempty
        >>> unixFilter "lessc" ["--compress", "bootstrap/less/bootstrap.less"]

    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $ do 
        compile templateCompiler

    match "blog/*" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/post.hamlet"
            >>> applyTemplateCompiler "templates/blog.hamlet"
            >>> relativizeUrlsCompiler




