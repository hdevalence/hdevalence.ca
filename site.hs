{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Data.Monoid (mappend)

import Hakyll

main :: IO ()
main = hakyllWith config $ do
    -- Images
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Stylesheets
    match "css/*.scss" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>=
            withItemBody (unixFilter "sass" ["-s", "--trace", "--scss"]) >>=
            return . fmap compressCss

    -- Compile templates
    match "templates/*" $ compile templateCompiler
            
    -- Main index
    match "index.markdown" $ do
        route   $ setExtension ".html"
        compile $ do
            let indexContext = constField "title" 
                                          "Henry de Valence"
                     `mappend` defaultContext
            pandocCompiler
                >>= loadAndApplyTemplate "templates/index.html"
                                         indexContext
                >>= relativizeUrls

    -- Compile Posts
    match postPattern $ do
        route $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html"
                                         postContext
                >>= loadAndApplyTemplate "templates/index.html"
                                         postContext
                >>= relativizeUrls

    -- Create blog index
    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            let listContext = (field "postlist" $ \_ -> postList recentFirst)
                    `mappend` constField "title"
                                         "Henry de Valence :: Blog"
                    `mappend` defaultContext
            makeItem ""
                >>= applyAsTemplate listContext
                >>= loadAndApplyTemplate "templates/postlist.html"
                                         listContext
                >>= loadAndApplyTemplate "templates/index.html"
                                         listContext
                >>= relativizeUrls

    -- Create RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedContext = postContext `mappend` bodyField "description"
            loadAllSnapshots postPattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss feedConfig feedContext 

-------------------------------

postPattern = ("blog/*.md" .||. "blog/*.markdown")



postList :: ([Item String] -> Compiler [Item String])
         -> Compiler String
postList sortFilter = do
    posts        <- sortFilter =<< loadAll postPattern
    itemTemplate <- loadBody "templates/postshort.html"
    list         <- applyTemplateList itemTemplate 
                                      postContext 
                                      posts
    return list

postContext :: Context String
postContext = dateField "date" "%B %e, %Y" 
    `mappend` defaultContext
              
-------------------------------

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

