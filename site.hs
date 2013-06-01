{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Prelude         hiding (id)
import           Data.Monoid            (mappend, (<>))
import qualified Text.Pandoc         as Pandoc

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

    -- Build tags
    tags <- buildTags postPattern $ fromCapture "blog/tagged/*"
            
    -- Main index
    match "index.markdown" $ do
        route   $ setExtension ".html"
        compile $ do
            let indexContext = constField "title" 
                                          "Henry de Valence"
                     `mappend` defaultContext
            myPandoc
                >>= loadAndApplyTemplate "templates/index.html"
                                         indexContext
                >>= relativizeUrls

    -- Compile Posts
    match postPattern $ do
        route $ setExtension ""
        compile $ do
            myPandoc
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html"
                                         (postContext tags)
                >>= loadAndApplyTemplate "templates/index.html"
                                         (postContext tags)
                >>= relativizeUrls

    -- Create blog index
    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            let listContext = (field "postlist" $ \_ -> postList tags postPattern recentFirst)
                    `mappend` constField "title" "Henry de Valence :: Blog"
                    `mappend` defaultContext
            makeItem ""
                >>= applyAsTemplate listContext
                >>= loadAndApplyTemplate "templates/postlist.html"
                                         listContext
                >>= loadAndApplyTemplate "templates/index.html"
                                         listContext
                >>= relativizeUrls

    -- Create tag pages
    -- I've pretty much copied this from Jasper's example, and it should
    -- be rewritten to eliminate duplicating the above.
    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            let tagContext = constField "title" ("Posts tagged " ++ tag)
                          <> constField "postlist" list
                          <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/postlist.html"
                                         tagContext
                >>= loadAndApplyTemplate "templates/index.html"
                                         tagContext
                >>= relativizeUrls

        version "rss" $ do
            route   $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                  >>= fmap (take 10) . recentFirst
                  >>= renderRss feedConfig (feedContext tags)

    -- Create RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots postPattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss feedConfig (feedContext tags)

-------------------------------

postPattern = ("blog/*.md" .||. "blog/*.markdown")

myPandoc = pandocCompilerWith defaultHakyllReaderOptions
                              pandocOptions
           where pandocOptions = defaultHakyllWriterOptions {
                                 Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
                                 }

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

postContext :: Tags -> Context String
postContext tags = dateField "date" "%B %e, %Y" 
         `mappend` tagsField "tags" tags
         `mappend` defaultContext

feedContext :: Tags -> Context String
feedContext tags = (postContext tags) <> bodyField "description"
              
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

