{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat)
import Control.Monad (forM, forM_)
import Data.List (isPrefixOf)
import Hakyll

main :: IO ()
main = hakyll $ do  
    
  forM_ [ "favicon.ico", "img/*" ] $ \p -> match p copy
      
  match "css/*" $ do
    route $ setExtension "css"
    compile $ byExtension (error "Not a (S)CSS file")
      [ (".css",  compressCssCompiler)
      , (".scss", sassCompiler)
      ]

  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pageCompiler
      >>> descCompiler
      >>> arr (setField "tagcloud" "")
      >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
      >>> renderTagsField "prettytags" (fromCapture "tags/*")
      >>> applyTemplateCompiler "templates/post.html"
      >>> defaultHtmlCompiler

--refactor these into 1 function
  match "posts.html" $ route idRoute
  create "posts.html" $ constA mempty
    >>> arr (setField "title" "All posts")
    >>> requireAllA "posts/*" addPostList
    >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
    >>> applyTemplateCompiler "templates/posts.html"
    >>> defaultHtmlCompiler

  match "index.html" $ route idRoute
  create "index.html" ( constA mempty
    >>> arr (setField "title" "Home")
    >>> requireA "tags" (setFieldA "tagcloud" $ renderTagCloud')
    >>> requireAllA "posts/*" (id *** arr (take 3 . recentFirst) >>> addPostList)
    >>> applyTemplateCompiler "templates/index.html"
    >>> defaultHtmlCompiler
        )
  create "tags" $ requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

  match "tags/*" $ route $ setExtension ".html"
  metaCompile $ require_ "tags"
    >>> arr tagsMap
    >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

  match "templates/*" $ compile templateCompiler
  where
    copy = route idRoute >> compile copyFileCompiler
    
    renderTagCloud' = renderTagCloud tagIdentifier 100 250

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"
    
    -- summaryCompiler :: String -> Int -> Compiler (Page String) (Page String)
    -- summaryCompiler s c =  arr (setField "title" $ if s == "index" then "Home" else s)
    -- >>> requireA "tags" (setFieldA "tagcloud" $ renderTagCloud')
    -- >>> requireAllA "posts/*" (id *** arr (take c . recentFirst) >>> addPostList)
    -- >>> applyTemplateCompiler "templates/" ++ s ++ ".html"
    -- >>> defaultHtmlCompiler


addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
              arr recentFirst
              >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
              >>> arr mconcat
              >>> arr pageBody

makeTagList :: String -> [Page String] -> Compiler () (Page String)
makeTagList tag posts = constA (mempty, posts)
                        >>> addPostList
                        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
                        >>> applyTemplateCompiler "templates/posts.html"
                        >>> defaultHtmlCompiler

defaultHtmlCompiler :: Compiler (Page String) (Page String)
defaultHtmlCompiler = applyTemplateCompiler "templates/default.html" >>> relativizeUrlsCompiler
  
descCompiler :: Compiler (Page String) (Page String)
descCompiler =  arr $ copyBodyToField "description"
                >>> arr changeField "description" go 
  where go i = take (min 1000 (simpleSearch "<a name=\"more\"/>" i 0)) i
        simpleSearch _ [] c       = c
        simpleSearch n h@(_:h') c = if (isPrefixOf n h) then c else simpleSearch n h' c+1 

sassCompiler :: Compiler Resource String
sassCompiler = getResourceString 
               >>> unixFilter "sass" ["-s", "--scss"]
               >>> arr compressCss