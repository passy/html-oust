{-# Language NoImplicitPrelude #-}

module Main where

import Prelude
-- TODO: Qualify imports when exploration phase is over. :)
import System.Environment
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Tree.NTree.TypeDefs


data ExternalResource = ExternalResource {
    _stylesheets :: [String],
    _imports :: [String],
    _scripts :: [String]
}

main :: IO ()
main = do
    args <- getArgs
    html <- readFile $ head args
    let doc = readString [withParseHTML yes, withWarnings no] html

    putStrLn "== Styles =="
    -- TODO: This should look like doc >>> stylesheets >>> getAttrValue ...
    links <- runX $ extractStylesheets doc
    print links

    putStrLn "== Scripts =="
    scripts <- runX $ extractScripts doc
    print scripts

    putStrLn "== Imports =="
    imports <- runX $ extractImports doc
    print imports

-- TODO:
-- All of these extractors should just be arrows. Should be fun refactoring
-- this.

extractStylesheets ::
    ArrowXml cat =>
    cat a (Data.Tree.NTree.TypeDefs.NTree XNode) ->
    cat a String
extractStylesheets doc =
    doc >>> css "link" >>> hasAttrValue "rel" (== "stylesheet") >>> getAttrValue "href"

extractScripts ::
    ArrowXml cat =>
    cat a (Data.Tree.NTree.TypeDefs.NTree XNode) ->
    cat a String
extractScripts doc =
    doc >>> css "script" >>> getAttrValue "src"

extractImports ::
    ArrowXml cat =>
    cat a (Data.Tree.NTree.TypeDefs.NTree XNode) ->
    cat a String
extractImports doc =
    doc >>> css "link" >>> hasAttrValue "rel" (== "import") >>> getAttrValue "href"
