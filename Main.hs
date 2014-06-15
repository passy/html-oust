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

    putStrLn "== URI ==="
    uri <- runX $ doc >>> getBaseURI
    print uri

    putStrLn "== Styles =="
    links <- runX $ extractStylesheets doc
    print links

    putStrLn "== Scripts =="
    scripts <- runX $ extractScripts doc
    print scripts

    putStrLn "== Imports =="
    imports <- runX $ extractImports doc
    print imports

extractStylesheets ::
    ArrowXml cat =>
    cat a (Data.Tree.NTree.TypeDefs.NTree XNode) ->
    cat a String
extractStylesheets = (selectStylesheets ! "href" <<<)

extractScripts ::
    ArrowXml cat =>
    cat a (Data.Tree.NTree.TypeDefs.NTree XNode) ->
    cat a String
extractScripts = (selectScripts ! "src" <<<)

extractImports ::
    ArrowXml cat =>
    cat a (Data.Tree.NTree.TypeDefs.NTree XNode) ->
    cat a String
extractImports = (selectImports ! "href" <<<)

selectImports :: ArrowXml a => a (NTree XNode) (NTree XNode)
selectImports = css "link[rel='import']"

selectScripts :: ArrowXml a => a (NTree XNode) (NTree XNode)
selectScripts = css "script"

selectStylesheets :: ArrowXml a => a (NTree XNode) (NTree XNode)
selectStylesheets = css "link[rel='stylesheet']"
