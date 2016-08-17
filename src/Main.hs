{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8
import System.Environment
import Text.HTML.TagSoup
import Text.Megaparsec
import Text.Megaparsec.TagSoup

main :: IO ()
main = do 
    target : [] <- getArgs
    r <- get "https://www.cs.uoregon.edu/research/summerschool/summer12/curriculum.html"
    let tags = parseTags $ r ^. responseBody
    --Char8.putStrLn $ r ^. responseBody
    print tags 
    putStrLn $ "writing to folder " ++ target

