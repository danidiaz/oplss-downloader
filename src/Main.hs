{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8
import System.Environment
--     http://hackage.haskell.org/package/tagsoup 
import Text.HTML.TagSoup(parseTags) 
--     http://hackage.haskell.org/package/megaparsec 
import Text.Megaparsec
--     http://hackage.haskell.org/package/tagsoup-megaparsec 
import Text.Megaparsec.TagSoup

main :: IO ()
main = do 
    target : [] <- getArgs
    r <- get "https://www.cs.uoregon.edu/research/summerschool/summer12/curriculum.html"
    let tags = parseTags $ r ^. responseBody
    --Char8.putStrLn $ r ^. responseBody
    print tags 
    putStrLn $ "writing to folder " ++ target

