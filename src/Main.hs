{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8
import System.Environment
--     http://hackage.haskell.org/package/tagsoup 
import Text.HTML.TagSoup
--     http://hackage.haskell.org/package/megaparsec 
import Text.Megaparsec hiding (satisfy)
--     http://hackage.haskell.org/package/tagsoup-megaparsec 
import Text.Megaparsec.TagSoup

data Course = Course 
            {
              name :: Char8.ByteString
            , videoURLs :: [Char8.ByteString]  
            } deriving (Show)
 
main :: IO ()
main = do 
    target : [] <- getArgs
    r <- get "https://www.cs.uoregon.edu/research/summerschool/summer12/curriculum.html"
    let tags = parseTags $ r ^. responseBody
        parser :: TagParser Char8.ByteString [Course]
        parser = do
            skipMany (satisfy isCourseOpen)
            many course 
            return []
        parseResult = parse parser "" tags
    print tags 
    putStrLn $ "writing to folder " ++ target
    where
    isCourseOpen = \case
        TagOpen "p" [("class","coursetitle")] -> True
        otherwise -> False
    course = do
        return ()
