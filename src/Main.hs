{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Wreq
import Control.Applicative
import Control.Monad
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8
import System.Environment
--     http://hackage.haskell.org/package/tagsoup 
import Text.HTML.TagSoup
--     http://hackage.haskell.org/package/megaparsec 
import Text.Megaparsec hiding (satisfy)
--     http://hackage.haskell.org/package/tagsoup-megaparsec 
import Text.Megaparsec.TagSoup

type TagParserT str m = ParsecT Dec [Tag str] m

data Course = Course 
            {
              title :: Char8.ByteString
            , lectures :: [Lecture]  
            } deriving (Show)

data Lecture = Lecture
             {
               lectureName :: Char8.ByteString
             , videoURLs :: [Char8.ByteString]  
             } deriving (Show)
 
parser :: TagParser Char8.ByteString [Course]
parser = do
    skipMany (satisfy (not . isCourseTitleOpen))
    many course 
    where
    course = do
        satisfy isCourseTitleOpen
        tagText
        tagOpen "a"
        TagText title <- tagText
        tagClose "a"
        tagClose "p" 
        satisfy isCourseDescOpen
        skipTillVideoStart
        tagOpen "ul"
        lectures <- many lecture
        tagClose "ul"
        tagClose "p" 
        pure (Course {..})
    lecture = do
        tagOpen "li"
        TagText lectureName <- tagText
        videoURLs <- many video
        tagClose "li"
        pure (Lecture {..})
    video = do
        TagOpen _ [("href",href)] <- tagOpen "a" 
        TagText link <- tagText    
        tagClose "a"
        pure link
    isCourseTitleOpen = \case
        TagOpen "p" [("class","coursetitle")] -> True
        otherwise -> False
    isCourseDescOpen = \case
        TagOpen "p" [("class","coursedesc")] -> True
        otherwise -> False
    videoStart = do
        tagOpen "li"
        tagText 
        vlink <- video
        guard (Char8.isSuffixOf "mp4" vlink)  
    skipTillVideoStart = do
        skipMany (anyTag *> notFollowedBy videoStart)     

main :: IO ()
main = do 
    target : [] <- getArgs
    r <- get "https://www.cs.uoregon.edu/research/summerschool/summer12/curriculum.html"
    let tags = parseTags $ r ^. responseBody
        parseResult = parse parser "" tags
    print $ zip [1..] tags
    print parseResult 
    putStrLn $ "writing to folder " ++ target
