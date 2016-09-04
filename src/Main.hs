{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.Wreq
import Data.Functor.Identity
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
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

class Monad m => MonadDebug m where
    tell' :: [String] -> m () 

instance MonadDebug Identity where
    tell' _ = pure () 

instance MonadDebug (WriterT [String] Identity) where
    tell'  = tell


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
 
parser :: MonadDebug m => TagParserT Char8.ByteString m [Course]
parser = do
    skipMany (satisfy (not . isCourseTitleOpen))
    many course 
    where
    course = do
        satisfy isCourseTitleOpen
        TagText title <- tagText
        tagOpen "a"
        tagText
        tagClose "a"
        tagClose "p" 
        satisfy isCourseDescOpen
        lift $ tell' ["before skipping"]
        skipTillVideoStart
        lift $ tell' ["after"]
        tagOpen "ul"
        lift $ tell' ["after2"]
        lectures <- many lecture
        tagClose "ul"
        tagClose "p" 
        pure (Course {..})
    lecture = do
        lift $ tell' ["after3"]
        tagOpen "li"
        lift $ tell' ["after4"]
        TagText lectureName <- tagText
        videoURLs <- many video
        tagClose "li"
        let result = Lecture {..}
        lift $ tell' ["lecture: " ++ show result]
        pure result
    video = do
        TagOpen _ [("href",href)] <- tagOpen "a" 
        TagText _ <- tagText    
        tagClose "a"
        lift $ tell' ["link: " ++ show href]
        pure href
    isCourseTitleOpen = \case
        TagOpen "p" [("class","coursetitle")] -> True
        otherwise -> False
    isCourseDescOpen = \case
        TagOpen "p" [("class","coursedesc")] -> True
        otherwise -> False
    videoStart = do
        tagOpen "li"
        lift $ tell' ["---1"]
        tagText 
        lift $ tell' ["---2"]
        vlink <- video
        lift $ tell' ["---3" ++ show vlink]
        guard (Char8.isSuffixOf "mp4" vlink)  
        lift $ tell' ["---4"]
    skipTillVideoStart = do
--      manyTill anyTag videoStart     
        skipMany (try (lift (tell' ["---U"]) *> anyTag *> lift (tell' ["---X"]) *> notFollowedBy videoStart *> lift (tell' ["---X"])  )     )

main :: IO ()
main = do 
    target : [] <- getArgs
    r <- get "https://www.cs.uoregon.edu/research/summerschool/summer12/curriculum.html"
    let tags = parseTags $ r ^. responseBody
        (parseResult, messages :: [String]) = runWriter (runParserT parser "" tags)
    -- print $ zip [1..] tags
    -- print $ messages
    print $ parseResult
    putStrLn $ "writing to folder " ++ target
