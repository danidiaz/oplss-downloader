{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Network.Wreq
import Data.Functor.Identity
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Lens
import Control.Exception(throwIO,bracket_,onException)
import qualified Data.ByteString.Lazy.Char8 as Char8
--     http://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as Bytes
import System.Environment
--     http://hackage.haskell.org/package/tagsoup 
import Text.HTML.TagSoup
--     http://hackage.haskell.org/package/megaparsec 
import Text.Megaparsec hiding (satisfy)
--     http://hackage.haskell.org/package/tagsoup-megaparsec 
import Text.Megaparsec.TagSoup
--     http://hackage.haskell.org/package/directory
import System.Directory 
--     http://hackage.haskell.org/package/filepath
import System.FilePath
import Control.Concurrent.QSem
import Control.Concurrent.MVar
import Control.Concurrent.Async
import System.IO

type TagParserT str m = ParsecT Dec [Tag str] m

data DebugMessage = Link Char8.ByteString
                  | Lecture' Lecture
                  deriving Show

class Monad m => MonadDebug m where
    debug :: DebugMessage -> m () 

instance MonadDebug Identity where
    debug _ = pure () 

instance MonadDebug (WriterT [DebugMessage] Identity) where
    debug msg = tell [msg]

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
 
cleanup :: Char8.ByteString -> Char8.ByteString 
cleanup = Char8.dropWhile isSpace 
        . head 
        . Char8.lines 
        . Char8.dropWhile isSpace 

nospaces :: Char8.ByteString -> FilePath
nospaces = Char8.unpack . Char8.intercalate "_" . Char8.split ' '

parser :: MonadDebug m => TagParserT Char8.ByteString m [Course]
parser = do
    skipMany (satisfy (not . isCourseTitleOpen))
    many course 
    where
    course = do
        satisfy isCourseTitleOpen
        TagText (cleanup -> title) <- tagText
        tagOpen "a"
        tagText
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
        TagText (cleanup -> lectureName) <- tagText
        videoURLs <- many video
        tagClose "li"
        let result = Lecture {..}
        lift $ debug (Lecture' result)
        pure result
    video = do
        TagOpen _ [("href",href)] <- tagOpen "a" 
        TagText _ <- tagText    
        tagClose "a"
        lift $ debug (Link href)
        pure href
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
    skipTillVideoStart = skipMany (try (anyTag *> notFollowedBy videoStart))

type RelativeURL = Char8.ByteString

prepareCourseTarget :: FilePath -> Course -> IO [(FilePath,RelativeURL)]
prepareCourseTarget basepath course = do
    let coursePath = basepath </> nospaces (title course) 
    exists <- doesDirectoryExist coursePath
    unless exists $ createDirectory coursePath
    foldMap (prepareLectureTarget coursePath) (lectures course)

prepareLectureTarget :: FilePath -> Lecture -> IO [(FilePath,RelativeURL)]
prepareLectureTarget basepath lect = do
    let lecturePath = basepath </> nospaces (lectureName lect) 
    exists <- doesDirectoryExist lecturePath
    unless exists $ createDirectory lecturePath
    return $ map (\relurl -> (fullpath lecturePath relurl,relurl)) (videoURLs lect)
    where 
    fullpath folder relurl = folder </> takeFileName (Char8.unpack relurl) 

traverseThrottled :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b) 
traverseThrottled concLevel action taskContainer = do
    sem <- newQSem concLevel
    let throttledAction = bracket_ (waitQSem sem) (signalQSem sem) . action
    runConcurrently (traverse (Concurrently . throttledAction) taskContainer)

type Notifier = String -> IO ()

type BaseURL = String

createNotifier :: IO Notifier
createNotifier = do
    latch <- newMVar ()
    return $ \msg -> withMVar latch $ \() -> putStrLn msg

download :: Notifier -> BaseURL -> (FilePath,RelativeURL) -> IO ()
download notify base (file,relurl) = do
    let absurl = base ++ Char8.unpack relurl
    exists <- doesFileExist file
    unless exists $ do
        notify $ "starting download of file " ++ file
        notify $ "from url " ++ absurl
        do (withFile file WriteMode $ \h -> foldGet (consume h) () absurl) 
           `onException`
                (do exists <- doesFileExist file
                    when exists (removeFile file))
        notify $ "ended download of file " ++ file
    where
    consume handle () bytes = Bytes.hPut handle bytes  

main :: IO ()
main = do 
    target : [] <- getArgs
    let baseURL = "https://www.cs.uoregon.edu/research/summerschool/summer12/"
    r <- get $ baseURL ++ "curriculum.html"
    let tags = parseTags $ r ^. responseBody
        (parseResult, _ :: [DebugMessage]) = runWriter (runParserT parser "" tags)
    -- print $ zip [1..] tags
    -- print $ messages
    case parseResult of
        Left  err    -> print err
        Right result -> do
            targetExists <- doesDirectoryExist target
            if not targetExists
            then throwIO (userError "target folder doesn't exist") 
            else do rs <- foldMap (prepareCourseTarget target) result 
                    putStrLn $ "writing to folder " ++ target
                    notify <- createNotifier
                    traverseThrottled 2 (download notify baseURL) rs
                    return ()
