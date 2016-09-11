{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Network.Wreq
import Data.Functor.Identity
import Data.Char
import Data.Foldable
import Data.Tree
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty
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
             , videoURLs :: [String]  
             } deriving (Show)
 

parser :: MonadDebug m => (String -> String) -> TagParserT Char8.ByteString m [Course]
parser unpackURL = do
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
        videoURLs <- fmap (fmap (unpackURL . Char8.unpack)) $ many video
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
    cleanup :: Char8.ByteString -> Char8.ByteString 
    cleanup = Char8.dropWhile isSpace 
            . head 
            . Char8.lines 
            . Char8.dropWhile isSpace 

type URL = String

type FileName = FilePath

type FolderName = FilePath

type AbsoluteFolderPath = FilePath

type FileToDownload = (URL,FileName)

fileTreeFromCourses :: [Course] -> Tree ([FileToDownload],FolderName)
fileTreeFromCourses courses     = node []
                                       "." 
                                       (fileTreeFromCourse <$> courses)
    where
    fileTreeFromCourse course   = node []
                                       (nospaces (title course)) 
                                       (fileTreeFromLecture <$> lectures course) 
    fileTreeFromLecture lecture = node (fileFromUrl <$> videoURLs lecture)
                                       (nospaces (lectureName lecture)) 
                                       []
    node a b c = Node (a,b) c
    fileFromUrl url = (url,takeFileName url)

nospaces :: Char8.ByteString -> FilePath
nospaces = Char8.unpack . Char8.intercalate "_" . Char8.split ' '

absolutize :: FilePath -> Tree (env,FolderName) -> Tree (env,AbsoluteFolderPath)
absolutize basepath tree = (\xs -> (getEnv xs, getAbsolute xs)) <$> inherit tree 
    where
    getEnv      = Data.List.NonEmpty.head
                . fmap fst
    getAbsolute = joinPath 
                . (basepath :) 
                . Data.List.NonEmpty.toList 
                . Data.List.NonEmpty.reverse
                . fmap snd 
    
createFolderStructure :: Tree (AbsoluteFolderPath) -> IO ()
createFolderStructure tree = for_ folders createDirectory
    where
    folders = concat . Data.Tree.levels $ tree
    
files :: Tree ([a],b) -> [(a,b)]
files tree = do (files,folder) <- toList tree
                file <- files
                [(file,folder)]

traverseThrottled :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b) 
traverseThrottled concLevel action taskContainer = do
    sem <- newQSem concLevel
    let throttledAction = bracket_ (waitQSem sem) (signalQSem sem) . action
    runConcurrently (traverse (Concurrently . throttledAction) taskContainer)

type Notifier = String -> IO ()

createNotifier :: IO Notifier
createNotifier = do
    latch <- newMVar ()
    return $ \msg -> withMVar latch $ \() -> putStrLn msg

download :: Notifier -> ((URL,FileName),AbsoluteFolderPath) -> IO ()
download notify ((url,file),folder) = do
    existsFolder <- doesDirectoryExist folder
    when existsFolder $ do -- do nothing if we deleted the sub-folder
        let path = folder </> file
        exists <- doesFileExist path
        unless exists $ do
            notify $ "starting download of file " ++ path
            notify $ "from url " ++ url
            do (withFile path WriteMode $ \h -> foldGet (consume h) () url) 
               `onException`
                    (do exists <- doesFileExist path
                        when exists (removeFile path))
            notify $ "ended download of file " ++ path
    where
    consume handle () bytes = Bytes.hPut handle bytes  

-- | This function exists in the latest version of "containers"
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Node x ts) = f x (map go ts)

inherit :: Tree a -> Tree (NonEmpty a)
inherit tree = foldTree algebra tree [] where
    algebra :: a -> [[a] -> Tree (NonEmpty a)] -> [a] -> Tree (NonEmpty a)
    algebra a fs as = Node (a:|as) (fs <*> [a:as]) 

data Mode = Show
          | Prepare
          | Download

parseMode :: String -> Mode
parseMode "show"     = Show
parseMode "prepare"  = Prepare
parseMode "download" = Download
parseMode unknown = error $ "unknown mode " ++ unknown

main :: IO ()
main = do 
    (parseMode -> mode) : target : [] <- getArgs
    let baseURL = "https://www.cs.uoregon.edu/research/summerschool/summer12/"
    r <- get $ baseURL ++ "curriculum.html"
    let tags = parseTags $ r ^. responseBody
        (parseResult, _ :: [DebugMessage]) = runWriter (runParserT (parser (baseURL++)) "" tags)
    -- print $ zip [1..] tags
    -- print $ messages
    case parseResult of
        Left  err    -> print err
        Right result -> do
            let fileTree = absolutize target $ fileTreeFromCourses result
            case mode of
                Show     -> putStrLn (drawTree (show <$> fileTree))
                Prepare  -> do
                    createFolderStructure $ fmap snd fileTree 
                    putStrLn $ "writing to folder " ++ target
                    putStrLn $ "delete the sub-folders you are not interested in"
                Download -> do
                    notify <- createNotifier
                    traverseThrottled 2 (download notify) (files fileTree)
                    return ()
