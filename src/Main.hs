{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Functor.Identity
import Data.Char
import Data.Maybe
import Data.Foldable
import Data.Tree
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty
import qualified Data.ByteString.Lazy.Char8 as Char8
--     http://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as Bytes
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Lens (view)
import Control.Exception(throwIO,bracket_,onException)
import Control.Concurrent.QSem
import Control.Concurrent.MVar
import Control.Concurrent.Async
import System.IO
import System.Environment
--     http://hackage.haskell.org/package/directory
import System.Directory 
--     http://hackage.haskell.org/package/filepath
import System.FilePath
--     http://hackage.haskell.org/package/tagsoup 
import Text.HTML.TagSoup
--     http://hackage.haskell.org/package/megaparsec 
import Text.Megaparsec hiding (satisfy)
--     http://hackage.haskell.org/package/tagsoup-megaparsec 
import Text.Megaparsec.TagSoup
import Network.Wreq

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
             , videoURLs :: [URL]  
             } deriving (Show)
 
type RelativeURL = String

type URL = String

parser :: MonadDebug m => (RelativeURL -> URL) -> TagParserT Char8.ByteString m [Course]
parser makeAbsolute = do
    skipMany (satisfy (not . isCourseTitleOpen))
    many course 
    where
    course = do
        satisfy isCourseTitleOpen
        TagText (cleanupName -> title) <- tagText
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
        TagText (cleanupName -> lectureName) <- tagText
        videoURLs <- fmap (fmap (makeAbsolute . Char8.unpack)) $ many video
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
    cleanupName :: Char8.ByteString -> Char8.ByteString 
    cleanupName = Char8.dropWhile isSpace 
                . head 
                . Char8.lines 
                . Char8.dropWhile isSpace 

type FileName = FilePath

type FolderPathPiece = FilePath

type FolderPath = FilePath

fileTreeFromCourses :: [Course] -> Forest ([(URL,FileName)],FolderPathPiece)
fileTreeFromCourses courses = fileTreeFromCourse <$> courses
    where
    fileTreeFromCourse course   = node []
                                       (nospaces (title course)) 
                                       (fileTreeFromLecture <$> lectures course) 
    fileTreeFromLecture lecture = node (fileFromUrl <$> videoURLs lecture)
                                       (nospaces (lectureName lecture)) 
                                       []
    node a b c = Node (a,b) c
    fileFromUrl url = (url,takeFileName url)
    nospaces :: Char8.ByteString -> FolderPathPiece
    nospaces = Char8.unpack . Char8.intercalate "_" . Char8.split ' '

-- | Decorate each node with the full path assembled from path pieces starting
-- from the root.
absolute :: (w -> FolderPathPiece) -> Tree w -> Tree (w,FolderPath)
absolute getFolder tree = pathFromPieces <$> inherit tree 
    where
    pathFromPieces  = liftA2 (,) Data.List.NonEmpty.head pathFromPieces'
    pathFromPieces' = joinPath 
                    . Data.List.NonEmpty.toList 
                    . Data.List.NonEmpty.reverse
                    . fmap getFolder
    
createFolderStructure :: Tree (FolderPath) -> IO ()
createFolderStructure tree = for_ folders (createDirectoryIfMissing False)
    where
    folders = concat . Data.Tree.levels $ tree
    
pruneMissingFolders :: forall w. (w -> FolderPath) -> Tree w -> IO (Maybe (Tree w))
pruneMissingFolders extract' tree = foldTree catafunc tree
    where
    catafunc :: w 
             -> [IO (Maybe (Tree w))]
             ->  IO (Maybe (Tree w)) 
    catafunc nodeinfo ts = do
        exists <- doesDirectoryExist (extract' nodeinfo)
        if   exists
        then Just . Node nodeinfo <$> mconcat (fmap (fmap maybeToList) ts)
        else return Nothing

flattenFiles :: Functor f => Tree (([f FileName],whatever),FolderPath) -> [f FilePath]
flattenFiles tree = do ((files,_),folderpath) <- toList tree
                       file <- files
                       [(folderpath </>) <$> file]

-- |Perform an action concurrently across a container, with a limit on the
-- maximum number of simultaneous actions.
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

-- | Downloads a file if it doesn't already exist. If an exception happens, an
-- attempt is made to delete the partially downloaded file.
download :: Notifier -> (URL,FilePath) -> IO ()
download notify (url,filepath) = do
    exists <- doesFileExist filepath
    unless exists $ do
        notify $ "starting download of file " ++ filepath
        notify $ "from url " ++ url
        do (withFile filepath WriteMode $ \h -> foldGet (consume h) () url) 
           `onException`
               (do exists <- doesFileExist filepath
                   when exists (removeFile filepath))
        notify $ "ended download of file " ++ filepath
    where
    consume handle () bytes = Bytes.hPut handle bytes  

-- | A tree catamorphism. This function already exists in the latest version of
-- "containers"
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Node x ts) = f x (map go ts)

-- | Annotate each node with the list of all its ancestors. The root node will
-- be at the end of the list.
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
    let tags = parseTags . view responseBody $ r 
        (parseResult, _ :: [DebugMessage]) = runWriter (runParserT (parser (baseURL++)) "" tags)
    case parseResult of
        Left  err    -> print err
        Right result -> do
            let fileTree = absolute snd $ Node ([],target) (fileTreeFromCourses result)
            case mode of
                Show     -> putStrLn (drawTree (show <$> fileTree))
                Prepare  -> do
                    createFolderStructure $ fmap snd fileTree 
                    putStrLn $ "writing to folder " ++ target
                    putStrLn $ "delete the sub-folders you are not interested in"
                Download -> do
                    files <- foldMap flattenFiles <$> pruneMissingFolders snd fileTree
                    notify <- createNotifier
                    traverseThrottled 2 (download notify) files
                    return ()
