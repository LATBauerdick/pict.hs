module Pict (processJpegs) where


import Text.Printf
import System.Directory (Permissions(..), getPermissions
                        , doesDirectoryExist, getDirectoryContents
                        , doesFileExist
                        , createDirectoryIfMissing, copyFileWithMetadata
                        )
import System.FilePath.Posix (takeExtension, takeFileName, takeDirectory)
import Data.Char (toLower)
import Data.Time (UTCTime)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import System.Process (rawSystem)
import Control.Monad
import Control.Parallel.Strategies
-- import Control.Concurrent (forkIO)
import Data.Maybe (isJust)
import Debug.Trace ( trace )
debug = flip trace

cmd_echo :: String -> String -> [String]
cmd_echo source target = [ "echo", "copy", source, target]
cmd_conv :: String -> String -> [String]
cmd_conv source target = [ "convert", source, "-resize", "2560x2048",
                                    "-quality" , "50", target]
paths :: [(String, String)]
paths = map (\ x -> ( printf "/mnt/P/O/P%d/" (x::Int),
                        printf "%d/%d-10" (x::Int) (x::Int) )) [2010..2016]

-- change this to process more files !!!!!!!!!!
maxItems = 99999 -- for predicat function foldDirTree onlyJpegs below

processJpegs :: Int -> Int -> Bool -> IO ()
processJpegs yr mo sJpegs = do
  let subPath = if mo > 0
                  then printf "%d/%d-%02d" (yr::Int) (yr::Int) (mo::Int)
                  else printf "%d" (yr::Int)
      sJpegsPath  = "/Users/bauerdic/Dropbox/Pictures/sJPEGs/"
      jpegsPath   = "/Users/bauerdic/Media/JPEGs" -- printf "/mnt/P/O/P%d/" (yr::Int)
      source = if sJpegs
                  then sJpegsPath
                  else sJpegsPath -- jpegsPath
      target = "/Users/bauerdic/neu"
      actOn :: FilePath -> IO (Maybe FilePath)
      actOn = if sJpegs
                then doItem source target copyIt
                else doItem source target convertIt
  putStrLn $ "copy from " ++ source ++ ">>>" ++ subPath
  js <- getItems source subPath
  putStrLn (printf "Processing %d items." (length js))
  createDirectoryIfMissing True target
  copied <- doAction actOn js
  putStr "processed items:"
  print $ length $ filter isJust copied

getItems :: FilePath -> FilePath -> IO [FilePath]
getItems source subPath = do
  items <- foldDirTree onlyJpegs [] source subPath
  return items

convertIt :: FilePath -> FilePath -> IO ()
convertIt sourcePath targetPath =  do
       let cmd:args = cmd_conv sourcePath  targetPath
       rawSystem cmd args
       return () -- for the moment, ignore any error !!!????

copyIt :: FilePath -> FilePath -> IO ()
copyIt sourcePath targetPath = copyFileWithMetadata sourcePath targetPath

--doAction :: (FilePath -> IO (Maybe FilePath)) -> [FilePath] -> IO [Maybe FilePath]
doAction :: (a -> IO b) -> [a] -> IO [b]
doAction actOn js = do
  -- let iamfs :: IO [Maybe FilePath]
  --     iamfs = (ioMap actOn js)
  --    -- len = m >>= \s -> return (length s)
  -- amfs <- iamfs
  let
--      aimfs :: [IO (Maybe FilePath)]
      aimfs = map actOn js `using` parList rseq
--      iamfs :: IO [Maybe FilePath]
      iamfs = insideoutIO aimfs
  amfs <- iamfs
  return amfs
insideoutIO :: [IO a] -> IO [a]
insideoutIO [] = do return []
insideoutIO (x:xs) = do
  x' <- x
  xs' <- insideoutIO xs
  return (x':xs')


ioMap :: (FilePath -> IO (Maybe FilePath)) -> [FilePath] -> IO [Maybe FilePath]
ioMap _ [] = return []
ioMap f (a:aas) = do
  b <- f a
  bs <- ioMap f aas
  return (b:bs)

doItem :: FilePath -> FilePath -> (FilePath -> FilePath -> IO()) -> FilePath -> IO (Maybe FilePath)
doItem baseSourcePath baseTargetPath doIt relativePath = do
  let s = baseSourcePath </> relativePath
      t = baseTargetPath </> relativePath
      msg = "Processing " ++ (takeFileName relativePath)
        ++ " to " ++ (takeDirectory t)
  createDirectoryIfMissing True $ takeDirectory t
  mf <- liftM (\x -> if (x) then Nothing else (Just t)) (doesFileExist t)
  if (isJust mf)
    then do putStr "."
            doIt s t `debug` msg --
    else putStr "x" -- `debug` "target file exist, NOT overwriting"
  return mf

onlyJpegs :: Iterator [FilePath]
onlyJpegs paths info
    | length paths == Pict.maxItems
      = Done paths
--    | isDirectory info && takeFileName path == "2016"
--      = Skip paths
    | extension `elem` [".jpg", ".jpeg"]
      = Continue (path : paths)
    | otherwise
      = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info

countJpegs :: Iterator Int
countJpegs count info =
    Continue (if (extension `elem` [".jpg", ".jpeg"])
              then count + 1
              else count)
    where extension = map toLower (takeExtension (infoPath info))

countDirectories :: Iterator Int
countDirectories count info =
    Continue (if isDirectory info
              then count + 1
              else count)

data Info = Info {
      infoPath      :: FilePath
    , infoPerms     :: Maybe Permissions
    , infoSize      :: Maybe Integer
    , infoModTime   :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldDirTree :: Iterator a -> a -> FilePath -> FilePath -> IO a
foldDirTree iter initSeed head path = do
    endSeed <- fold initSeed head path
    return (unwrap endSeed)
  where
    fold seed head path = getUsefulContents head path >>= walk head path seed

    walk head path seed (name:names) = do
      let path' = path </> name
      info <- getInfo head (path </> name)
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk head path seed' names
        Continue seed'
          | isDirectory info -> do
--              print ("path' -->" ++ path')
              next <- fold seed' head path'
              case next of
                done@(Done _) -> return done
                seed''        -> walk head path (unwrap seed'') names
          | otherwise -> walk head path seed' names
    walk _ _ seed _ = return (Continue seed)

maybeIO :: IO a -> IO (Maybe a)
maybeIO actOn = catch (Just `liftM` actOn)
                      (\e -> do
                        let err = e :: SomeException
                        return Nothing)

getInfo :: FilePath -> FilePath -> IO Info
getInfo path name = do
  let path' = path </> name
  perms <- maybeIO (getPermissions path')
--  size <- maybeIO (bracket (openFile path' ReadMode) hClose hFileSize)
--  modified <- maybeIO (getModificationTime path')
  return (Info name perms Nothing Nothing)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

getUsefulContents :: FilePath -> FilePath -> IO [String]
getUsefulContents hd p = do
  let path = hd </> p
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

