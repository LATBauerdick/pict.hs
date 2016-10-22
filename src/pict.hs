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
maxItems = 42 -- for predicat function foldDirTree onlyJpegs below

processJpegs :: Int -> Int -> Bool -> IO ()
processJpegs yr mo sJpegs = do
  let subPath = if mo > 0
                  then printf "%d/%d-%02d" (yr::Int) (yr::Int) (mo::Int)
                  else printf "%d" (yr::Int)
      sJpegsPath  = "/Users/bauerdic/Dropbox/Pictures/sJPEGs/"
      jpegsPath = sJpegsPath -- printf "/mnt/P/O/P%d/" (yr::Int)
      source = if sJpegs
                  then sJpegsPath
                  else jpegsPath
      target = "/Users/bauerdic/neu"
  putStrLn $ "copy from " ++ source ++ ">>>" ++ subPath
  if sJpegs
     then doCopy source subPath target
     else doConvert source subPath target

doConvert :: FilePath -> FilePath -> FilePath -> IO ()
doConvert source subPath target = do
  js <- getItems source subPath
  cnt <- foldDirTree countJpegs 0 source subPath
  putStrLn (printf "Processing %d items." cnt)

  createDirectoryIfMissing True target
  mapM_ (convertItem source target) js

convertItem :: FilePath -> FilePath -> FilePath -> IO ()
convertItem baseSourcePath baseTargetPath relativePath = do
  let sourcePath = baseSourcePath </> relativePath
      targetPath = baseTargetPath </> relativePath
      cmd:args = cmd_conv sourcePath  targetPath
      msg = "Converting " ++ (takeFileName relativePath)
        ++ " to " ++ (takeDirectory targetPath)
  createDirectoryIfMissing True $ takeDirectory targetPath
  fex <- doesFileExist targetPath
  if (not fex)
     then do putStr "."
             rawSystem cmd args `debug` msg
             return () -- for the moment, ignore any error !!!????
     else putStr "x" `debug` "target file exist, NOT overwriting"

doCopy :: FilePath -> FilePath -> FilePath -> IO ()
doCopy source subPath target = do
  js <- getItems source subPath
  cnt <- foldDirTree countJpegs 0 source subPath
  putStrLn (printf "Processing %d items." cnt)

  createDirectoryIfMissing True target
  mapM_ (copyItem source target) js

copyItem :: FilePath -> FilePath -> FilePath -> IO ()
copyItem baseSourcePath baseTargetPath relativePath = do
  let sourcePath = baseSourcePath </> relativePath
  let targetPath = baseTargetPath </> relativePath

  let msg = "Copying " ++ (takeFileName relativePath)
        ++ " to " ++ (takeDirectory targetPath)
  createDirectoryIfMissing True $ takeDirectory targetPath
  fex <- doesFileExist targetPath
  if (not fex)
     then do putStr "."
             copyFileWithMetadata sourcePath targetPath `debug` msg
     else putStr "x" `debug` "target file exist, NOT overwriting"


getItems :: FilePath -> FilePath -> IO [FilePath]
getItems source subPath = do
  items <- foldDirTree onlyJpegs [] source subPath
  return items

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
maybeIO act = catch (Just `liftM` act)
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

