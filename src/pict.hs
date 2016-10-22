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


getItems :: FilePath -> FilePath -> IO [FilePath]
getItems source subPath = do
  items <- foldDirTree onlyJpegs [] source subPath
  return items

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
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

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


--     |> Enum.map(fn {s, subt} -> {
--         (Path.absname(subt, s) |> Path.expand),
--         (Path.absname(subt, "/mnt/P/O/JPEGs/") |> Path.expand)
--                                 } end)
--     |> IO.inspect
--     |> Enum.each(fn {sp, tp} ->
--                     {time, _result} = :timer.tc(
--                           &Pictures.Process.run/3, [sp, tp, cmd_copy()]
--         )
--         :io.format "processing took ~.2f seconds~n", [time/1_000_000.0]
--                   end)


-- #                  @source "/mnt/P/O/P2016/"
-- #                  @subtree "2016" # allow to restrict to sub tree
-- #                  @target "/mnt/P/O/JPEGs/"
-- #    sp = Path.absname(@subtree, @source) |> Path.expand
-- #    tp = Path.absname(@subtree, @target) |> Path.expand
-- #    {time, _result} = :timer.tc(
-- #      &Pictures.Process.run/3, [sp, tp, cmd]
-- #      )
-- #      :io.format "processing took ~.2f seconds~n", [time/1_000_000.0]
--   end
--   def sjpegs do
--     cmd = fn source, target -> [  "convert",
--                                   source,
--                                   "-resize", "2560x2048",
--                                   "-quality" , "50",
--                                   target
--                                 ] end

--     2016..2016
--     |> Enum.map(fn x -> { "/mnt/P/O/P#{x}/", "#{x}/#{x}-09" } end) # only 2016-09
--     |> Enum.map(fn {s, subt} -> { (Path.absname(subt, s) |> Path.expand),
--                                   (Path.absname(subt, "~/Dropbox/Pictures/sJPEGs/") |> Path.expand) }
--                           end)
--                           |> IO.inspect
--     |> Enum.each(fn {sp, tp} ->
--       {time, _result} = :timer.tc(
--         &Pictures.Process.run/3, [sp, tp, cmd]
--         )
--         :io.format "processing took ~.2f seconds~n", [time/1_000_000.0]
--                   end)
--   end

--   def run(source, target, cmd) do
-- #
-- # recursively process any .JPG file in the source tree
-- # using command structure [cmd, args, ...]
-- # e.g. use [convert s -resize 100x100 t] to create
-- #    a smaller JPG in the target tree
-- # -- creating directories as required
-- # -- not overwriting exiting files
-- #
--     [source | dtree(source)]
--     |> Enum.map(fn p -> a=Path.relative_to(p, source);
--                   cond do
--                     a == p -> {source, target}
--                     true -> {Path.absname(a, source), Path.absname(a, target)}
--                   end
--                 end)
--     |> Enum.each(fn {s, t} -> IO.puts "#{s} -> #{t}"
--                   File.mkdir_p(t)
--                   do_process s, t, cmd
--                  end)
--   end

--   @max_parallel 16
--   defp do_process(s, t, cmd) do
--     Path.wildcard(Path.absname("*.{JPG,jpg}",s))
--     |> Enum.map(&Path.basename/1)
--     |> Enum.filter(fn path -> not File.exists?(Path.absname(path,t)) end)
--     |> Enum.map(fn x -> cmd.(Path.absname(x,s),Path.absname(x,t)) end)
--     |> Enum.chunk(@max_parallel, @max_parallel, [])
--     |> Enum.each(fn c -> do_exec(c) end)
--   end
--   defp do_exec(chunk) do
-- # exec cmd in chunks of max_parallel
--     chunk
--     |> Enum.map(fn x -> Task.async(fn -> do_sys(x) end) end)
--     |> Enum.map(fn t -> Task.await(t, 120_000) end)
--   end

--   defp do_sys([cmd | args]) do
-- #    System.cmd(cmd, args, into: IO.stream(:stdio, :line))
--     {time, result} = :timer.tc(
--         System, :cmd, ["nice", [cmd | args]]
--       )
--     {_output, status} = result
--     :io.format "processing #{cmd} #{Path.basename(hd(args))}, status ~2B, took ~.2f sec~n",
--                 [status, time/1_000_000.0]
--     result
--   end

--   defp dtree(dir) do
--     cond do
--       not File.dir?(dir) -> []
--       true -> File.ls!(dir)
--           |> Enum.filter(fn name -> File.dir?("#{dir}/#{name}") end)
--           |> Enum.map(fn name -> ["#{dir}/#{name}" | dtree("#{dir}/#{name}")] end)
--           |> List.flatten()
--     end
--   end
