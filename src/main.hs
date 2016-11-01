module Main (main) where

import System.Environment
import System.Exit
import Pict ( processJpegs )

main :: IO ()
main = getArgs >>= parse >>= process True

parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse [] = return ["-c"] :: IO [String] -- default
parse x = return x :: IO [String]

usage = putStrLn "Usage: pict [-p(rocess) | -c(opy)] [year] [month]"
version = putStrLn "Haskell pict v. 0.1"
exit = exitWith ExitSuccess

process :: Bool -> [String] -> IO ()
process conv args =
  case args of
    [] -> do
      Pict.processJpegs 2016 10 conv
    "-c":rest -> do
      putStrLn "will copy jpegs"
      process False rest
    "-p":rest -> do
      putStrLn "will process / convert to small jpegs"
      process True rest
    [syr] -> do
      let yr = read syr
      Pict.processJpegs yr 0 conv
    [syr, smo] -> do
      let yr = read syr
      let mo = read smo
      Pict.processJpegs yr mo conv

