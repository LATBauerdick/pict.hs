module Main (main) where

import System.Environment
import System.Exit
import Pict ( processJpegs )

main :: IO ()
main = getArgs >>= parse >>= process

parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse [] = return ["-s", "2016", "9"] :: IO [String] -- default
parse x = return x :: IO [String]

usage = putStrLn "Usage: pict <whatever>"
version = putStrLn "Haskell pict v. 0.1"
exit = exitWith ExitSuccess

process :: [String] -> IO ()
process args =
  case args of
    "-s":rest -> do
      putStrLn "will copy from small jpegs for range"
      print rest
      Pict.processJpegs 2016 0 True
    "-c":rest -> do
      putStrLn "will convert to small jpegs"
      print rest
      Pict.processJpegs 2016 10 False
    [syr] -> do
      print syr
      let yr = 2016
      Pict.processJpegs yr 0 True
    [syr, smo] -> do
      print syr
      print smo
      let yr = 2016
      let mo = 9
      Pict.processJpegs yr mo True

