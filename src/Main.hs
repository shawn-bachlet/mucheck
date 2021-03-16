{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
import System.Environment (getArgs)

import Test.MuCheck (mucheck)
import Test.MuCheck.TestAdapter.AssertCheckAdapter
import Test.MuCheck.TestAdapter
import Test.MuCheck.Utils.Print
import Test.MuCheck.Options (MuOptions(..), optionsFromFile)
import Test.MuCheck.TestAdapter.HspecAdapter (HspecRun)

main :: IO ()
main = do
  val <- getArgs
  case val of
    ("-h" : _ ) -> help
    (options : _) -> do
      mOptions <- optionsFromFile options
      case mOptions of
        Nothing -> putStrLn "Could not parse options file"
        Just opts -> do
          (msum, _) <-
            case testRunner opts of
              AssertCheck -> mucheck @AssertCheckRun opts
              Hspec -> mucheck @HspecRun opts
          print msum
    _ -> error "Need function file [args]\n\tUse -h to get help"

help :: IO ()
help = putStrLn $ "mucheck function file [args]\n" ++ showAS ["E.g:",
       " mucheck [-tix <file.tix>] Examples/AssertCheckTest.hs",""]