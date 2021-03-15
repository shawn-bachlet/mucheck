module Main where
import System.Environment (getArgs)

import Test.MuCheck (mucheck)
import Test.MuCheck.TestAdapter.AssertCheckAdapter
import Test.MuCheck.TestAdapter
import Test.MuCheck.Utils.Print

main :: IO ()
main = do
  val <- getArgs
  case val of
    ("-h" : _ ) -> help
    ("-tix" : tix : ghcidCmd : file: _ ) -> do
      (msum, _tsum) <- mucheck (toRun file :: AssertCheckRun) ghcidCmd tix
      print msum
    (ghcidCmd : file : _args) -> do
      (msum, _tsum) <- mucheck (toRun file :: AssertCheckRun) ghcidCmd ""
      print msum
    _ -> error "Need function file [args]\n\tUse -h to get help"

help :: IO ()
help = putStrLn $ "mucheck function file [args]\n" ++ showAS ["E.g:",
       " mucheck [-tix <file.tix>] Examples/AssertCheckTest.hs",""]
