{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections, MultiWayIf, DeriveDataTypeable, RecordWildCards #-}
-- | The Interpreter module is responible for invoking the Hint interpreter to
-- evaluate mutants.
module Test.MuCheck.Interpreter (evaluateMutants, evalMethod, evalMutant, evalTest, summarizeResults, MutantSummary(..)) where

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Data.Either (partitionEithers)
import Data.Typeable
import Language.Haskell.Ghcid
import System.Directory (createDirectoryIfMissing)
import Test.MuCheck.AnalysisSummary
import Test.MuCheck.TestAdapter
import Test.MuCheck.Utils.Common
import Test.MuCheck.Utils.Print
import qualified Language.Haskell.Interpreter as I


-- | Data type to hold results of a single test execution
data MutantSummary = MSumError Mutant String Summary         -- ^ Capture the error if one occured
                   | MSumAlive Mutant Summary                -- ^ The mutant was alive
                   | MSumKilled Mutant Summary               -- ^ The mutant was kileld
                   | MSumOther Mutant Summary                -- ^ Undetermined - we will treat it as killed as it is not a success.
                   deriving (Show, Typeable)

-- | Given the list of tests suites to check, run the test suite on mutants.
evaluateMutants :: (Show b, Summarizable b, TRun a b) =>
     a                                                               -- ^ The module to be evaluated
  -> String                                                          -- ^ The ghcid command
  -> FilePath                                                        -- ^ The test file
  -> String                                                          -- ^ The test command
  -> [Mutant]                                                        -- ^ The mutants to be evaluated
  -> IO (MAnalysisSummary, [MutantSummary])                          -- ^ Returns a tuple of full run summary and individual mutant summary
evaluateMutants m ghcidCmd testFile testCmd mutants = do
  print $ length mutants
  (ghci, _) <- startGhci ghcidCmd Nothing
    (const . const $ pure ())
  results <- mapM (evalMutant ghci testFile testCmd) mutants -- [InterpreterOutput t]
  stopGhci ghci
  let singleTestSummaries = map (summarizeResults m testCmd) $ zip mutants results
      ma  = fullSummary m testCmd results
  return (ma, singleTestSummaries)

-- | The `summarizeResults` function evaluates the results of a test run
-- using the supplied `isSuccess` and `testSummaryFn` functions from the adapters
summarizeResults :: (Summarizable s, TRun a s) =>
     a                                                            -- ^ The module to be evaluated
  -> String                                                       -- ^ Tests we used to run analysis
  -> (Mutant, InterpreterOutput s)                              -- ^ The mutant and its corresponding output of test runs.
  -> MutantSummary                                                -- ^ Returns a summary of the run for the mutant
summarizeResults m testCmd (mutant, ioresults) = case results of -- the last result should indicate status because we dont run if there is error.
  Left err -> MSumError mutant (show err) logS
  Right out -> myresult out
  where results = _io ioresults
        myresult out | isSuccess out = MSumAlive mutant logS
                     | isFailure out = MSumKilled mutant logS
                     | otherwise     = MSumOther mutant logS
        logS :: Summary
        logS = summarize mutant testCmd ioresults
        summarize = summarize_ m

-- | Run all tests on a mutant
evalMutant :: (Typeable t, Summarizable t) =>
     Ghci
  -> FilePath                                                     -- ^ Test file
  -> String                                                      -- ^ Test Command
  -> Mutant                                                       -- ^ Mutant being tested
  -> IO (InterpreterOutput t)                                     -- ^ Returns the result of test runs
evalMutant ghci testFile testCmd Mutant{..} = do
  -- Hint does not provide us a way to evaluate the module without
  -- writing it to disk first, so we go for this hack.
  -- We write the temporary file to disk, run interpreter on it, get
  -- the result (we dont remove the file now, but can be added)
  createDirectoryIfMissing True ".mutants"
  let mutantFile = ".mutants/" ++ hash _mutant ++ ".hs"

  say mutantFile

  writeFile mutantFile _mutant
  let logF = mutantFile ++ ".log"
  _ <- exec ghci (":l " <> testFile <> " " <> mutantFile)
  print "*"
  evalTest ghci logF testCmd

-- | Stop mutant runs at the first sign of problems (invalid mutants or test
-- failure).
stopFast :: (Typeable t, Summarizable t) =>
     (String -> IO (InterpreterOutput t))  -- ^ The function that given a test, runs it, and returns the result
  -> [TestStr]                             -- ^ The tests to be run
  -> IO [InterpreterOutput t]              -- ^ Returns the output of all tests. If there is an error, then it will be at the last test.
stopFast _ [] = return []
stopFast fn (x:xs) = do
  v <- fn x
  case _io v of
    Left r -> do  say (showE r)
                  -- do not append results of the run because mutant was non viable unless it was the last
                  if xs == []
                    then return [v]
                    else stopFast fn xs
    Right out -> if isSuccess out
      then liftM (v :) $ stopFast fn xs
      else return [v] -- test failed (mutant detected)

-- | Show error
showE :: I.InterpreterError -> String
showE (I.UnknownError e) = "Unknown: " ++ e
showE (I.WontCompile e) = "Compile: " ++ show (head e)
showE (I.NotAllowed e) = "Not Allowed: " ++ e
showE (I.GhcException e) = "GhcException: " ++ e

-- | Run one single test on a mutant
evalTest :: forall a. (Typeable a, Summarizable a) =>
    Ghci
 -> String                                 -- ^ The file where we will write the stdout and stderr during the run.
 -> String                                -- ^ The test to be run
 -> IO (InterpreterOutput a)               -- ^ Returns the output of given test run
evalTest ghci logF testCmd = do
  res <- exec ghci testCmd
  putStrLn "********************"
  print res
  let val = Right $ parseResult res
  return Io {_io = val, _ioLog = logF}

-- | Given the filename, modulename, test to evaluate, evaluate, and return result as a pair.
--
-- > t = I.runInterpreter (evalMethod
-- >        "Examples/QuickCheckTest.hs"
-- >        "quickCheckResult idEmp")
evalMethod :: (I.MonadInterpreter m, Typeable t) =>
     String                               -- ^ The mutant _file_ to load
  -> TestStr                              -- ^ The test to be run
  -> m t                                  -- ^ Returns the monadic computation to be run by I.runInterpreter
evalMethod fileName evalStr = do
  I.loadModules [fileName, "src/Test/MuCheck/TestAdapter/AssertCheck"]
  ms <- I.getLoadedModules
  I.setTopLevelModules ms
  I.interpret evalStr (I.as :: (Typeable a => IO a)) >>= liftIO


-- | Summarize the entire run. Passed results are per mutant
fullSummary :: (Show b, Summarizable b, TRun a b) =>
     a                                      -- ^ The module
  -> String                                 -- ^ The list of tests we used
  -> [InterpreterOutput b]                -- ^ The test ouput (per mutant, (per test))
  -> MAnalysisSummary                       -- ^ Returns the full summary of the run
fullSummary m _testCmd results = MAnalysisSummary {
  _maOriginalNumMutants = -1,
  _maCoveredNumMutants = -1,
  _maNumMutants = length results,
  _maAlive = length alive,
  _maKilled = length fails,
  _maErrors= length errors}
  where res = map _io results
        (errors, completed) = partitionEithers res
        fails = filter (failure_ m) completed -- look if others failed or not
        alive = filter (success_ m) completed
