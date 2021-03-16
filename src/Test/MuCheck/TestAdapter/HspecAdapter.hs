{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TypeSynonymInstances #-}

-- | Module for using Hspec tests
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Test.MuCheck.TestAdapter.HspecAdapter where

import Data.Char (isDigit)
import qualified Test.Hspec.Core.Runner as Hspec
import Test.MuCheck.TestAdapter
import Data.Typeable
import Data.List (isInfixOf)

deriving instance Typeable Hspec.Summary
type HspecSummary = Hspec.Summary

-- | Summarizable instance of `Hspec.Summary`
instance Summarizable HspecSummary where
  testSummary _mutant _test result = Summary $ _ioLog result
  isSuccess (Hspec.Summary { Hspec.summaryExamples = _, Hspec.summaryFailures = sf } ) = sf == 0
  parseResult xs =
    let
      s = head $ filter (\l -> "examples" `isInfixOf` l && "failures" `isInfixOf` l) xs
      (ex, r) = span isDigit s
      fs = takeWhile isDigit $ dropWhile (not . isDigit) r
    in
      Hspec.Summary (read @Int ex) (read @Int fs)

data HspecRun = HspecRun String

instance TRun HspecRun HspecSummary where
  genTest _m tstfn = "assertCheckResult " ++ tstfn
  getName (HspecRun str) = str
  toRun s = HspecRun s

  summarize_ _m = testSummary :: Mutant -> TestStr -> InterpreterOutput HspecSummary -> Summary
  success_ _m = isSuccess :: HspecSummary -> Bool
  failure_ _m = isFailure :: HspecSummary -> Bool
  other_ _m = isOther :: HspecSummary -> Bool