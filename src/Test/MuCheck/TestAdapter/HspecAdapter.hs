{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TypeSynonymInstances #-}

-- | Module for using Hspec tests
module Test.MuCheck.TestAdapter.HspecAdapter where

import Data.Char (isDigit)
import qualified Test.Hspec.Core.Runner as Hspec
import Test.MuCheck.TestAdapter
import Data.Typeable

deriving instance Typeable Hspec.Summary
type HspecSummary = Hspec.Summary

-- | Summarizable instance of `Hspec.Summary`
instance Summarizable HspecSummary where
  testSummary _mutant _test result = Summary $ _ioLog result
  isSuccess (Hspec.Summary { Hspec.summaryExamples = _, Hspec.summaryFailures = sf } ) = sf == 0
  parseResult s =
    let
      (ex, r) = (takeWhile isDigit s, dropWhile isDigit s)
      fs = takeWhile isDigit $ dropWhile (not . isDigit) r
    in
      Hspec.Summary (read @Int ex) (read @Int fs)
