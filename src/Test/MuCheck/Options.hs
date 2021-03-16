{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
module Test.MuCheck.Options where

import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Yaml as Y
import Test.MuCheck.TestAdapter (TestRunner(..))

data MuOptions = MuOptions
    { tixFile :: Maybe FilePath
    , mutantModule :: FilePath
    , testRunner :: TestRunner
    , testFile :: FilePath
    , testCommand :: String
    , ghcidCommand :: String
    }
    deriving stock (Show, Generic)
    deriving anyclass (A.FromJSON)

optionsFromFile :: FilePath -> IO (Maybe MuOptions)
optionsFromFile = fmap (either (const Nothing) Just) . Y.decodeFileEither