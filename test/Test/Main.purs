module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Node.Library.Execa as Execa
import Test.Node.Library.Execa.Windows as Windows
import Test.Node.Library.ParseCommand as ParseCommand
import Test.Node.Library.ShebangCommand as ShebangCommand
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (Config, defaultConfig, runSpec, runSpecT)

testConfig :: Config
testConfig = defaultConfig
  { slow = Milliseconds 10_000.0
  , timeout = Just (Milliseconds 300_000.0)
  , exit = true
  }

main :: Effect Unit
main = do
  runWindowsTests <- runSpecT testConfig [ consoleReporter ] do
    Windows.spec
  launchAff_ do
    void runWindowsTests
    -- runSpec [ consoleReporter ] do
    --   Execa.spec
    --   ParseCommand.spec
    --   ShebangCommand.spec
  