module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Node.Library.Execa as Execa
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

main :: Effect Unit
main = launchAff_ $ void $ join $ runSpecT defaultConfig [ consoleReporter ] do
  Execa.spec
