module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Node.Library.Execa as Execa
import Test.Node.Library.ParseCommand as ParseCommand
import Test.Node.Library.Which as Which
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  ParseCommand.spec
  Which.spec
  Execa.spec
