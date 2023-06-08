module Test.Node.Library.Which where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Node.Library.Execa.Utils (toError)
import Node.Library.Execa.Which (defaultWhichOptions, isWindows, which)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = do
  describe "which" do
    it "should find all binaries for spago" do
      result <- which "spago" defaultWhichOptions
      isWin <- liftEffect isWindows
      case result of
        Left e -> fail $ message $ toError e
        Right a
          | isWin && NEA.length a < 2 -> fail $ "Only detected these many binaries on Windows: " <> show a
          | otherwise -> pure unit
