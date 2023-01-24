module Test.Node.Library.Execa where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Node.Library.Execa (execaCommandSync)
import Node.Library.Execa.Utils (utf8)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

spec :: Spec Unit
spec = do
  describe "execa" do
    pure unit
  describe "execaSync" do
    pure unit
  describe "execaCommand" do
    pure unit
  describe "execaCommandSync" do
    describe "`cat` tests" do
      it "input is file" do
        result <- liftEffect $ execaCommandSync "cat test.dhall" identity
        case result of
          Right r -> r.stdout `shouldContain` "let config ="
          Left e -> fail e.message
      it "input is buffer" do
        result <- liftEffect $ execaCommandSync "cat" (_ { input = Just $ utf8.toBuffer "test" })
        case result of
          Right r -> r.stdout `shouldEqual` "test"
          Left e -> fail e.message
