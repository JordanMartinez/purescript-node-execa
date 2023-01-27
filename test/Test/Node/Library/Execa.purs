module Test.Node.Library.Execa where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, joinFiber)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.Library.Execa (execa, execaCommand, execaCommandSync, execaSync)
import Node.Library.Execa.Utils (utf8)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

spec :: SpecT Aff Unit Aff Unit
spec = do
  describe "execa" do
    it "`echo test` should fail due to a Node.js bug" do
      spawned <- execa "echo" [] identity
      spawned.writeCloseStdin UTF8 "test"
      result <- joinFiber spawned.run
      case result of
        Right _ -> fail "Expected EPIPE error"
        Left e -> e.message `shouldContain` "EPIPE"
    describe "`cat` tests" do
      it "input is file" do
        spawned <- execa "cat" [ "test.dhall" ] identity
        result <- joinFiber spawned.run
        case result of
          Right r -> r.stdout `shouldContain` "let config ="
          Left e -> fail e.message
      it "input is buffer" do
        spawned <- execa "cat" [ "-" ] identity
        spawned.writeCloseStdin UTF8 "test"
        result <- joinFiber spawned.run
        case result of
          Right r -> r.stdout `shouldEqual` "test"
          Left e -> fail e.message
    describe "kill works" do
      it "basic kill produces error" do
        spawned <- execa "bash" [ "test/fixtures/sleep.sh", "1" ] identity
        liftEffect spawned.cancel
        result <- joinFiber spawned.run
        case result of
          Right _ -> fail "Cancelling should work"
          Left e -> e.isCanceled `shouldEqual` true
    describe "all stream works" do
      it "basic kill produces error" do
        spawned <- execa "bash" [ "test/fixtures/outErr.sh" ] identity
        void $ joinFiber spawned.run
        all <- spawned.all >>= _.result
        all.string `shouldEqual` "stdout\nstderr\n"
  describe "execaSync" do
    describe "`cat` tests" do
      it "input is file" do
        result <- liftEffect $ execaSync "cat" [ "test.dhall" ] identity
        case result of
          Right r -> r.stdout `shouldContain` "let config ="
          Left e -> fail e.message
      it "input is buffer" do
        result <- liftEffect $ execaSync "cat" [ "-" ] (_ { input = Just $ utf8.toBuffer "test" })
        case result of
          Right r -> r.stdout `shouldEqual` "test"
          Left e -> fail e.message
  describe "execaCommand" do
    describe "`cat` tests" do
      it "input is file" do
        spawned <- execaCommand "cat test.dhall" identity
        result <- joinFiber spawned.run
        case result of
          Right r -> r.stdout `shouldContain` "let config ="
          Left e -> fail e.message
      it "input is buffer" do
        spawned <- execaCommand "cat -" identity
        spawned.writeCloseStdin UTF8 "test"
        result <- joinFiber spawned.run
        case result of
          Right r -> r.stdout `shouldEqual` "test"
          Left e -> fail e.message
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
