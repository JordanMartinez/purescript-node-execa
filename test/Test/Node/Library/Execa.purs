module Test.Node.Library.Execa where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, joinFiber)
import Effect.Class (liftEffect)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Library.Execa (execa, execaCommandSync)
import Node.Library.Execa.ChildProcess (stdin)
import Node.Library.Execa.Utils (utf8)
import Node.Stream as Stream
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

spec :: SpecT Aff Unit Aff Unit
spec = do
  describe "execa" do
    it "`echo test` should fail due to a Node.js bug" do
      spawned <- execa "echo" [] identity
      liftEffect do
        buf <- Buffer.fromString "test" UTF8
        void $ Stream.write (stdin spawned.childProcess) buf mempty
        void $ Stream.end (stdin spawned.childProcess) mempty
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
        liftEffect do
          buf <- Buffer.fromString "test" UTF8
          void $ Stream.write (stdin spawned.childProcess) buf mempty
          void $ Stream.end (stdin spawned.childProcess) mempty
        result <- joinFiber spawned.run
        case result of
          Right r -> r.stdout `shouldEqual` "test"
          Left e -> fail e.message
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
