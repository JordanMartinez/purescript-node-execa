module Test.Node.Library.Execa where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Node.Library.Execa (execa, execaCommand, execaCommandSync, execaSync)
import Node.Library.Execa.Utils (utf8)
import Node.Library.HumanSignals (signals)
import Node.Platform (Platform(..))
import Node.Process as Process
import Test.Spec (class Example, Spec, SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

isWindows :: Boolean
isWindows = Process.platform == Just Win32

itWindows :: forall m t arg g. Monad m => Example t arg g => String -> t -> SpecT g arg m Unit
itWindows msg test = do
  when isWindows do
    it (msg <> " (windows only)") test

itNix :: forall m t arg g. Monad m => Example t arg g => String -> t -> SpecT g arg m Unit
itNix msg test = do
  unless isWindows do
    it (msg <> " (*nix only)") test

spec :: Spec Unit
spec = describe "execa" do
  itNix "`echo test` should fail due to a Node.js bug" do
    spawned <- execa "echo" [] identity
    spawned.stdin.writeUtf8End "test"
    result <- spawned.result
    case result of
      Right _ -> fail "Expected EPIPE error"
      Left e -> e.message `shouldContain` "EPIPE"
  describe "`cat` tests" do
    it "input is file" do
      spawned <- execa "cat" [ "test.dhall" ] identity
      result <- spawned.result
      case result of
        Right r -> r.stdout `shouldContain` "let config ="
        Left e -> fail e.message
    itNix "input is buffer" do
      spawned <- execa "cat" [ "-" ] identity
      spawned.stdin.writeUtf8End "test"
      result <- spawned.result
      case result of
        Right r -> r.stdout `shouldEqual` "test"
        Left e -> fail e.message
  describe "kill works" do
    itNix "basic cancel produces error" do
      spawned <- execa "bash" [ "test/fixtures/sleep.sh", "1" ] identity
      spawned.cancel
      result <- spawned.result
      case result of
        Right _ -> fail "Cancelling should work"
        Left e -> e.isCanceled `shouldEqual` true
    itWindows "basic cancel produces error" do
      spawned <- execa "test/fixtures/sleep.cmd" [ "1" ] identity
      spawned.cancel
      result <- spawned.result
      case result of
        Right _ -> fail "Cancelling should work"
        Left e -> e.isCanceled `shouldEqual` true
    itNix "basic kill (string) produces error" do
      spawned <- execa "test/fixtures/sleep.cmd" [ "1" ] identity
      _ <- spawned.killWithSignal (Right "SIGTERM")
      result <- spawned.result
      case result of
        Right _ -> fail "Cancelling should work"
        Left e -> e.signal `shouldEqual` (Just $ Right "SIGTERM")
    itNix "basic kill (int) produces error" do
      spawned <- execa "test/fixtures/sleep.cmd" [ "1" ] identity
      _ <- spawned.killWithSignal (Left signals.byName."SIGTERM".number)
      result <- spawned.result
      case result of
        Right _ -> fail "Cancelling should work"
        Left e -> case e.signal of
          Just (Left i) -> i `shouldEqual` signals.byName."SIGTERM".number
          Just (Right s) -> s `shouldEqual` signals.byName."SIGTERM".name
          _ -> fail "Did not get a kill signal"
  describe "timeout produces error" do
    itNix "basic timeout produces error" do
      spawned <- execa "test/fixtures/sleep.cmd" [ "10" ]
        (_ { timeout = Just { milliseconds: 400.0, killSignal: Right "SIGTERM" } })
      result <- spawned.result
      case result of
        Right _ -> fail "Timeout should work"
        Left e -> do
          e.signal `shouldEqual` (Just $ Right "SIGTERM")
          e.timedOut `shouldEqual` true
  describe "execaSync" do
    describe "`cat` tests" do
      it "input is file" do
        result <- liftEffect $ execaSync "cat" [ "test.dhall" ] identity
        case result of
          Right r -> r.stdout `shouldContain` "let config ="
          Left e -> fail e.message
      itNix "input is buffer" do
        result <- liftEffect $ execaSync "cat" [ "-" ] (_ { input = Just $ utf8.toBuffer "test" })
        case result of
          Right r -> r.stdout `shouldEqual` "test"
          Left e -> fail e.message
  describe "execaCommand" do
    describe "`cat` tests" do
      it "input is file" do
        spawned <- execaCommand "cat test.dhall" identity
        result <- spawned.result
        case result of
          Right r -> r.stdout `shouldContain` "let config ="
          Left e -> fail e.message
      itNix "input is buffer" do
        spawned <- execaCommand "cat -" identity
        spawned.stdin.writeUtf8End "test"
        result <- spawned.result
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
      itNix "input is buffer" do
        result <- liftEffect $ execaCommandSync "cat" (_ { input = Just $ utf8.toBuffer "test" })
        case result of
          Right r -> r.stdout `shouldEqual` "test"
          Left e -> fail e.message
