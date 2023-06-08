module Test.Node.Library.Execa where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Node.Library.Execa (execa, execaCommand, execaCommandSync, execaSync)
import Node.Library.Execa.Utils (utf8)
import Node.Library.HumanSignals (signals)
import Node.Path as Path
import Test.Node.Library.Utils (isWindows, itNix)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

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
  describe "using sleep files" do
    let
      shellCmd = if isWindows then "pwsh" else "sh"
      sleepFile = Path.concat [ "test", "fixtures", "sleep." <> if isWindows then "cmd" else "sh" ]
    describe "kill works" do
      it "basic cancel produces error" do
        spawned <- execa shellCmd [ sleepFile, "1" ] identity
        spawned.cancel
        result <- spawned.result
        case result of
          Right _ -> fail "Cancelling should work"
          Left e -> e.isCanceled `shouldEqual` true
      it "basic kill (string) produces error" do
        spawned <- execa shellCmd [ sleepFile, "1" ] identity
        _ <- spawned.killWithSignal (Right "SIGTERM")
        result <- spawned.result
        case result of
          Right _ -> fail "Cancelling should work"
          Left e -> e.signal `shouldEqual` (Just $ Right "SIGTERM")
      it "basic kill (int) produces error" do
        spawned <- execa shellCmd [ sleepFile, "1" ] identity
        _ <- spawned.killWithSignal (Left signals.byName."SIGTERM".number)
        result <- spawned.result
        case result of
          Right _ -> fail "Cancelling should work"
          Left e -> case e.signal of
            Just (Left i) -> i `shouldEqual` signals.byName."SIGTERM".number
            Just (Right s) -> s `shouldEqual` signals.byName."SIGTERM".name
            _ -> fail "Did not get a kill signal"
    describe "timeout produces error" do
      it "basic timeout produces error" do
        spawned <- execa shellCmd [ sleepFile, "10" ]
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
