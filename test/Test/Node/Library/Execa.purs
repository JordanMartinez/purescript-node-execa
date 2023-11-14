module Test.Node.Library.Execa where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Node.Buffer as Buffer
import Node.ChildProcess.Types (Exit(..), fromKillSignal', intSignal, stringSignal)
import Node.Encoding (Encoding(..))
import Node.Library.Execa (execa, execaCommand, execaCommandSync, execaSync)
import Node.Library.HumanSignals (signals)
import Node.Path as Path
import Node.UnsafeChildProcess.Safe as SafeCP
import Test.Node.Library.Utils (isWindows, itNix)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions as Assertions
import Test.Spec.Assertions.String (shouldContain)

spec :: Spec Unit
spec = describe "execa" do
  itNix "`echo test` should fail due to a Node.js bug" do
    spawned <- execa "echo" [] identity
    for_ spawned.stdin \s -> s.writeUtf8End "test"
    result <- spawned.getResult
    case result.stdinError of
      Nothing -> fail "Expected EPIPE error"
      Just e -> Exception.message e `shouldContain` "EPIPE"
  describe "`cat` tests" do
    it "input is file" do
      spawned <- execa "cat" [ "test.dhall" ] identity
      result <- spawned.getResult
      case result.exit of
        Normally 0 -> result.stdout `shouldContain` "let config ="
        _ -> fail result.message
    itNix "input is buffer" do
      spawned <- execa "cat" [ "-" ] identity
      for_ spawned.stdin \s -> s.writeUtf8End "test"
      result <- spawned.getResult
      case result.exit of
        Normally 0 -> result.stdout `shouldEqual` "test"
        _ -> fail result.message
  it "ENOENT should produce exit code 127" do
    result <- _.getResult =<< execa "this-does-not-exist" [] identity
    case result.exit of
      Normally 127 -> mempty
      _ -> fail $ "Should have gotten exit 127. " <> show result
  describe "using sleep files" do
    let
      shellCmd = if isWindows then "pwsh" else "sh"
      sleepFile = Path.concat [ "test", "fixtures", "sleep." <> if isWindows then "ps1" else "sh" ]
    describe "kill works" do
      it "basic cancel produces error" do
        spawned <- execa shellCmd [ sleepFile, "1" ] identity
        spawned.cancel
        result <- spawned.getResult
        unless result.canceled do
          fail "Cancelling should work"
      it "basic kill (string) produces error" do
        spawned <- execa shellCmd [ sleepFile, "1" ] identity
        _ <- spawned.killWithSignal (stringSignal "SIGTERM")
        result <- spawned.getResult
        case result.exit of
          Normally _ -> fail "Cancelling should work"
          BySignal sig -> sig `shouldEqual` (stringSignal "SIGTERM")
      it "basic kill (int) produces error" do
        spawned <- execa shellCmd [ sleepFile, "1" ] identity
        _ <- spawned.killWithSignal (intSignal signals.byName."SIGTERM".number)
        result <- spawned.getResult
        case result.exit of
          Normally _ -> fail "Cancelling should work"
          BySignal sig -> sig # fromKillSignal'
            (\i -> i `shouldEqual` signals.byName."SIGTERM".number)
            (\s -> s `shouldEqual` signals.byName."SIGTERM".name)
    describe "timeout produces error" do
      it "basic timeout produces error" do
        spawned <- execa shellCmd [ sleepFile, "10" ]
          (_ { timeout = Just { milliseconds: Milliseconds 400.0, killSignal: stringSignal "SIGTERM" } })
        result <- spawned.getResult
        case result.exit of
          Normally 64 | isWindows -> do
            sig <- liftEffect $ SafeCP.signalCode spawned.childProcess
            when (sig /= (Just "SIGTERM")) do
              Assertions.fail $ "Didn't get expected kill signal. Result was\n" <> show result
            unless (result.timedOut) do
              Assertions.fail $ "Result didn't indicate time out. Result was\n" <> show result
          BySignal sig -> do
            when (sig /= (stringSignal "SIGTERM")) do
              Assertions.fail $ "Didn't get expected kill signal. Result was\n" <> show result
            unless (result.timedOut) do
              Assertions.fail $ "Result didn't indicate time out. Result was\n" <> show result
          _ ->
            fail $ "Timeout should work: " <> show result
  describe "execaSync" do
    describe "`cat` tests" do
      it "input is file" do
        result <- liftEffect $ execaSync "cat" [ "test.dhall" ] identity
        case result.exit of
          Normally 0 -> result.stdout `shouldContain` "let config ="
          _ -> fail result.message
      itNix "input is buffer" do
        input <- liftEffect $ Buffer.fromString "test" UTF8
        result <- liftEffect $ execaSync "cat" [ "-" ] (_ { input = Just input })
        case result.exit of
          Normally 0 -> result.stdout `shouldEqual` "test"
          _ -> fail result.message
  describe "execaCommand" do
    describe "`cat` tests" do
      it "input is file" do
        spawned <- execaCommand "cat test.dhall" identity
        result <- spawned.getResult
        case result.exit of
          Normally 0 -> result.stdout `shouldContain` "let config ="
          _ -> fail result.message
      itNix "input is buffer" do
        spawned <- execaCommand "cat -" identity
        for_ spawned.stdin \s -> s.writeUtf8End "test"
        result <- spawned.getResult
        case result.exit of
          Normally 0 -> result.stdout `shouldEqual` "test"
          _ -> fail result.message
  describe "execaCommandSync" do
    describe "`cat` tests" do
      it "input is file" do
        result <- liftEffect $ execaCommandSync "cat test.dhall" identity
        case result.exit of
          Normally 0 -> result.stdout `shouldContain` "let config ="
          _ -> fail result.message
      itNix "input is buffer" do
        input <- liftEffect $ Buffer.fromString "test" UTF8
        result <- liftEffect $ execaCommandSync "cat" (_ { input = Just input })
        case result.exit of
          Normally 0 -> result.stdout `shouldEqual` "test"
          _ -> fail result.message
