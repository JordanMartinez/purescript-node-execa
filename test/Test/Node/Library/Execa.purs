module Test.Node.Library.Execa where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Node.Library.Execa (execa, execaCommand, execaCommandSync, execaSync)
import Node.Library.Execa.ParseCommand (parseCommand')
import Node.Library.Execa.Utils (utf8)
import Node.Library.HumanSignals (signals)
import Node.Platform (Platform(..))
import Node.Process as Process
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Assertions.String (shouldContain)

spec :: Spec Unit
spec = do
  describe "execa" do
    if (Process.platform /= Just Win32) then do
      nixTests
    else
      windowsTests

nixTests :: Spec Unit
nixTests = do
  describe "*nix" do
    it "`echo test` should fail due to a Node.js bug" do
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
      it "input is buffer" do
        spawned <- execa "cat" [ "-" ] identity
        spawned.stdin.writeUtf8End "test"
        result <- spawned.result
        case result of
          Right r -> r.stdout `shouldEqual` "test"
          Left e -> fail e.message
    describe "kill works" do
      it "basic cancel produces error" do
        spawned <- execa "bash" [ "test/fixtures/sleep.sh", "1" ] identity
        spawned.cancel
        result <- spawned.result
        case result of
          Right _ -> fail "Cancelling should work"
          Left e -> e.isCanceled `shouldEqual` true
      it "basic kill (string) produces error" do
        spawned <- execa "bash" [ "test/fixtures/sleep.sh", "1" ] identity
        _ <- spawned.killWithSignal (Right "SIGTERM")
        result <- spawned.result
        case result of
          Right _ -> fail "Cancelling should work"
          Left e -> e.signal `shouldEqual` (Just $ Right "SIGTERM")
      it "basic kill (int) produces error" do
        spawned <- execa "bash" [ "test/fixtures/sleep.sh", "1" ] identity
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
        spawned <- execa "bash" [ "test/fixtures/sleep.sh", "10" ]
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
      it "input is buffer" do
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
      it "input is buffer" do
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
      it "input is buffer" do
        result <- liftEffect $ execaCommandSync "cat" (_ { input = Just $ utf8.toBuffer "test" })
        case result of
          Right r -> r.stdout `shouldEqual` "test"
          Left e -> fail e.message
  describe "parseCommand" do
    let
      shouldBeFileArgs file args = do
        let result = parseCommand' $ (fst file) <> " " <> (Array.intercalate " " $ map fst args)
        Right (snd file) `shouldEqual` (map _.file) result
        Right (map snd args) `shouldEqual` (map _.args) result

      escapeSlash = """\"""
      backslash = escapeSlash
      space = " "
      escapedSpace = escapeSlash <> " "
      dquote = "\""
      squote = "'"
      escSQuote = escapeSlash <> squote
      escDQuote = escapeSlash <> dquote
      escBackslash = escapeSlash <> backslash

    it "should work despite extra spaces" do
      shouldBeFileArgs
        (" file  " /\ "file")
        [ "    arg1" /\ "arg1"
        , "arg2   " /\ "arg2"
        , "   arg3   " /\ "arg3"
        ]

    it "should account for escaped spaces, double-quotes, single-quotes, and back slashes" do
      shouldBeFileArgs
        ("file" /\ "file")
        [ ("a" <> escapedSpace <> "b") /\ ("a" <> space <> "b")
        , ("a" <> escDQuote <> "b") /\ ("a" <> dquote <> "b")
        , ("a" <> escSQuote <> "b") /\ ("a" <> squote <> "b")
        , ("a" <> escBackslash <> "b") /\ ("a" <> backslash <> "b")
        ]

    it "should account for escaped double-quotes within double-quote context" do
      shouldBeFileArgs
        ("file" /\ "file")
        [ (dquote <> "a" <> escDQuote <> "b" <> dquote) /\ ("a" <> dquote <> "b")
        ]

    it "should account for escaped single-quotes within single-quote context" do
      shouldBeFileArgs
        ("file" /\ "file")
        [ (squote <> "a" <> escSQuote <> "b" <> squote) /\ ("a" <> squote <> "b")
        ]

    it "should account for ignore all other escaped chars in a double- or single-quote context" do
      shouldBeFileArgs
        ("file" /\ "file")
        [ (dquote <> "a" <> escSQuote <> "b" <> dquote) /\ ("a" <> escSQuote <> "b")
        , (squote <> "a" <> escDQuote <> "b" <> squote) /\ ("a" <> escDQuote <> "b")
        ]

windowsTests :: Spec Unit
windowsTests = do
  describe "windows" do
    pure unit
