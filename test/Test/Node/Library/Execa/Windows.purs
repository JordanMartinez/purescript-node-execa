module Test.Node.Library.Execa.Windows where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.CodePoints as SCP


import Effect (Effect)
import Effect.Aff (Aff, Error, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Effect.Exception (message)
import Node.ChildProcess.Types (Exit(..), pipe, inherit)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.FS.Perms (permsAll)
import Node.FS.Sync as FS
import Node.Library.Execa (ExecaResult, execa, execa')
import Node.Library.Execa.CrossSpawn (MetaChar(..), escapeAll, ignoring)
import Node.OS as OS
import Node.Path (FilePath, dirname)
import Node.Path as Path
import Node.Process as Process
import Test.Node.Library.Utils (describeWindows)
import Test.Spec (SpecT, afterAll, beforeAll, before_, it)
import Test.Spec.Assertions (fail)

type SpecProps = 
  { pwd :: FilePath
  , tmpDir :: FilePath
  , checkFixture :: FilePath -> String -> Aff Unit
  }

setupTempDir :: String -> Aff SpecProps
setupTempDir pursVersion = do
  pwd <- liftEffect Process.cwd
  osTmpDir <- liftEffect OS.tmpdir
  tmpDir <- FSA.mkdtemp (Path.concat [ osTmpDir, "execa-windows-tests-" ])
  liftEffect $ Process.chdir tmpDir
  log $ "Running tests in temp directory: " <> tmpDir
  result <- _.getResult =<< execa "npm.cmd" [ "install", "purescript@" <> pursVersion ] (_ {
    stdout = Just inherit,
    stderr = Just inherit
  })
  log $ "Install PureScript Exit Status: " <> show result.exit
  log $ "Message: \r\n" <> result.message
  let 
    checkFixture :: FilePath -> String -> Aff Unit
    checkFixture fileExpected actual = do
      overwriteSpecFile <- liftEffect $ map isJust $ Process.lookupEnv "SPEC_TEST_ACCEPT"
      let fixtureFileExpected = Path.concat [ pwd, "test", "fixtures", "cmd-shim", fileExpected ]
      if overwriteSpecFile then do
        Console.log $ "Overwriting fixture at path: " <> fixtureFileExpected
        let parentDir = dirname fixtureFileExpected
        unlessM (liftEffect $ FS.exists parentDir) $ FSA.mkdir' parentDir { mode: permsAll, recursive: true }
        FSA.writeTextFile UTF8 fixtureFileExpected (actual <> "\n")
      else do
        expected <- String.trim <$> FSA.readTextFile UTF8 fixtureFileExpected
        actual `shouldEqualStr` expected

  pure
    { pwd
    , tmpDir
    , checkFixture
    }

cleanupTempDir :: SpecProps -> Aff Unit
cleanupTempDir { tmpDir, pwd } = do
  liftEffect $ Process.chdir pwd
  FSA.rm' tmpDir { recursive: true, maxRetries: 0, force: false, retryDelay: 0 }

prettyPrint :: String -> String
prettyPrint =
    String.replaceAll (Pattern "\\r\\n") (Replacement "\r\n")
      <<< String.replaceAll (Pattern "\\\"") (Replacement "\"")

check
  :: { stdout :: String -> Aff Unit
     , stderr :: String -> Aff Unit
     , result :: ExecaResult -> Aff Unit
     }
  -> ExecaResult
  -> Aff Unit
check checkers execResult = do
  let
    stdout = String.trim $ execResult.stdout
    stderr = String.trim $ execResult.stderr

  printStdoutStderr <- liftEffect $ map isJust $ Process.lookupEnv "SPEC_TEST_DEBUG"

  when printStdoutStderr do
    log $ "STDOUT:\r\n" <> prettyPrint stdout
    log $ "STDERR:\r\n" <> prettyPrint stderr
  checkers.result execResult
  checkers.stdout stdout
  checkers.stderr stderr

checkOutputsStr
  :: { stdoutStr :: Maybe String
     , stderrStr :: Maybe String
     , result :: ExecaResult -> Aff Unit
     }
  -> ExecaResult
  -> Aff Unit
checkOutputsStr checkers =
  check
    { stdout: maybe mempty (\exp act -> act `shouldEqualStr` exp) checkers.stdoutStr
    , stderr: maybe mempty (\exp act -> act `shouldEqualStr` exp) checkers.stderrStr
    , result: checkers.result
    }

shouldEqualStr
  :: forall m
   . MonadThrow Error m
  => String
  -> String
  -> m Unit
shouldEqualStr v1 v2 =
  when (v1 /= v2) do
    fail $ Array.intercalate "\n"
      [ ""
      , "===== (Actual)"
      , v1
      , "====="
      , "  ≠"
      , "===== (Expected)"
      , v2
      , "====="
      , ""
      ]

isSuccess :: ExecaResult -> Aff Unit
isSuccess r = case r.exit of
  Normally 0 -> pure unit
  _ -> fail $ prettyPrint r.message

isFailure :: ExecaResult -> Aff Unit
isFailure r = case r.exit of
  Normally 0 -> fail $ prettyPrint r.message
  _ -> pure unit

resetDir :: Aff Unit
resetDir = do
  FSA.rm' "output" { recursive: true, force: true, maxRetries: 0, retryDelay: 0 }
  FSA.rm' "src" { recursive: true, force: true, maxRetries: 0, retryDelay: 0 }
  FSA.rm' ".spago" { recursive: true, force: true, maxRetries: 0, retryDelay: 0 }

spec :: SpecT Aff Unit Effect Unit
spec = describeWindows "CLI parsing" do
  let pursVersion = "0.15.10"
  beforeAll (setupTempDir pursVersion) do
    afterAll cleanupTempDir do
      before_ resetDir do
        let
          purs args = do
            _.getResult =<< execa "purs.cmd" args (_ 
              { stdout = Just pipe
              , stderr = Just pipe
              , preferLocal = Just { localDir: Nothing, execPath: Nothing }
              })
        it "verify PureScript works" \_ -> do
          purs [ "--version" ] >>= checkOutputsStr { stdoutStr: Just pursVersion, stderrStr: Nothing, result: isSuccess }

        let
          mkPursFile path modParts = do
            let parentDir = Path.dirname path
            alreadyThere <- liftEffect $ FS.exists parentDir
            unless alreadyThere do
              FSA.mkdir' parentDir { recursive: true, mode: permsAll }
            try $ FSA.writeTextFile UTF8 path $ "module " <> (Array.intercalate "." modParts) <> " where\r\n\r\nx = 1 :: Int"

          singleFileTests :: Array { glob :: FilePath, modParts :: Array String, testName :: String, mkFixture :: String -> String }
          singleFileTests = do
            -- Note: the `:` character is excluded because it writes its content to the stream named `o.purs`.
            -- `purs` does not read that stream when reading from the file.
            specialSymbol <- Array.cons Nothing $ map (Just <<< SCP.singleton) $ String.toCodePointArray "~!@#$%^&*()_+`{}|[]\\\";',./<>? "
            let glob = Path.concat [ "src", "Fo" <> (fold specialSymbol) <> "o.purs" ]
            pure 
              { glob
              , modParts: [ "Foo" ]
              , testName: "compiles a single file with glob: " <> glob
              , mkFixture: \outputChannel ->
                  Array.intercalate "-" $ Array.catMaybes [ pure "single-file", toCharName =<< specialSymbol, pure outputChannel ]
              }
        for_ singleFileTests \{ glob, modParts, testName, mkFixture } -> do
          it testName \{ checkFixture, tmpDir } -> do
            result <- mkPursFile glob modParts
            case result of
              Left err -> checkFixture (mkFixture "error") 
                $ String.replaceAll (Pattern tmpDir) (Replacement "")
                $ message err
              Right _ ->
                purs [ "compile", glob ] >>= check 
                  { stdout: checkFixture "single-file-success-stdout"
                  , stderr: checkFixture "single-file-success-stderr"
                  , result: isSuccess 
                  }
        
        it "does not compile a single file with glob: src\\Fo:o.purs" \{ checkFixture, tmpDir } -> do
            let glob = Path.concat [ "src", "Fo:o.purs" ]
            let mkFixture name = Array.intercalate "-" [ "single-file", "colon", name]
            result <- mkPursFile glob ["Foo"]
            case result of
              Left err -> checkFixture (mkFixture "error")
                $ String.replaceAll (Pattern tmpDir) (Replacement "")
                $ message err
              Right _ ->
                purs [ "compile", glob ] >>= check 
                  { stdout: checkFixture (mkFixture "stdout")
                  , stderr: checkFixture (mkFixture "stderr")
                  , result: isFailure
                  }

        it "user can optionally choose not to escape args with '*' character" \{ checkFixture, tmpDir } -> do
          let
            purs' args = do
              _.getResult =<< execa' "purs.cmd" args (_ 
                { stdout = Just pipe
                , stderr = Just pipe
                , preferLocal = Just { localDir: Nothing, execPath: Nothing }
                })
            spagoGlobPrefix = [ ".spago", "packages", "package-name-1.2.3", "src" ]
              
          let path = Path.concat $ spagoGlobPrefix <> [ "Foo.purs " ]
          let modParts = ["Foo"]
          result <- mkPursFile path modParts
          case result of
            Left err -> checkFixture "single-file-glob-error"
                $ String.replaceAll (Pattern tmpDir) (Replacement "")
                $ message err
            Right _ -> do
              let ignoringStar = ignoring [ MetaAsterisk ]
              r <- purs' [ escapeAll "compile", ignoringStar $ Path.concat $ spagoGlobPrefix <> [ "**", "*.purs" ] ]
              checkFixture "single-file-glob-escaped-command" r.escapedCommand
              check 
                { stdout: checkFixture "single-file-glob-stdout"
                , stderr: checkFixture "single-file-glob-stderr"
                , result: isSuccess
                }
                r

  pure unit

toCharName :: String -> Maybe String
toCharName = case _ of
  "~" -> Just "tilde"
  "!" -> Just "exclamation"
  "@" -> Just "at"
  "#" -> Just "hash"
  "$" -> Just "dollar"
  "%" -> Just "percent"
  "^" -> Just "caret"
  "&" -> Just "ampersand"
  "*" -> Just "asterisk"
  "(" -> Just "left_parens"
  ")" -> Just "right_parens"
  "_" -> Just "underscore"
  "+" -> Just "plus"
  "`" -> Just "backtick"
  "{" -> Just "left_curly"
  "}" -> Just "right_curly"
  "|" -> Just "pipe"
  "[" -> Just "left_bracket"
  "]" -> Just "right_bracket"
  "\\" -> Just "backslash"
  ":" -> Just "colon"
  "\"" -> Just "double_quote"
  ";" -> Just "semi_colon"
  "'" -> Just "single_quote"
  "," -> Just "comma"
  "." -> Just "period"
  "/" -> Just "forward_slash"
  "<" -> Just "left_angle"
  ">" -> Just "right_angle"
  "?" -> Just "question"
  " " -> Just "space"
  _ -> Nothing