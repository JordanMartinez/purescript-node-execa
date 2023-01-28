-- | Provides a higher-level replacement to Node.js `child_process` module.
-- | Uses sane defaults with clearer error messages.
-- | - `spawn`/`spawnSync` -> `execa`/`execaSync`
-- | - `exec`/`execSync` -> `execaCommand`/`execaCommandSync`
-- | - `fork` - has no equivalent
module Node.Library.Execa
  ( ExecaError
  , ExecaOptions
  , ExecaResult
  , ExecaSuccess
  , execa
  , ExecaSyncOptions
  , ExecaSyncResult
  , execaSync
  , execaCommand
  , execaCommandSync
  ) where

import Prelude

import Control.Alternative ((<|>), guard)
import Control.Parallel (parOneOf)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (for_, sequence_)
import Data.Int (floor, toNumber)
import Data.Lens (Prism', is, preview, prism)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.String as String
import Data.String.Regex (Regex, test)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, Milliseconds(..), effectCanceler, finally, joinFiber, makeAff, never, nonCanceler, suspendAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn3, runEffectFn1, runEffectFn2)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (unsafeThaw)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Immutable as ImmutableBuffer
import Node.Buffer.Internal as Buffer
import Node.Encoding (Encoding(..))
import Node.Library.Execa.ChildProcess (ChildProcess, KillSignal, fromKillSignal, intKillSignal, kill, kill', onError, onExit, spawnSync, stderr, stdin, stdout, stringKillSignal)
import Node.Library.Execa.ChildProcess as ChildProcess
import Node.Library.Execa.CrossSpawn (CrossSpawnConfig)
import Node.Library.Execa.CrossSpawn as CrossSpawn
import Node.Library.Execa.GetStream (getStreamBuffer)
import Node.Library.Execa.MergeStream as MergeStreams
import Node.Library.Execa.NpmRunPath (defaultNpmRunPathOptions, npmRunPathEnv)
import Node.Library.Execa.SignalExit as SignalExit
import Node.Library.Execa.StripFinalNewline (stripFinalNewlineBuf)
import Node.Library.HumanSignals (signals)
import Node.Process as Process
import Node.Stream (Duplex, destroy)
import Node.Stream as Stream
import Node.Stream as Streams
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Updates the given `env`:
-- | - if `extendEnv` is enabled, unions it with `Process.env`
-- | - if `preferLocal` is enabled, prepends the current directory
-- | and its parents' `node_modules/.bin` to `PATH`
getEnv
  :: { env :: Object String
     , extendEnv :: Boolean
     , preferLocal :: Maybe { localDir :: Maybe String, execPath :: Maybe String }
     }
  -> Effect (Object String)
getEnv r = do
  processEnv <- Process.getEnv
  let
    env = if r.extendEnv then Object.union r.env processEnv else r.env
  case r.preferLocal of
    Nothing ->
      pure env
    Just options ->
      npmRunPathEnv env $ defaultNpmRunPathOptions { cwd = options.localDir, execPath = options.execPath }

-- | - `cleanup` (default: `true`): Kill the spawned process when the parent process exits unless either:
-- |  - the spawned process is `detached`
-- |  - the parent process is terminated abruptly, for example, with SIGKILL as opposed to SIGTERM or a normal exit
-- | `- preferLocal` (default: `Nothing`): When `Just`, includes and prefers locally-installed `node_modules/.bin` binaries
-- |   when looking for a binary to execute. In short, if you `npm install foo``, you can run `execa "foo"`.
-- |   `localDir` (if `Nothing`, `Process.cwd` is used) - Preferred path to find locally installed binaries in
-- |   `execPath` (if `Nothing`, `Process.execPath` is used) - Path to the Node.js executable to use in child processes. 
-- |      This can be either an absolute path or a path relative to the `localDir` option.
-- | - `stripFinalNewline` - (default: `true`). If enabled, trims the newline character of `stdout`/`stderr` (e.g. `/(?:/r/n)|\r|\n$/`
-- | - `extendEnv` (default: `true`) - Extends the child process' `env` with `Process.env`
-- | - `argv0` - see Node docs
-- | - `stdioExtra` - Append any other `stdio` values to the array.
-- |    The `stdio` array used is always `["pipe", "pipe", "pipe", "ipc"] <> fromMaybe [] options.stdioExtra`
-- | - `detached` - see Node docs
-- | - `uid` - see Node docs
-- | - `gid` - see Node docs
-- | - `shell` - see Node docs. The Boolean variant is not supported
-- | - `timeout` - the amount of time to wait before killing the child process with the given kill signal
-- | - `maxBuffer` - the amount of buffer space available to `stdout`/`stderr`.
-- |    If more data is written to their buffers, child process will error with a max buffer size exceeded error.
-- | - `windowsVerbatimArguments` - see Node docs
-- | - `windowsHide` - see Node docs
-- | - `windowsEnableCmdEcho` (default: `true`) - Enables the `\q` flag when using the `cmd` shell. See https://github.com/nodejs/node/issues/27120
-- |    This goes against the Windows' defaults but makes the `stdout`/`stderr` behavior more consistent across different operating systems.
type ExecaOptions =
  -- execa options
  { cleanup :: Maybe Boolean
  , preferLocal :: Maybe { localDir :: Maybe String, execPath :: Maybe String }
  , stripFinalNewline :: Maybe Boolean
  , extendEnv :: Maybe Boolean
  -- child process spawn options:
  , cwd :: Maybe String
  , env :: Maybe (Object String)
  , encoding :: Maybe Encoding
  , argv0 :: Maybe String
  , stdioExtra :: Maybe (Array Foreign)
  , detached :: Maybe Boolean
  , uid :: Maybe Int
  , gid :: Maybe Int
  , shell :: Maybe String
  , timeout :: Maybe { milliseconds :: Number, killSignal :: Either Int String }
  , maxBuffer :: Maybe Number
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  -- cross spawn options
  , windowsEnableCmdEcho :: Maybe Boolean
  }

defaultExecaOptions :: ExecaOptions
defaultExecaOptions =
  { cleanup: Nothing
  , preferLocal: Nothing
  , stdioExtra: Nothing
  , stripFinalNewline: Nothing
  , extendEnv: Nothing
  , encoding: Nothing
  , cwd: Nothing
  , env: Nothing
  , argv0: Nothing
  , detached: Nothing
  , uid: Nothing
  , gid: Nothing
  , shell: Nothing
  , timeout: Nothing
  , maxBuffer: Nothing
  , windowsVerbatimArguments: Nothing
  , windowsHide: Nothing
  , windowsEnableCmdEcho: Nothing
  }

defaultOptions
  :: { cleanup :: Boolean
     , extendEnv :: Boolean
     , maxBuffer :: Number
     , preferLocal ::
         Maybe
           { execPath :: Maybe String
           , localDir :: Maybe String
           }
     , encoding :: Encoding
     , stripFinalNewline :: Boolean
     , windowsEnableCmdEcho :: Boolean
     , windowsHide :: Boolean
     , windowsVerbatimArguments :: Boolean
     }
defaultOptions =
  { cleanup: true
  , preferLocal: Just { localDir: Nothing, execPath: Nothing }
  , stripFinalNewline: true
  , extendEnv: true
  , maxBuffer: toNumber $ 1_000 * 1_000 * 100 -- 100 MB
  , encoding: UTF8
  , windowsVerbatimArguments: false
  , windowsHide: true
  , windowsEnableCmdEcho: false
  }

handleArguments
  :: String
  -> Array String
  -> ExecaOptions
  -> Effect { file :: String, args :: Array String, options :: ExecaRunOptions, parsed :: CrossSpawnConfig }
handleArguments file args initOptions = do
  parsed <- CrossSpawn.parse file args
    { shell: initOptions.shell
    , env: initOptions.env
    , cwd: initOptions.cwd
    , windowsVerbatimArguments: Nothing
    , windowsEnableCmdEcho: fromMaybe defaultOptions.windowsEnableCmdEcho initOptions.windowsEnableCmdEcho
    }
  processCwd <- Process.cwd
  env <- getEnv
    { env: fromMaybe Object.empty initOptions.env
    , extendEnv: fromMaybe defaultOptions.extendEnv initOptions.extendEnv
    , preferLocal: initOptions.preferLocal
    }
  let
    -- validateTimeout
    { timeout, killSignal } = fromMaybe { timeout: Nothing, killSignal: Nothing } do
      { milliseconds, killSignal } <- initOptions.timeout
      guard $ milliseconds > 0.0
      pure { timeout: Just milliseconds, killSignal: Just killSignal }

    options =
      { cleanup: fromMaybe defaultOptions.cleanup initOptions.cleanup
      , stdioExtra: fromMaybe [] initOptions.stdioExtra
      , maxBuffer: fromMaybe defaultOptions.maxBuffer initOptions.maxBuffer
      , stripFinalNewline: fromMaybe defaultOptions.stripFinalNewline initOptions.stripFinalNewline
      , cwd: fromMaybe processCwd initOptions.cwd
      , encoding: fromMaybe defaultOptions.encoding initOptions.encoding
      , env
      , argv0: initOptions.argv0
      , detached: fromMaybe false initOptions.detached
      , uid: initOptions.uid
      , gid: initOptions.gid
      , shell: initOptions.shell
      , timeout
      , killSignal
      , windowsHide: fromMaybe defaultOptions.windowsHide initOptions.windowsHide
      , windowsVerbatimArguments: fromMaybe defaultOptions.windowsVerbatimArguments parsed.options.windowsVerbatimArguments
      }
  pure { file: parsed.command, args: parsed.args, options, parsed }

-- | `childProcess` - access to the child process itself
-- | `run` - gets the result of the process via `joinFiber run`
-- | `cancel` - kill the child process, but indicate it was cancelled rather than killed in the error message
-- | `all` - interleave `stdout`/`stderr` into one stream
type ExecaResult =
  { all ::
      Aff
        { result :: Aff { inputError :: Maybe Error, string :: String }
        , stream :: Duplex
        }
  , cancel :: Effect Unit
  , childProcess :: ChildProcess
  , run :: Fiber (Either ExecaError ExecaSuccess)
  , writeStdin :: Encoding -> String -> Aff Unit
  , writeCloseStdin :: Encoding -> String -> Aff Unit
  , closeStdin :: Aff Unit
  }

type ExecaSuccess =
  { command :: String
  , escapedCommand :: String
  , exitCode :: Int
  , stderr :: String
  , stdout :: String
  }

-- | Replacement for `childProcess.spawn`. Since this is asynchronous,
-- | the returned value will not provide any results until one calls `joinFiber run`:
-- | `execa ... >>= \result -> joinFiber result.run`. 
-- |
-- | Override the default options using record update syntax.
-- | If defaults are good enough, just use `identity`.
-- | ```
-- | result <- execa "git checkout -b my-branch" (_
-- |    { cwd = Just $ Path.concat [ "some", "other", "directory"]
-- |    })
-- | joinFiber result.run
-- |
-- | result2 <- execa "git checkout -b my-branch" identity
-- | joinFiber result2.run
-- | ```
execa :: String -> Array String -> (ExecaOptions -> ExecaOptions) -> Aff ExecaResult
execa file args buildOptions = do
  let options = buildOptions defaultExecaOptions
  parsed <- liftEffect $ handleArguments file args options
  let
    command = joinCommand file args
    escapedCommand = getEscapedCommand file args
  spawned <- liftEffect $ ChildProcess.spawn parsed.file parsed.args
    { cwd: options.cwd
    , env: options.env
    , argv0: options.argv0
    , stdioExtra: options.stdioExtra
    , detached: options.detached
    , uid: options.uid
    , gid: options.gid
    , serialization: Nothing
    , shell: options.shell
    , windowsVerbatimArguments: options.windowsVerbatimArguments
    , windowsHide: options.windowsHide
    , timeout: parsed.options.timeout
    , killSignal: parsed.options.killSignal
    }
  spawnedFiber <- suspendAff $ makeAff \cb -> do
    onExit spawned case _ of
      ChildProcess.ExitCode i -> do
        cb $ Right $ ExitCode i
      ChildProcess.SignalCode sig ->
        cb $ Right $ Killed sig
    onError spawned \error -> do
      cb $ Right $ SpawnError error

    Streams.onError (stdin spawned) \error ->
      cb $ Right $ StdinError error
    pure nonCanceler
  timeoutFiber <- suspendAff do
    case parsed.options.timeout, parsed.options.killSignal of
      Just milliseconds, Just killSignal -> do
        makeAff \cb -> do
          tid <- setTimeout ((unsafeCoerce :: Number -> Int) milliseconds) do
            void $ kill' killSignal { forceKillAfterTimeout: Nothing } spawned
            cb $ Right $ TimedOut killSignal
          pure $ effectCanceler do
            clearTimeout tid
      _, _ ->
        never

  mainFiber <- suspendAff do
    parOneOf
      [ joinFiber spawnedFiber
      , joinFiber timeoutFiber
      ]

  processDoneFiber <- do
    if not parsed.options.cleanup || parsed.options.detached then pure mainFiber
    else suspendAff do
      removeHandlerRef <- liftEffect $ Ref.new Nothing
      finally
        (liftEffect $ Ref.read removeHandlerRef >>= sequence_)
        ( do
            liftEffect do
              removal <- SignalExit.onExit \_ _ -> do
                void $ kill' (Right "SIGTERM") { forceKillAfterTimeout: Nothing } spawned
              Ref.write (Just removal) removeHandlerRef
            joinFiber mainFiber
        )

  liftEffect $ runEffectFn2 monkeyPatchKill spawned spawnedKill
  isCanceledRef <- liftEffect $ Ref.new false
  -- PureScript implementaton note:
  -- We don't need to `handleInput` because
  -- we force end-users to write to `stdin` via
  -- its `Stream` interface.
  let
    cancel = do
      killSucceeded <- kill spawned
      when killSucceeded do
        Ref.write true isCanceledRef

    bufferToString = ImmutableBuffer.toString parsed.options.encoding

    getSpawnResult = do
      { main: _, stdout: _, stderr: _ }
        <$> joinFiber processDoneFiber
        <*> getStreamBuffer (stdout spawned) { maxBuffer: Just parsed.options.maxBuffer }
        <*> getStreamBuffer (stderr spawned) { maxBuffer: Just parsed.options.maxBuffer }

  run <- suspendAff do
    result <- getSpawnResult
    let
      handleOutput' stream getStreamResult = liftEffect do
        buf <- handleOutput { stripFinalNewline: parsed.options.stripFinalNewline } getStreamResult.buffer
        when (isJust getStreamResult.inputError) do
          destroy stream
        pure buf
    stdout' <- bufferToString <$> handleOutput' (stdout spawned) result.stdout
    stderr' <- bufferToString <$> handleOutput' (stderr spawned) result.stderr
    case result.main, result.stdout.inputError, result.stderr.inputError of
      ExitCode 0, Nothing, Nothing -> do
        pure $ Right
          { command
          , escapedCommand
          , exitCode: 0
          , stdout: stdout'
          , stderr: stderr'
          }
      someError, stdoutErr, stderrErr -> liftEffect do
        isCanceled <- Ref.read isCanceledRef
        killed <- ChildProcess.killed spawned
        pure $ Left $ mkError
          { error: preview _SpawnError someError
          , stdinErr: preview _StdinError someError
          , stdoutErr
          , stderrErr
          , exitCode: preview _ExitCode someError
          , signal: preview _Killed someError <|> preview _TimedOut someError
          , stdout: stdout'
          , stderr: stderr'
          , command
          , escapedCommand
          , parsed
          , timedOut: is _TimedOut someError
          , isCanceled
          , killed
          }

  allStream <- suspendAff $ liftEffect do
    duplex <- MergeStreams.mergeStreams \iface -> do
      _ <- MergeStreams.add (stdout spawned) iface
      void $ MergeStreams.add (stderr spawned) iface
    pure
      { stream: duplex
      , result: do
          { buffer, inputError } <- getStreamBuffer duplex { maxBuffer: Just $ parsed.options.maxBuffer * 2.0 }
          pure { string: bufferToString buffer, inputError }
      }
  pure
    { childProcess: spawned
    , run
    , cancel
    , all: joinFiber allStream
    , writeStdin: \encoding str -> do
        liftEffect do
          buf <- Buffer.fromString str encoding
          void $ Stream.write (stdin spawned) buf mempty
    , writeCloseStdin: \encoding str -> do
        liftEffect do
          buf <- Buffer.fromString str encoding
          void $ Stream.write (stdin spawned) buf mempty
          void $ Stream.end (stdin spawned) mempty
    , closeStdin: do
        liftEffect do
          void $ Stream.end (stdin spawned) mempty
    }

-- | - `cleanup` (default: `true`): Kill the spawned process when the parent process exits unless either:
-- |    - the spawned process is `detached`
-- |    - the parent process is terminated abruptly, for example, with SIGKILL as opposed to SIGTERM or a normal exit
-- | `- preferLocal` (default: `Nothing`): When `Just`, includes and prefers locally-installed `node_modules/.bin` binaries
-- |   when looking for a binary to execute. In short, if you `npm install foo``, you can run `execa "foo"`.
-- |   `localDir` (if `Nothing`, `Process.cwd` is used) - Preferred path to find locally installed binaries in
-- |   `execPath` (if `Nothing`, `Process.execPath` is used) - Path to the Node.js executable to use in child processes. 
-- |      This can be either an absolute path or a path relative to the `localDir` option.
-- | - `stripFinalNewline` - (default: `true`). If enabled, trims the newline character of `stdout`/`stderr` (e.g. `/(?:/r/n)|\r|\n$/`
-- | - `extendEnv` (default: `true`) - Extends the child process' `env` with `Process.env`
-- | - `argv0` - see Node docs
-- | - `input` - When defined, the input is piped into the child's `stdin` and then `stdin` is `end`ed.
-- | - `stdioExtra` - Append any other `stdio` values to the array.
-- |    The `stdio` array used is always `["pipe", "pipe", "pipe", "ipc"] <> fromMaybe [] options.stdioExtra`
-- | - `detached` - see Node docs
-- | - `uid` - see Node docs
-- | - `gid` - see Node docs
-- | - `shell` - see Node docs. The Boolean variant is not supported
-- | - `timeout` - the amount of time to wait before killing the child process with the given kill signal
-- | - `maxBuffer` - the amount of buffer space available to `stdout`/`stderr`.
-- |    If more data is written to their buffers, child process will error with a max buffer size exceeded error.
-- | - `encoding` (default: `Just UTF8`) - the encoding to use to decode `stdout`/`stderr` to a String
-- | - `windowsVerbatimArguments` - see Node docs
-- | - `windowsHide` - see Node docs
-- | - `windowsEnableCmdEcho` (default: `true`) - Enables the `\q` flag when using the `cmd` shell. See https://github.com/nodejs/node/issues/27120
-- |    This goes against the Windows' defaults but makes the `stdout`/`stderr` behavior more consistent across different operating systems.
type ExecaSyncOptions =
  -- execa options
  { cleanup :: Maybe Boolean
  , preferLocal :: Maybe { localDir :: Maybe String, execPath :: Maybe String }
  , stripFinalNewline :: Maybe Boolean
  , extendEnv :: Maybe Boolean
  , cwd :: Maybe String
  , env :: Maybe (Object String)
  , argv0 :: Maybe String
  , input :: Maybe ImmutableBuffer
  , stdioExtra :: Maybe (Array Foreign)
  , detached :: Maybe Boolean
  , uid :: Maybe Int
  , gid :: Maybe Int
  , shell :: Maybe String
  , timeout :: Maybe { milliseconds :: Number, killSignal :: Either Int String }
  , maxBuffer :: Maybe Number
  , encoding :: Maybe Encoding
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  -- cross spawn options
  , windowsEnableCmdEcho :: Maybe Boolean
  }

defaultExecaSyncOptions :: ExecaSyncOptions
defaultExecaSyncOptions =
  { cleanup: Nothing
  , preferLocal: Nothing
  , stripFinalNewline: Nothing
  , extendEnv: Nothing
  , cwd: Nothing
  , env: Nothing
  , argv0: Nothing
  , input: Nothing
  , stdioExtra: Nothing
  , detached: Nothing
  , uid: Nothing
  , gid: Nothing
  , shell: Nothing
  , timeout: Nothing
  , maxBuffer: Nothing
  , encoding: Nothing
  , windowsVerbatimArguments: Nothing
  , windowsHide: Nothing
  , windowsEnableCmdEcho: Nothing
  }

-- | Replacement for `childProcess.spawnSync`. Override the default options
-- | using record update syntax. If defaults are good enough, just use `identity`.
-- | ```
-- | execaSync "jq" [ "-M", "--" ] (_ 
-- |    { input = Just $ ImmutableBuffer.fromString UTF8 """{ "json": 0, "array": ["my json"] }"""
-- |    })
-- |
-- | execaSync "jq" [ "-M", "path/to/some/file.json" ] identity
-- | ```
execaSync :: String -> Array String -> (ExecaSyncOptions -> ExecaSyncOptions) -> Effect (Either ExecaError ExecaSyncResult)
execaSync file args buildOptions = do
  let options = buildOptions defaultExecaSyncOptions
  parsed <- handleArguments file args $ Record.delete (Proxy :: _ "input") options
  let
    command = joinCommand file args
    escapedCommand = getEscapedCommand file args
  result <- spawnSync parsed.file parsed.args
    { cwd: Just parsed.options.cwd
    , input: options.input
    , argv0: parsed.options.argv0
    , stdioExtra: Just parsed.options.stdioExtra
    , env: Just parsed.options.env
    , uid: parsed.options.uid
    , gid: parsed.options.gid
    , timeout: parsed.options.timeout
    , killSignal: parsed.options.killSignal
    , maxBuffer: Just parsed.options.maxBuffer
    , shell: parsed.options.shell
    , windowsVerbatimArguments: Just parsed.options.windowsVerbatimArguments
    , windowsHide: Just parsed.options.windowsHide
    }
  let
    stripOption = fromMaybe true options.stripFinalNewline
    encoding = fromMaybe defaultOptions.encoding options.encoding
    bufferToString = ImmutableBuffer.toString encoding
  stdout' <- bufferToString <$> handleOutput { stripFinalNewline: stripOption } result.stdout
  stderr' <- bufferToString <$> handleOutput { stripFinalNewline: stripOption } result.stderr
  let
    resultError = toMaybe result.error
    resultSignal = map fromKillSignal $ toMaybe result.signal
    hasNonZeroExit = case toMaybe result.status of
      Just n | n /= 0 -> true
      _ -> false
  if isJust resultError || hasNonZeroExit || isJust resultSignal then
    pure $ Left $ mkError
      { command
      , escapedCommand
      , stdout: stdout'
      , stderr: stderr'
      , stdinErr: Nothing
      , stdoutErr: Nothing
      , stderrErr: Nothing
      , error: resultError
      , signal: resultSignal
      , exitCode: toMaybe result.status
      , parsed
      , timedOut: Just "ETIMEDOUT" == (map _.code resultError)
      , isCanceled: false
      , killed: isJust resultSignal
      }
  else do
    pure $ Right
      { command
      , escapedCommand
      , stdout: stdout'
      , stderr: stderr'
      , exitCode: 0
      }

type ExecaSyncResult =
  { command :: String
  , escapedCommand :: String
  , exitCode :: Int
  , stdout :: String
  , stderr :: String
  }

handleOutput :: forall r. { stripFinalNewline :: Boolean | r } -> ImmutableBuffer -> Effect ImmutableBuffer
handleOutput options value
  | options.stripFinalNewline =
      unsafeThaw value >>= stripFinalNewlineBuf
  | otherwise = pure value

-- Handle `execaCommand()`
parseCommand :: String -> Maybe { file :: String, args :: Array String }
parseCommand command = do
  let
    initialTokens = Regex.split spacesRegex $ String.trim command
    result = initialTokens # flip Array.foldl { previousToken: Nothing, tokens: [] } \acc token ->
      case acc.previousToken of
        Nothing ->
          acc { previousToken = Just token }
        Just prevTok
          -- Allow spaces to be escaped by a backslash if not meant as a delimiter
          | Just tokNoSlash <- String.stripSuffix (String.Pattern "\\") prevTok ->
              acc { previousToken = Just $ tokNoSlash <> " " <> token }
          | otherwise ->
              { previousToken: Just token, tokens: acc.tokens `Array.snoc` prevTok }
    tokens = maybe result.tokens (Array.snoc result.tokens) result.previousToken
  case Array.uncons tokens of
    Just { head, tail } -> Just { file: head, args: tail }
    _ -> Nothing

joinCommand :: String -> Array String -> String
joinCommand file args = file <> " " <> Array.intercalate " " args

getEscapedCommand :: String -> Array String -> String
getEscapedCommand file args = do
  Array.intercalate " " $ map escapeArg $ Array.cons file args
  where
  escapeArg arg
    | test noEscapeRegex arg = arg
    | otherwise = "\"" <> (Regex.replace doubleQuotesregex ("\\" <> "\"") arg) <> "\""

data SpawnResult
  = ExitCode Int
  | Killed (Either Int String)
  | SpawnError ChildProcess.Error
  | StdinError Error
  | TimedOut (Either Int String)

_ExitCode :: Prism' SpawnResult Int
_ExitCode = prism ExitCode case _ of
  ExitCode i -> Right i
  other -> Left other

_Killed :: Prism' SpawnResult (Either Int String)
_Killed = prism Killed case _ of
  Killed sig -> Right sig
  other -> Left other

_SpawnError :: Prism' SpawnResult ChildProcess.Error
_SpawnError = prism SpawnError case _ of
  SpawnError a -> Right a
  other -> Left other

_StdinError :: Prism' SpawnResult Error
_StdinError = prism StdinError case _ of
  StdinError a -> Right a
  other -> Left other

_TimedOut :: Prism' SpawnResult (Either Int String)
_TimedOut = prism TimedOut case _ of
  TimedOut a -> Right a
  other -> Left other

-- | `/^[\w.-]+$/`
noEscapeRegex ∷ Regex
noEscapeRegex = unsafeRegex """^[\w.-]+$""" noFlags

-- | `/"/g`
doubleQuotesregex ∷ Regex
doubleQuotesregex = unsafeRegex "\"" global

-- | `/ +/g`
spacesRegex ∷ Regex
spacesRegex = unsafeRegex " +" global

spawnedKill
  :: EffectFn3
       (EffectFn1 KillSignal Boolean)
       (Nullable KillSignal)
       { forceKillAfterTimeout :: Maybe Milliseconds }
       Boolean
spawnedKill = mkEffectFn3 \killFn numOrStringSignal options -> do
  let
    signal = case toMaybe numOrStringSignal of
      Nothing -> Right "SIGTERM"
      Just numOrStr -> fromKillSignal numOrStr
  killSignalSucceeded <- runEffectFn1 killFn $ either intKillSignal stringKillSignal signal
  let
    mbTimeout = do
      guard $ isSigTerm signal
      guard killSignalSucceeded
      options.forceKillAfterTimeout
  for_ mbTimeout \(Milliseconds timeout) -> do
    t <- runEffectFn2 setTimeoutImpl (floor timeout) do
      void $ runEffectFn1 killFn $ stringKillSignal "SIGKILL"
    t.unref
  pure killSignalSucceeded
  where
  isSigTerm :: Either Int String -> Boolean
  isSigTerm = case _ of
    Left i -> maybe false (eq "SIGTERM" <<< String.toUpper <<< _.name) $ Map.lookup i signals.byNumber
    Right s -> eq "SIGTERM" $ String.toUpper s

foreign import monkeyPatchKill
  :: EffectFn2
       ChildProcess
       ( EffectFn3
           (EffectFn1 KillSignal Boolean)
           (Nullable KillSignal)
           { forceKillAfterTimeout :: Maybe Milliseconds }
           Boolean
       )
       Unit

foreign import setTimeoutImpl :: EffectFn2 Int (Effect Unit) { unref :: Effect Unit }

type ExecaRunOptions =
  -- execa options
  { cleanup :: Boolean
  , stdioExtra :: Array Foreign
  , stripFinalNewline :: Boolean
  , encoding :: Encoding
  -- child process spawn options:
  , cwd :: String
  , env :: Object String -- 
  , argv0 :: Maybe String
  , detached :: Boolean
  , uid :: Maybe Int
  , gid :: Maybe Int
  , shell :: Maybe String
  , timeout :: Maybe Number
  , killSignal :: Maybe (Either Int String)
  , maxBuffer :: Number
  , windowsVerbatimArguments :: Boolean
  , windowsHide :: Boolean
  }

type ExecaError =
  { originalMessage :: Maybe String
  , message :: String
  , shortMessage :: String
  , escapedCommand :: String
  , exitCode :: Maybe Int
  , signal :: Maybe (Either Int String)
  , signalDescription :: Maybe String
  , stdout :: String
  , stderr :: String
  , failed :: Boolean
  , timedOut :: Boolean
  , isCanceled :: Boolean
  , killed :: Boolean
  }

mkError
  :: { stdout :: String
     , stderr :: String
     , error :: Maybe ChildProcess.Error
     , stdinErr :: Maybe Exception.Error
     , stdoutErr :: Maybe Exception.Error
     , stderrErr :: Maybe Exception.Error
     , signal :: Maybe (Either Int String)
     , exitCode :: Maybe Int
     , command :: String
     , escapedCommand :: String
     , parsed :: { file :: String, args :: Array String, options :: ExecaRunOptions, parsed :: CrossSpawnConfig }
     , timedOut :: Boolean
     , isCanceled :: Boolean
     , killed :: Boolean
     }
  -> ExecaError
mkError { stdout, stderr, error, stdinErr, stdoutErr, stderrErr, signal, exitCode, command, escapedCommand, parsed, timedOut, isCanceled, killed } =
  { originalMessage: (error >>= _.message >>> toMaybe) <|> (map Exception.message $ stdinErr <|> stdoutErr <|> stderrErr)
  , message
  , shortMessage
  , escapedCommand
  , exitCode
  , signal
  , signalDescription
  , stdout
  , stderr
  , failed: true
  , timedOut: timedOut
  , isCanceled: isCanceled
  , killed: killed && not timedOut
  }
  where
  signalDescription = signal >>= case _ of
    Left i -> map _.description $ Map.lookup i signals.byNumber
    Right s -> map _.description $ Object.lookup s signals.byString
  errorCode = map _.code error
  prefix
    | timedOut
    , Just timeout <- parsed.options.timeout =
        "timed out after " <> show timeout <> "milliseconds"
    | isCanceled =
        "was canceled"
    | Just code <- errorCode =
        "failed with " <> code
    | Just signal' <- signal
    , Just description <- signalDescription =
        "was killed with " <> show signal' <> " (" <> description <> ")"
    | Just exit <- exitCode =
        "failed with exit code " <> show exit
    | Just err <- stdinErr =
        "had error in `stdin`: " <> Exception.message err
    | Just err <- stdoutErr =
        "had error in `stdout`: " <> Exception.message err
    | Just err <- stderrErr =
        "had error in `stderr`: " <> Exception.message err
    | otherwise =
        "failed"
  execaMessage = "Command " <> prefix <> ": " <> command
  shortMessage = execaMessage <> (maybe "" (append "\n") $ (toMaybe <<< _.message) =<< error)
  message = Array.intercalate "\n"
    [ shortMessage
    , stderr
    , stdout
    ]

-- | Replacement for `childProcess.exec`. Override the default options
-- | using record update syntax. If defaults are good enough, just use `identity`.
-- | ```
-- | execaCommand "git checkout -b my-branch"
-- |    { cwd = Just $ Path.concat [ "some", "other", "directory"]
-- |    })
-- |
-- | execaCommand "git checkout -b my-branch" identity
-- | ```
execaCommand :: String -> (ExecaOptions -> ExecaOptions) -> Aff ExecaResult
execaCommand s buildOptions = do
  case parseCommand s of
    Just { file, args } ->
      execa file args buildOptions
    Nothing ->
      liftEffect $ throw $ "Command " <> show s <> " could not be parsed into `{ file :: String, args :: Array String }` value."

-- | Replacement for `childProcess.execSync`. Override the default options
-- | using record update syntax. If defaults are good enough, just use `identity`.
-- | Note: this will throw an error if the string does not contain
-- | a valid command.
-- | ```
-- | execaCommandSync "git checkout -b my-branch" (_
-- |    { cwd = Just $ Path.concat [ "some", "other", "directory"]
-- |    })
-- |
-- | execaCommandSync "git checkout -b my-branch" identity
-- | ```
execaCommandSync :: String -> (ExecaSyncOptions -> ExecaSyncOptions) -> Effect (Either ExecaError ExecaSyncResult)
execaCommandSync s buildOptions = do
  case parseCommand s of
    Just { file, args } ->
      execaSync file args buildOptions
    Nothing ->
      liftEffect $ throw $ "Command " <> show s <> " could not be parsed into `{ file :: String, args :: Array String }` value."
