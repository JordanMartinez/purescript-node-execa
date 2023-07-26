-- | Provides a higher-level replacement to Node.js `child_process` module.
-- | Uses sane defaults with clearer error messages.
-- | - `spawn`/`spawnSync` -> `execa`/`execaSync`
-- | - `exec`/`execSync` -> `execaCommand`/`execaCommandSync`
-- | - `fork` - has no equivalent
module Node.Library.Execa
  ( ExecaError
  , ExecaOptions
  , ExecaProcess
  , ExecaSuccess
  , ExecaChildProcess
  , toChildProcess
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
import Data.Posix (Gid, Pid, Uid)
import Data.String as String
import Data.String.Regex (Regex, test)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (global, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds(..), effectCanceler, finally, forkAff, joinFiber, makeAff, never, suspendAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.ChildProcess (ChildProcess, kill')
import Node.ChildProcess as CP
import Node.ChildProcess.Types (Exit(..), KillSignal, StdIO, customShell, fromKillSignal, fromKillSignal', stringSignal)
import Node.Encoding (Encoding(..))
import Node.Errors.SystemError (SystemError)
import Node.Errors.SystemError as SystemError
import Node.EventEmitter (on, once)
import Node.Library.Execa.CrossSpawn (CrossSpawnConfig)
import Node.Library.Execa.CrossSpawn as CrossSpawn
import Node.Library.Execa.GetStream (getStreamBuffer)
import Node.Library.Execa.NpmRunPath (defaultNpmRunPathOptions, npmRunPathEnv)
import Node.Library.Execa.ParseCommand (parseCommand)
import Node.Library.Execa.SignalExit as SignalExit
import Node.Library.Execa.StripFinalNewline (stripFinalNewlineBuf)
import Node.Library.HumanSignals (signals)
import Node.Process as Process
import Node.Stream (Readable, Writable, destroy)
import Node.Stream as Stream
import Node.UnsafeChildProcess.Unsafe as Unsafe
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
  , stdioExtra :: Maybe (Array StdIO)
  , detached :: Maybe Boolean
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  , shell :: Maybe String
  , timeout :: Maybe { milliseconds :: Milliseconds, killSignal :: KillSignal }
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
  -> Effect
       { file :: String
       , args :: Array String
       , options :: ExecaRunOptions
       , parsed :: CrossSpawnConfig
       }
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
    { timeout, killSignal, timeoutWithKillSignal } = case initOptions.timeout of
      Just r | r.milliseconds > Milliseconds 0.0 ->
        { timeout: Just r.milliseconds, killSignal: Just r.killSignal, timeoutWithKillSignal: Just r }
      _ -> { timeout: Nothing, killSignal: Nothing, timeoutWithKillSignal: Nothing }

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
      , timeoutWithKillSignal
      , windowsHide: fromMaybe defaultOptions.windowsHide initOptions.windowsHide
      , windowsVerbatimArguments: fromMaybe defaultOptions.windowsVerbatimArguments parsed.options.windowsVerbatimArguments
      }
  pure { file: parsed.command, args: parsed.args, options, parsed }

-- | Re-exposes all the bindings for `ChildProcess`.
-- | In addition exposes, the following:
-- | `result` - gets the result of the process
-- | `cancel` - kill the child process, but indicate it was cancelled rather than killed in the error message
-- | `stdin.stream` - access the child process' `stdin`
-- | `stdin.writeUt8` - Write a string to the child process' `stdin`
-- | `stdin.writeUt8End` - Write a string to the child process' `stdin` and then `end` the stream
-- | `stdin.end` - End the child process' `stdin`
type ExecaProcess =
  { cancel :: Aff Unit
  , unsafeChannelRef :: Aff Unit
  , unsafeChannelUnref :: Aff Unit
  , connected :: Aff Boolean
  , disconnect :: Aff Unit
  , exitCode :: Aff (Maybe Int)
  , kill :: Aff Boolean
  , killForced :: Milliseconds -> Aff Boolean
  , killForcedWithSignal :: KillSignal -> Milliseconds -> Aff Boolean
  , killWithSignal :: KillSignal -> Aff Boolean
  , killed :: Aff Boolean
  , childProcess :: ExecaChildProcess
  , pid :: Aff (Maybe Pid)
  , pidExists :: Aff Boolean
  , ref :: Aff Unit
  , result :: Aff (Either ExecaError ExecaSuccess)
  , signalCode :: Aff (Maybe String)
  , spawnArgs :: Array String
  , spawnFile :: String
  , stdin ::
      { stream :: Writable ()
      , writeUtf8 :: String -> Aff Unit
      , writeUtf8End :: String -> Aff Unit
      , end :: Aff Unit
      , pipeFromParentProcessStdin :: Aff Unit
      }
  , stdout ::
      { stream :: Readable ()
      , output :: Aff { text :: String, error :: Maybe Exception.Error }
      , pipeToParentStdout :: Aff Unit
      }
  , stderr ::
      { stream :: Readable ()
      , output :: Aff { text :: String, error :: Maybe Exception.Error }
      , pipeToParentStderr :: Aff Unit
      }
  , unref :: Aff Unit
  }

type ExecaSuccess =
  { command :: String
  , escapedCommand :: String
  , exitCode :: Int
  , stderr :: String
  , stdout :: String
  }

newtype ExecaChildProcess = ExecaChildProcess CP.ChildProcess

toChildProcess :: ExecaChildProcess -> ChildProcess
toChildProcess (ExecaChildProcess cp) = cp

-- | Replacement for `childProcess.spawn`. Since this is asynchronous,
-- | the returned value will not provide any results until one calls `spawned.result`:
-- | `execa ... >>= \spawned -> spawned.result`. 
-- |
-- | Override the default options using record update syntax.
-- | If defaults are good enough, just use `identity`.
-- | ```
-- | spawned <- execa "git checkout -b my-branch" (_
-- |    { cwd = Just $ Path.concat [ "some", "other", "directory"]
-- |    })
-- | spawned.result
-- |
-- | spawned2 <- execa "git checkout -b my-branch" identity
-- | spawned2.result
-- | ```
execa :: String -> Array String -> (ExecaOptions -> ExecaOptions) -> Aff ExecaProcess
execa file args buildOptions = do
  let options = buildOptions defaultExecaOptions
  parsed <- liftEffect $ handleArguments file args options
  let
    command = joinCommand file args
    escapedCommand = getEscapedCommand file args
  spawned <- liftEffect $ CP.spawn' parsed.file parsed.args
    ( _
        { cwd = options.cwd
        , env = options.env
        , argv0 = options.argv0
        , appendStdio = options.stdioExtra
        , detached = options.detached
        , uid = options.uid
        , gid = options.gid
        , serialization = Nothing
        , shell = map customShell options.shell
        , windowsVerbatimArguments = options.windowsVerbatimArguments
        , windowsHide = options.windowsHide
        }
    )
  spawnedFiber <- suspendAff $ makeAff \cb -> do
    exitRef <- Ref.new mempty
    errorRef <- Ref.new mempty
    streamRef <- Ref.new mempty
    let
      removeListeners = do
        join $ Ref.read exitRef
        join $ Ref.read errorRef
        join $ Ref.read streamRef
    rmExit <- spawned # once CP.exitH \res -> do
      removeListeners
      cb $ Right case res of
        Normally i -> ExitCode i
        BySignal sig -> Killed sig
    Ref.write rmExit exitRef

    rmError <- spawned # once CP.errorH \error -> do
      removeListeners
      cb $ Right $ SpawnError error
    Ref.write rmError errorRef

    rmStream <- (CP.stdin spawned) # on Stream.errorH \error -> do
      removeListeners
      cb $ Right $ StdinError error
    Ref.write rmStream streamRef
    pure $ effectCanceler removeListeners
  timeoutFiber <- suspendAff do
    case parsed.options.timeoutWithKillSignal of
      Just { milliseconds, killSignal: signal } -> do
        makeAff \cb -> do
          tid <- setTimeout ((unsafeCoerce :: Milliseconds -> Int) milliseconds) do
            void $ execaKill (Just signal) Nothing spawned
            void $ destroy (CP.stdin spawned)
            void $ destroy (CP.stdout spawned)
            void $ destroy (CP.stderr spawned)
            cb $ Right $ TimedOut signal
          pure $ effectCanceler do
            clearTimeout tid
      _ ->
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
                void $ execaKill (Just $ stringSignal "SIGTERM") Nothing spawned
              Ref.write (Just removal) removeHandlerRef
            joinFiber mainFiber
        )

  isCanceledRef <- liftEffect $ Ref.new false
  -- PureScript implementaton note:
  -- We don't need to `handleInput` because
  -- we force end-users to write to `stdin` via
  -- its `Stream` interface.
  let
    cancel :: Aff Unit
    cancel = liftEffect do
      killSucceeded <- execaKill (Just $ stringSignal "SIGTERM") Nothing spawned
      when killSucceeded do
        Ref.write true isCanceledRef

    bufferToString = Buffer.toString parsed.options.encoding

    mkStdIoFiber stream = forkAff do
      streamResult <- getStreamBuffer stream { maxBuffer: Just parsed.options.maxBuffer }
      text <- liftEffect do
        text <- bufferToString =<< handleOutput { stripFinalNewline: parsed.options.stripFinalNewline } streamResult.buffer
        when (isJust streamResult.inputError) do
          destroy stream
        pure text
      pure { text, error: streamResult.inputError }

  runFiber <- forkAff $ joinFiber processDoneFiber
  stdoutFiber <- mkStdIoFiber (CP.stdout spawned)
  stderrFiber <- mkStdIoFiber (CP.stderr spawned)

  let
    getSpawnResult = do
      { main: _, stdout: _, stderr: _ }
        <$> joinFiber runFiber
        <*> joinFiber stdoutFiber
        <*> joinFiber stderrFiber

  run <- forkAff do
    result <- getSpawnResult
    case result.main, result.stdout.error, result.stderr.error of
      ExitCode 0, Nothing, Nothing -> do
        pure $ Right
          { command
          , escapedCommand
          , exitCode: 0
          , stdout: result.stdout.text
          , stderr: result.stderr.text
          }
      someError, stdoutErr, stderrErr -> liftEffect do
        isCanceled <- Ref.read isCanceledRef
        killed' <- CP.killed spawned
        pure $ Left $ mkError
          { error: preview _SpawnError someError
          , stdinErr: preview _StdinError someError
          , stdoutErr
          , stderrErr
          , exitCode: preview _ExitCode someError
          , signal: preview _Killed someError <|> preview _TimedOut someError
          , stdout: result.stdout.text
          , stderr: result.stderr.text
          , command
          , escapedCommand
          , execaOptions: parsed.options
          , timedOut: is _TimedOut someError
          , isCanceled
          , killed: killed'
          }

  pure
    { unsafeChannelRef: liftEffect $ Unsafe.unsafeChannelRef $ CP.toUnsafeChildProcess spawned
    , unsafeChannelUnref: liftEffect $ Unsafe.unsafeChannelUnref $ CP.toUnsafeChildProcess spawned
    , connected: liftEffect $ CP.connected spawned
    , disconnect: liftEffect $ CP.disconnect spawned
    , exitCode: liftEffect $ CP.exitCode spawned
    , kill: liftEffect $ execaKill (Just $ stringSignal "SIGTERM") Nothing spawned
    , killWithSignal: \signal -> liftEffect do
        execaKill (Just signal) Nothing spawned
    , killForced: \forceKillAfterTimeout -> liftEffect do
        execaKill (Just $ stringSignal "SIGTERM") (Just forceKillAfterTimeout) spawned
    , killForcedWithSignal: \signal forceKillAfterTimeout -> liftEffect do
        execaKill (Just signal) (Just forceKillAfterTimeout) spawned
    , pidExists: liftEffect $ CP.pidExists spawned
    , killed: liftEffect $ CP.killed spawned
    , pid: liftEffect $ CP.pid spawned
    , unref: liftEffect $ CP.unref spawned
    , ref: liftEffect $ CP.ref spawned
    , signalCode: liftEffect $ CP.signalCode spawned
    , spawnArgs: CP.spawnArgs spawned
    , spawnFile: CP.spawnFile spawned
    , childProcess: ExecaChildProcess spawned
    , stdin:
        { stream: CP.stdin spawned
        , writeUtf8: \string -> liftEffect do
            buf <- Buffer.fromString string UTF8
            void $ Stream.write (CP.stdin spawned) buf
        , writeUtf8End: \string -> liftEffect do
            buf <- Buffer.fromString string UTF8
            void $ Stream.write (CP.stdin spawned) buf
            void $ Stream.end (CP.stdin spawned)
        , end: liftEffect do
            void $ Stream.end (CP.stdin spawned)
        , pipeFromParentProcessStdin: liftEffect do
            void $ Stream.pipe Process.stdin (CP.stdin spawned)
        }
    , stdout:
        { stream: CP.stdout spawned
        , output: joinFiber stdoutFiber
        , pipeToParentStdout: liftEffect do
            void $ Stream.pipe (CP.stdout spawned) Process.stdout
        }
    , stderr:
        { stream: CP.stderr spawned
        , output: joinFiber stderrFiber
        , pipeToParentStderr: liftEffect do
            void $ Stream.pipe (CP.stderr spawned) Process.stderr
        }
    , cancel
    , result: joinFiber run
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
  , input :: Maybe Buffer
  , stdioExtra :: Maybe (Array StdIO)
  , detached :: Maybe Boolean
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  , shell :: Maybe String
  , timeout :: Maybe { milliseconds :: Milliseconds, killSignal :: KillSignal }
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
  result <- CP.spawnSync' parsed.file parsed.args
    ( _
        { cwd = Just parsed.options.cwd
        , input = options.input
        , argv0 = parsed.options.argv0
        , appendStdio = Just parsed.options.stdioExtra
        , env = Just parsed.options.env
        , uid = parsed.options.uid
        , gid = parsed.options.gid
        , timeout = parsed.options.timeout
        , killSignal = parsed.options.killSignal
        , maxBuffer = Just parsed.options.maxBuffer
        , shell = map customShell parsed.options.shell
        , windowsVerbatimArguments = Just parsed.options.windowsVerbatimArguments
        , windowsHide = Just parsed.options.windowsHide
        }
    )
  let
    stripOption = fromMaybe true options.stripFinalNewline
    encoding = fromMaybe defaultOptions.encoding options.encoding
    bufferToString = Buffer.toString encoding
  stdout' <- bufferToString =<< handleOutput { stripFinalNewline: stripOption } result.stdout
  stderr' <- bufferToString =<< handleOutput { stripFinalNewline: stripOption } result.stderr
  let
    resultSignal = case result.exitStatus of
      BySignal s -> Just s
      _ -> Nothing
    resultExitCode = case result.exitStatus of
      Normally n -> Just n
      _ -> Nothing
    hasNonZeroExit = case result.exitStatus of
      Normally n | n /= 0 -> true
      _ -> false
  if isJust result.error || hasNonZeroExit || isJust resultSignal then
    pure $ Left $ mkError
      { command
      , escapedCommand
      , stdout: stdout'
      , stderr: stderr'
      , stdinErr: Nothing
      , stdoutErr: Nothing
      , stderrErr: Nothing
      , error: result.error
      , signal: resultSignal
      , exitCode: resultExitCode
      , execaOptions: parsed.options
      , timedOut: Just "ETIMEDOUT" == (map SystemError.code result.error)
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

handleOutput :: forall r. { stripFinalNewline :: Boolean | r } -> Buffer -> Effect Buffer
handleOutput options value
  | options.stripFinalNewline = stripFinalNewlineBuf value
  | otherwise = pure value

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
  | Killed KillSignal
  | SpawnError SystemError
  | StdinError Error
  | TimedOut KillSignal

_ExitCode :: Prism' SpawnResult Int
_ExitCode = prism ExitCode case _ of
  ExitCode i -> Right i
  other -> Left other

_Killed :: Prism' SpawnResult KillSignal
_Killed = prism Killed case _ of
  Killed sig -> Right sig
  other -> Left other

_SpawnError :: Prism' SpawnResult SystemError
_SpawnError = prism SpawnError case _ of
  SpawnError a -> Right a
  other -> Left other

_StdinError :: Prism' SpawnResult Error
_StdinError = prism StdinError case _ of
  StdinError a -> Right a
  other -> Left other

_TimedOut :: Prism' SpawnResult KillSignal
_TimedOut = prism TimedOut case _ of
  TimedOut a -> Right a
  other -> Left other

-- | `/^[\w.-]+$/`
noEscapeRegex ∷ Regex
noEscapeRegex = unsafeRegex """^[\w.-]+$""" noFlags

-- | `/"/g`
doubleQuotesregex ∷ Regex
doubleQuotesregex = unsafeRegex "\"" global

execaKill
  :: Maybe KillSignal
  -> Maybe Milliseconds
  -> ChildProcess
  -> Effect Boolean
execaKill mbKillSignal forceKillAfterTimeout cp = do
  let
    killSignal = fromMaybe (stringSignal "SIGTERM") mbKillSignal
  killSignalSucceeded <- kill' killSignal cp
  let
    mbTimeout = do
      guard $ isSigTerm killSignal
      guard killSignalSucceeded
      forceKillAfterTimeout
  for_ mbTimeout \(Milliseconds timeout) -> do
    t <- runEffectFn2 setTimeoutImpl (floor timeout) do
      void $ kill' (stringSignal "SIGKILL") cp
    t.unref
  pure killSignalSucceeded
  where
  isSigTerm :: KillSignal -> Boolean
  isSigTerm = fromKillSignal'
    (\i -> maybe false (eq "SIGTERM" <<< String.toUpper <<< _.name) $ Map.lookup i signals.byNumber)
    (\s -> eq "SIGTERM" $ String.toUpper s)

foreign import setTimeoutImpl :: EffectFn2 Int (Effect Unit) { unref :: Effect Unit }

type ExecaRunOptions =
  -- execa options
  { cleanup :: Boolean
  , stdioExtra :: Array StdIO
  , stripFinalNewline :: Boolean
  , encoding :: Encoding
  -- child process spawn options:
  , cwd :: String
  , env :: Object String -- 
  , argv0 :: Maybe String
  , detached :: Boolean
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  , shell :: Maybe String
  , timeout :: Maybe Milliseconds
  , killSignal :: Maybe KillSignal
  , timeoutWithKillSignal :: Maybe { milliseconds :: Milliseconds, killSignal :: KillSignal }
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
  , signal :: Maybe KillSignal
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
     , error :: Maybe SystemError
     , stdinErr :: Maybe Exception.Error
     , stdoutErr :: Maybe Exception.Error
     , stderrErr :: Maybe Exception.Error
     , signal :: Maybe KillSignal
     , exitCode :: Maybe Int
     , command :: String
     , escapedCommand :: String
     , execaOptions :: ExecaRunOptions
     , timedOut :: Boolean
     , isCanceled :: Boolean
     , killed :: Boolean
     }
  -> ExecaError
mkError r =
  { originalMessage: (r.error <#> SystemError.message) <|> (map Exception.message $ r.stdinErr <|> r.stdoutErr <|> r.stderrErr)
  , message
  , shortMessage
  , escapedCommand: r.escapedCommand
  , exitCode: r.exitCode
  , signal: r.signal
  , signalDescription
  , stdout: r.stdout
  , stderr: r.stderr
  , failed: true
  , timedOut: r.timedOut
  , isCanceled: r.isCanceled
  , killed: r.killed && not r.timedOut
  }
  where
  signalDescription = r.signal >>= fromKillSignal >>> case _ of
    Left i -> map _.description $ Map.lookup i signals.byNumber
    Right s -> map _.description $ Object.lookup s signals.byString
  errorCode = map SystemError.code r.error
  prefix
    | r.timedOut
    , Just timeout <- r.execaOptions.timeout =
        "timed out after " <> show timeout <> "milliseconds"
    | r.isCanceled =
        "was canceled"
    | Just code <- errorCode =
        "failed with " <> code
    | Just signal' <- r.signal
    , Just description <- signalDescription =
        "was killed with " <> (either show show $ fromKillSignal signal') <> " (" <> description <> ")"
    | Just exit <- r.exitCode =
        "failed with exit code " <> show exit
    | Just err <- r.stdinErr =
        "had error in `stdin`: " <> Exception.message err
    | Just err <- r.stdoutErr =
        "had error in `stdout`: " <> Exception.message err
    | Just err <- r.stderrErr =
        "had error in `stderr`: " <> Exception.message err
    | otherwise =
        "failed"
  execaMessage = "Command " <> prefix <> ": " <> r.command
  shortMessage = execaMessage <> (maybe "" (append "\n") $ map SystemError.message r.error)
  message = Array.intercalate "\n"
    [ shortMessage
    , r.stderr
    , r.stdout
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
execaCommand :: String -> (ExecaOptions -> ExecaOptions) -> Aff ExecaProcess
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
