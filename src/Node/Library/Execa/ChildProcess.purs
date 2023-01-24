module Node.Library.Execa.ChildProcess
  ( Handle
  , ChildProcess
  , stdin
  , stdout
  , stderr
  , stdio
  , onSpawn
  , KillSignal
  , intKillSignal
  , stringKillSignal
  , fromKillSignal
  , Error
  , toStandardError
  , onError
  , Exit(..)
  , onExit
  , onClose
  , onDisconnect
  , onMessage
  , SerializationOption(..)
  , spawn
  , SpawnOptions
  , JsSpawnSyncResult
  , spawnSync
  , SpawnSyncOptions
  , spawnFile
  , spawnArgs
  , pid
  , exitCode
  , signalCode
  , connected
  , killed
  , kill
  , kill'
  , channel
  , ref
  , unref
  , pidExists
  , send
  , disconnect
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Posix (Pid)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception as Exception
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn5, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn5)
import Foreign (Foreign, readInt, readString, renderForeignError, unsafeToForeign)
import Foreign.Object (Object)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Stream (Writable, Readable)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

-- | A handle for inter-process communication (IPC).
foreign import data Handle :: Type

-- | Opaque type returned by `spawn`.
-- | Needed as input for most methods in this module.
foreign import data ChildProcess :: Type

channel
  :: ChildProcess
  -> Effect (Maybe { ref :: Effect Unit, unref :: Effect Unit })
channel cp = toMaybe <$> runEffectFn1 channelImpl cp

foreign import channelImpl :: EffectFn1 (ChildProcess) (Nullable { ref :: Effect Unit, unref :: Effect Unit })

-- | Indicates whether it is still possible to send and receive
-- | messages from the child process.
connected
  :: ChildProcess
  -> Effect Boolean
connected cp = runEffectFn1 connectedImpl cp

foreign import connectedImpl :: EffectFn1 (ChildProcess) Boolean

-- | Closes the IPC channel between parent and child.
disconnect :: ChildProcess -> Effect Unit
disconnect cp = runEffectFn1 disconnectImpl cp

foreign import disconnectImpl :: EffectFn1 (ChildProcess) Unit

exitCode :: ChildProcess -> Effect (Maybe Int)
exitCode cp = toMaybe <$> runEffectFn1 exitCodeImpl cp

foreign import exitCodeImpl :: EffectFn1 (ChildProcess) (Nullable Int)

-- | Same as `kill' SIGTERM`
kill :: ChildProcess -> Effect Boolean
kill = kill' (Right "SIGTERM") { forceKillAfterTimeout: Nothing }

-- | Send a signal to a child process. In the same way as the
-- | [unix kill(2) system call](https://linux.die.net/man/2/kill),
-- | sending a signal to a child process won't necessarily kill it.
-- |
-- | The resulting effects of this function depend on the process
-- | and the signal. They can vary from system to system.
-- | The child process might emit an `"error"` event if the signal
-- | could not be delivered.
-- |
-- | If `forceKillAfterTimeout` is defined and
-- | the kill signal was successful, `childProcess.kill "SIGKILL"`
-- | will be called once the timeout is reached.
kill' :: Either Int String -> { forceKillAfterTimeout :: Maybe Milliseconds } -> ChildProcess -> Effect Boolean
kill' sig options cp = runEffectFn3 killImpl cp (either intKillSignal stringKillSignal sig) options

foreign import killImpl :: EffectFn3 (ChildProcess) KillSignal { forceKillAfterTimeout :: Maybe Milliseconds } Boolean

pidExists :: ChildProcess -> Effect Boolean
pidExists cp = runEffectFn1 pidExistsImpl cp

foreign import pidExistsImpl :: EffectFn1 (ChildProcess) Boolean

killed :: ChildProcess -> Effect Boolean
killed cp = runEffectFn1 killedImpl cp

foreign import killedImpl :: EffectFn1 (ChildProcess) Boolean

-- | The process ID of a child process. Note that if the process has already
-- | exited, another process may have taken the same ID, so be careful!
pid :: ChildProcess -> Effect (Maybe Pid)
pid cp = toMaybe <$> runEffectFn1 pidImpl cp

foreign import pidImpl :: EffectFn1 (ChildProcess) (Nullable Pid)

ref :: ChildProcess -> Effect Unit
ref cp = runEffectFn1 refImpl cp

foreign import refImpl :: EffectFn1 (ChildProcess) Unit

unref :: ChildProcess -> Effect Unit
unref cp = runEffectFn1 unrefImpl cp

foreign import unrefImpl :: EffectFn1 (ChildProcess) Unit

type SendOptions =
  { keepOpen :: Maybe Boolean
  }

type JsSendOptions =
  { keepOpen :: Boolean
  }

-- | Send messages to the (`nodejs`) child process.
-- |
-- | See the [node documentation](https://nodejs.org/api/child_process.html#child_process_subprocess_send_message_sendhandle_options_callback)
-- | for in-depth documentation.
send
  :: ChildProcess
  -> Foreign
  -> Handle
  -> (SendOptions -> SendOptions)
  -> Effect Unit
  -> Effect Boolean
send cp msg handle buildOptions cb = runEffectFn5 sendImpl cp msg handle jsOptions cb
  where
  options = buildOptions { keepOpen: Nothing }
  jsOptions = { keepOpen: fromMaybe undefined options.keepOpen }

foreign import sendImpl :: EffectFn5 (ChildProcess) (Foreign) (Handle) (JsSendOptions) (Effect Unit) (Boolean)

signalCode
  :: ChildProcess
  -> Effect (Maybe String)
signalCode cp = map toMaybe $ runEffectFn1 signalCodeImpl cp

foreign import signalCodeImpl :: EffectFn1 (ChildProcess) (Nullable String)

foreign import spawnArgs :: ChildProcess -> Array String

foreign import spawnFile :: ChildProcess -> String

-- | The standard input stream of a child process.
foreign import stdin :: ChildProcess -> Writable ()

stdio :: ChildProcess -> Effect (Array Foreign)
stdio cp = runEffectFn1 stdioImpl cp

foreign import stdioImpl :: EffectFn1 (ChildProcess) (Array Foreign)

-- | The standard output stream of a child process.
foreign import stdout :: ChildProcess -> Readable ()

-- | The standard error stream of a child process.
foreign import stderr :: ChildProcess -> Readable ()

-- | Handle the `"close"` signal.
onClose :: ChildProcess -> (Maybe Int -> Maybe String -> Effect Unit) -> Effect Unit
onClose cp cb = runEffectFn2 onCloseImpl cp $ mkEffectFn2 \a b -> cb (toMaybe a) (toMaybe b)

foreign import onCloseImpl :: EffectFn2 (ChildProcess) (EffectFn2 (Nullable Int) (Nullable String) Unit) (Unit)

-- | Handle the `"disconnect"` signal.
onDisconnect :: ChildProcess -> Effect Unit -> Effect Unit
onDisconnect cp cb = runEffectFn2 onDisconnectImpl cp cb

foreign import onDisconnectImpl :: EffectFn2 (ChildProcess) (Effect Unit) (Unit)

-- | Handle the `"error"` signal.
onError :: ChildProcess -> (Error -> Effect Unit) -> Effect Unit
onError cp cb = runEffectFn2 onErrorImpl cp $ mkEffectFn1 cb

foreign import onErrorImpl :: EffectFn2 (ChildProcess) (EffectFn1 Error Unit) (Unit)

-- | Specifies how a child process exited; normally (with an exit code), or
-- | terminated by the given signal.
data Exit
  = ExitCode Int
  | SignalCode String

derive instance Eq Exit
derive instance Generic Exit _
instance showExit :: Show Exit where
  show x = genericShow x

-- | Handle the `"exit"` signal.
onExit :: ChildProcess -> (Exit -> Effect Unit) -> Effect Unit
onExit cp cb = runEffectFn2 onExitImpl cp $ mkEffectFn2 \e s ->
  cb case toMaybe e, toMaybe s of
    Just i, _ -> ExitCode i
    _, Just sig -> SignalCode sig
    _, _ -> unsafeCrashWith "Impossible: either exit code or signal code must be non-null"

foreign import onExitImpl :: EffectFn2 (ChildProcess) (EffectFn2 (Nullable Int) (Nullable String) Unit) (Unit)

-- | Handle the `"message"` signal.
onMessage :: ChildProcess -> (Foreign -> Maybe Handle -> Effect Unit) -> Effect Unit
onMessage cp cb = runEffectFn2 onMessageImpl cp $ mkEffectFn2 \a b -> cb a (toMaybe b)

foreign import onMessageImpl :: EffectFn2 (ChildProcess) (EffectFn2 Foreign (Nullable Handle) Unit) (Unit)

onSpawn :: ChildProcess -> Effect Unit -> Effect Unit
onSpawn cp cb = runEffectFn2 onSpawnImpl cp cb

foreign import onSpawnImpl :: EffectFn2 (ChildProcess) (Effect Unit) Unit

-- | either Int or String
foreign import data KillSignal :: Type

intKillSignal :: Int -> KillSignal
intKillSignal = unsafeCoerce

stringKillSignal :: String -> KillSignal
stringKillSignal = unsafeCoerce

fromKillSignal :: KillSignal -> Either Int String
fromKillSignal ks = do
  let
    ksFor :: Foreign
    ksFor = unsafeCoerce ks
    renderError errs = unsafeCrashWith
      $ append "Unexpected kill signal. Value should be String or Int but got these errors: "
      $ intercalate "; "
      $ map renderForeignError errs

  either renderError identity $ runExcept $ (Left <$> readInt ksFor) <|> (Right <$> readString ksFor)

data SerializationOption
  = SerializeJson
  | SerializeAdvanced

derive instance Eq SerializationOption
derive instance Generic SerializationOption _
instance Show SerializationOption where
  show x = genericShow x

toJsSerialization :: SerializationOption -> String
toJsSerialization = case _ of
  SerializeJson -> "json"
  SerializeAdvanced -> "advanced"

-- Note: `signal` option intentionally not supported.
type SpawnOptions =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , argv0 :: Maybe String
  , stdioExtra :: Maybe (Array Foreign)
  , detached :: Maybe Boolean
  , uid :: Maybe Int
  , gid :: Maybe Int
  , serialization :: Maybe SerializationOption
  , shell :: Maybe String
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  , timeout :: Maybe Number
  , killSignal :: Maybe (Either Int String)
  }

type JsSpawnOptions =
  { cwd :: String
  , env :: Object String
  , argv0 :: String
  , stdio :: Array Foreign
  , detached :: Boolean
  , uid :: Int
  , gid :: Int
  , serialization :: String
  , shell :: String
  , windowsVerbatimArguments :: Boolean
  , windowsHide :: Boolean
  , timeout :: Number
  , killSignal :: KillSignal
  }

spawn
  :: String
  -> Array String
  -> SpawnOptions
  -> Effect ChildProcess
spawn cmd args options = do
  runEffectFn3 spawnImpl cmd args
    { cwd: fromMaybe undefined options.cwd
    , env: fromMaybe undefined options.env
    , argv0: fromMaybe undefined options.argv0
    , detached: fromMaybe undefined options.detached
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , serialization: maybe undefined toJsSerialization options.serialization
    , stdio: [ pipe, pipe, pipe, ipc ] <> fromMaybe [] options.stdioExtra
    , shell: fromMaybe undefined options.shell
    , timeout: fromMaybe undefined options.timeout
    , killSignal: fromMaybe undefined $ map (either intKillSignal stringKillSignal) options.killSignal
    , windowsHide: fromMaybe undefined options.windowsHide
    , windowsVerbatimArguments: fromMaybe undefined options.windowsVerbatimArguments
    }
  where
  pipe = unsafeToForeign "pipe"
  ipc = unsafeToForeign "ipc"

foreign import spawnImpl
  :: EffectFn3
       String
       (Array String)
       (JsSpawnOptions)
       (ChildProcess)

type SpawnSyncOptions =
  { cwd :: Maybe String
  , input :: Maybe ImmutableBuffer
  , argv0 :: Maybe String
  , stdioExtra :: Maybe (Array Foreign)
  , env :: Maybe (Object String)
  , uid :: Maybe Int
  , gid :: Maybe Int
  , timeout :: Maybe Number
  , killSignal :: Maybe (Either Int String)
  , maxBuffer :: Maybe Number
  , shell :: Maybe String
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  }

type JsSpawnSyncOptions =
  { cwd :: String
  , argv0 :: String
  , input :: ImmutableBuffer
  , stdio :: Array Foreign
  , env :: Object String
  , uid :: Int
  , gid :: Int
  , timeout :: Number
  , killSignal :: KillSignal
  , maxBuffer :: Number
  , encoding :: String
  , shell :: String
  , windowsVerbatimArguments :: Boolean
  , windowsHide :: Boolean
  }

type JsSpawnSyncResult =
  { pid :: Pid
  , output :: Array Foreign
  , stdout :: ImmutableBuffer
  , stderr :: ImmutableBuffer
  , status :: Nullable Int
  , signal :: Nullable KillSignal
  , error :: Nullable Error
  }

spawnSync :: String -> Array String -> SpawnSyncOptions -> Effect JsSpawnSyncResult
spawnSync file args options = do
  runEffectFn3 spawnSyncImpl file args jsOptions
  where
  pipe = unsafeToForeign "pipe"
  ignore = unsafeToForeign "ignore"
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , argv0: fromMaybe undefined options.argv0
    , input: fromMaybe undefined options.input
    , stdio: [ pipe, pipe, pipe, ignore ] <> fromMaybe [] options.stdioExtra
    , env: fromMaybe undefined options.env
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , timeout: fromMaybe undefined options.timeout
    , killSignal: fromMaybe undefined $ map (either intKillSignal stringKillSignal) options.killSignal
    , maxBuffer: fromMaybe undefined options.maxBuffer
    , encoding: "buffer" -- force stdout/stderr in callback to be Buffers
    , shell: fromMaybe undefined options.shell
    , windowsHide: fromMaybe undefined options.windowsHide
    , windowsVerbatimArguments: fromMaybe undefined options.windowsVerbatimArguments
    }

foreign import spawnSyncImpl
  :: EffectFn3
       String
       (Array String)
       (JsSpawnSyncOptions)
       JsSpawnSyncResult

foreign import undefined :: forall a. a

-- | An error which occurred inside a child process.
type Error =
  { code :: String
  , errno :: String
  , syscall :: String
  , message :: Nullable String
  }

-- | Convert a ChildProcess.Error to a standard Error, which can then be thrown
-- | inside an Effect or Aff computation (for example).
toStandardError :: Error -> Exception.Error
toStandardError = unsafeCoerce

foreign import bufferToReadStream :: ImmutableBuffer -> Readable ()
