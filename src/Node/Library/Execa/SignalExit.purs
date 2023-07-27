-- A majority of the below code was ported from this JavaScript library
-- https://github.com/tapjs/signal-exit
-- Copyright `signal-exit` contributors
-- ISC License: https://opensource.org/license/isc/
module Node.Library.Execa.SignalExit
  ( onExit
  , onAfterExit
  ) where

import Prelude

import Data.Either (hush)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Nullable (Nullable, notNull, null, toMaybe, toNullable)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Effect (Effect)
import Effect.Exception (try)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Node.EventEmitter (EventEmitter, EventHandle(..), listenerCount, on, unsafeEmitFn)
import Node.EventEmitter as EventEmitter
import Node.Platform (Platform(..))
import Node.Process (Process, mkSignalH', process)
import Node.Process as Process
import Unsafe.Coerce (unsafeCoerce)

foreign import unsafeProcessHasProp :: EffectFn1 String Boolean
foreign import unsafeReadProcessProp :: forall a. EffectFn1 String a
foreign import unsafeWriteProcessProp :: forall a. EffectFn2 String a Unit

foreign import data ProcessEmitFn :: Type
foreign import data ProcessReallyExitFn :: Type
foreign import processCallFn :: EffectFn2 ProcessReallyExitFn (Nullable Int) Unit

foreign import customProcessEmit :: EffectFn3 (EffectFn1 ProcessEmitFn Boolean) String (Nullable Int) Boolean -> EffectFn2 String (Nullable Int) Boolean

processToEventEmitter :: Process -> EventEmitter
processToEventEmitter = unsafeCoerce

isWin :: Boolean
isWin = Just Win32 == Process.platform

type Options =
  { alwaysLast :: Boolean
  }

newtype ExitEmitter = ExitEmitter EventEmitter

exitEvent :: String
exitEvent = "exit"

afterexitEvent :: String
afterexitEvent = "afterexit"

exitE :: Maybe Int -> Maybe String -> ExitEmitter -> Effect Unit
exitE code err (ExitEmitter emitter) =
  void $ runEffectFn3 (unsafeEmitFn emitter) exitEvent (toNullable code) (toNullable err)

exitH :: EventHandle ExitEmitter (Maybe Int -> Maybe String -> Effect Unit) (EffectFn2 (Nullable Int) (Nullable String) Unit)
exitH = EventHandle exitEvent \cb -> mkEffectFn2 \code err ->
  cb (toMaybe code) (toMaybe err)

afterexitE :: Maybe Int -> Maybe String -> ExitEmitter -> Effect Unit
afterexitE code err (ExitEmitter emitter) =
  void $ runEffectFn3 (unsafeEmitFn emitter) afterexitEvent (toNullable code) (toNullable err)

afterexitH :: EventHandle ExitEmitter (Maybe Int -> Maybe String -> Effect Unit) (EffectFn2 (Nullable Int) (Nullable String) Unit)
afterexitH = EventHandle afterexitEvent \cb -> mkEffectFn2 \code err ->
  cb (toMaybe code) (toMaybe err)

onExit :: (Maybe Int -> Maybe String -> Effect Unit) -> Effect (Effect Unit)
onExit cb = onExit' cb { alwaysLast: false }

onAfterExit :: (Maybe Int -> Maybe String -> Effect Unit) -> Effect (Effect Unit)
onAfterExit cb = onExit' cb { alwaysLast: true }

-- PureScript implementation note:
-- I'm not sure what will happen if this library and
-- the original `signal-exit` JS library is used
-- in the same project.
onExit' :: (Maybe Int -> Maybe String -> Effect Unit) -> Options -> Effect (Effect Unit)
onExit' cb options = do
  { emitter: exitEmitter@(ExitEmitter emitter) } <- getGlobalRecOnProcessObject
  load
  unSubscribe <-
    if options.alwaysLast then do
      exitEmitter # on afterexitH cb
    else do
      exitEmitter # on exitH cb
  pure do
    unSubscribe
    exitLen <- EventEmitter.listenerCount emitter exitEvent
    afterExitLen <- EventEmitter.listenerCount emitter afterexitEvent
    when (exitLen == 0 && afterExitLen == 0) do
      unload
  where
  unload = do
    { loadedRef
    , countRef
    , signalListenersRef
    , restoreOriginalProcessFunctions
    } <- getGlobalRecOnProcessObject
    whenM (Ref.read loadedRef) do
      Ref.write false loadedRef
      Ref.read signalListenersRef >>= traverse_ case _ of
        Nothing -> pure unit
        Just unsubscribe -> unsubscribe
      restoreOriginalProcessFunctions
      Ref.modify_ (_ - 1) countRef

  emitFn = mkEffectFn3 \event code signal -> do
    { emitter: ExitEmitter emitter
    , emittedEventsRef
    } <- getGlobalRecOnProcessObject
    eventsAlreadyEmitted <- Ref.read emittedEventsRef
    unless (Set.member event eventsAlreadyEmitted) do
      Ref.modify_ (Set.insert event) emittedEventsRef
      map (\(_ :: Boolean) -> unit) $ (runEffectFn3 (unsafeEmitFn emitter) event code signal)

  load = do
    { loadedRef
    , countRef
    , signalListenersRef
    } <- getGlobalRecOnProcessObject
    unlessM (Ref.read loadedRef) do
      Ref.write true loadedRef
      -- This is the number of onSignalExit's that are in play.
      -- It's important so that we can count the correct number of
      -- listeners on signals, and don't wait for the other one to
      -- handle it instead of us.
      Ref.modify_ (_ + 1) countRef
      -- PureScript implementation note:
      -- The `signals` array is not filtered to
      -- only include signals where `process.on`
      -- did not throw. Rather, we just store
      -- either a `Nothing` (it threw) or
      -- or a `Just unsubscribe` (if it did not throw).
      -- The `Just` stores the code we need to use later
      -- to remove the listener. This obviates the need
      -- for a `Object.lookup sig sigListeners` usage here
      -- that can return a `Nothing` when it's impossible
      -- for that to occur.
      signalListeners <- for signals \sig -> map hush $ try do
        let listener = mkListener sig countRef
        rm <- process # on (mkSignalH' sig) listener
        pure $ void $ try do
          rm
      Ref.write signalListeners signalListenersRef
      runEffectFn2 unsafeWriteProcessProp "emit" processEmitFn
      runEffectFn2 unsafeWriteProcessProp "reallyExit" processReallyExitFn

  -- Good
  mkListener :: String -> Ref Int -> Effect Unit
  mkListener sig countRef = do
    listenersLen <- listenerCount (processToEventEmitter process) sig
    count <- Ref.read countRef
    when (listenersLen == count) do
      unload
      runEffectFn3 emitFn exitEvent null (notNull sig)
      runEffectFn3 emitFn afterexitEvent null (notNull sig)
      -- "SIGHUP" throws an `ENOSYS` error on Windows,
      -- so use a supported signal instead
      let sig' = if isWin && sig == "SIGHUP" then "SIGINT" else sig
      Process.killStr Process.pid sig'

  processReallyExitFn = mkEffectFn1 \(code :: Nullable Int) -> do
    { emitter
    , originalProcessReallyExit
    } <- getGlobalRecOnProcessObject
    let exitCode = fromMaybe 0 $ toMaybe code
    runEffectFn2 unsafeWriteProcessProp exitEvent exitCode
    emitter # exitE (Just exitCode) Nothing
    emitter # afterexitE (Just exitCode) Nothing
    runEffectFn2 processCallFn originalProcessReallyExit (notNull exitCode)

  processEmitFn = customProcessEmit $ mkEffectFn3 \runOriginalProcessEmit ev arg -> do
    { originalProcessEmit } <- getGlobalRecOnProcessObject
    if ev == exitEvent then do
      exitCode <- case toMaybe arg of
        Nothing ->
          map toNullable $ Process.getExitCode
        Just exitCode' -> do
          Process.setExitCode exitCode'
          pure $ notNull exitCode'

      ret <- runEffectFn1 runOriginalProcessEmit originalProcessEmit
      runEffectFn3 emitFn exitEvent exitCode null
      runEffectFn3 emitFn afterexitEvent exitCode null
      pure ret
    else do
      runEffectFn1 runOriginalProcessEmit originalProcessEmit

signalExitProp :: String
signalExitProp = "__purescript_signal_exit__"

type SignalEventRecord =
  { originalProcessEmit :: ProcessEmitFn
  , originalProcessReallyExit :: ProcessReallyExitFn
  , restoreOriginalProcessFunctions :: Effect Unit
  , emitter :: ExitEmitter
  , countRef :: Ref Int
  , emittedEventsRef :: Ref (Set String)
  , loadedRef :: Ref Boolean
  , signalListenersRef :: Ref (Array (Maybe (Effect Unit)))
  }

getGlobalRecOnProcessObject :: Effect SignalEventRecord
getGlobalRecOnProcessObject =
  ifM
    (runEffectFn1 unsafeProcessHasProp signalExitProp)
    (runEffectFn1 unsafeReadProcessProp signalExitProp)
    attachRefsToProcessObject
  where
  attachRefsToProcessObject = do
    originalProcessEmit :: ProcessEmitFn <- runEffectFn1 unsafeReadProcessProp "emit"
    originalProcessReallyExit :: ProcessReallyExitFn <- runEffectFn1 unsafeReadProcessProp "reallyExit"
    let
      restoreOriginalProcessFunctions = do
        runEffectFn2 unsafeWriteProcessProp "emit" originalProcessEmit
        runEffectFn2 unsafeWriteProcessProp "reallyExit" originalProcessReallyExit

    emitter <- EventEmitter.new
    EventEmitter.setUnlimitedListeners emitter
    countRef <- Ref.new 0
    emittedEventsRef <- Ref.new Set.empty
    loadedRef <- Ref.new false
    signalListenersRef <- Ref.new ([] :: Array (Maybe (Effect Unit)))

    let
      obj =
        { originalProcessEmit
        , originalProcessReallyExit
        , restoreOriginalProcessFunctions
        , emitter: ExitEmitter emitter
        , countRef
        , emittedEventsRef
        , loadedRef
        , signalListenersRef
        }
    runEffectFn2 unsafeWriteProcessProp signalExitProp obj
    pure obj

-- This is not the set of all possible signals.
--
-- It IS, however, the set of all signals that trigger
-- an exit on either Linux or BSD systems.  Linux is a
-- superset of the signal names supported on BSD, and
-- the unknown signals just fail to register, so we can
-- catch that easily enough.
--
-- Don"t bother with SIGKILL.  It"s uncatchable, which
-- means that we can't fire any callbacks anyway.
--
-- If a user does happen to register a handler on a non-
-- fatal signal like SIGWINCH or something, and then
-- exit, it"ll end up firing `process.emit("exit')`, so
-- the handler will be fired anyway.
--
-- SIGBUS, SIGFPE, SIGSEGV and SIGILL, when not raised
-- artificially, inherently leave the process in a
-- state from which it is not safe to try and enter JS
-- listeners.
signals :: Array String
signals = normal <> nonWindows <> linux
  where
  normal =
    [ "SIGHUP"
    , "SIGINT"
    , "SIGTERM"
    ]
  nonWindows = guard (not isWin)
    [ "SIGABRT"
    , "SIGALRM"
    , "SIGVTALRM"
    , "SIGXCPU"
    , "SIGXFSZ"
    , "SIGUSR2"
    , "SIGTRAP"
    , "SIGSYS"
    , "SIGQUIT"
    , "SIGIOT"
    -- See https://github.com/tapjs/signal-exit/issues/21
    -- Should detect profiler and enable/disable accordingly.
    -- PureScript implementation note:
    -- Ideally, this would be detected automatically
    -- based on the Node args to passed to Node.
    -- For the time being, we'll just add the manual option.
    , "SIGPROF"
    ]
  linux = guard (Process.platform == Just Linux)
    [ "SIGIO"
    , "SIGPOLL"
    , "SIGPWR"
    , "SIGSTKFLT"
    ]
