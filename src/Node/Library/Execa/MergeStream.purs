-- A majority of the below code was ported from this JavaScript library
-- https://github.com/grncdr/merge-stream
-- Copyright `merge-stream` contributors
-- MIT License: https://opensource.org/license/mit/
module Node.Library.Execa.MergeStream
  ( Interface
  , mergeStreams
  , add
  ) where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn2)
import Node.EventEmitter (once_, unsafeEmitFn)
import Node.Stream (Duplex, Readable, end, endH, errorH, pipe')
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

newtype Interface = Interface
  { sources :: Ref (Array (Readable ()))
  , output :: Duplex
  }

mergeStreams :: (Interface -> Effect Unit) -> Effect Duplex
mergeStreams useInterface = do
  iface@(Interface { output }) <- buildInterface
  useInterface iface
  pure output

buildInterface :: Effect Interface
buildInterface = do
  sources <- Ref.new []
  output <- Stream.newPassThrough
  pure $ Interface { sources, output }

add :: forall w. Readable w -> Interface -> Effect Boolean
add source iface@(Interface r) = do
  ifM
    (Stream.readable r.output)
    ( do
        let readStream = toReadableOnlyStream source
        Ref.modify_ (flip Array.snoc readStream) r.sources

        readStream # once_ endH (remove readStream iface)
        readStream # once_ errorH \err -> do
          void $ runEffectFn2 (unsafeEmitFn $ Stream.toEventEmitter r.output) "error" err
        pipe' readStream r.output { end: false }
        pure true
    )
    (pure false)

remove :: forall w. Readable w -> Interface -> Effect Unit
remove source (Interface r) = do
  arr <- Ref.read r.sources
  let arr' = Array.filter (not <<< unsafeRefEq (toReadableOnlyStream source)) arr
  stillReadable <- Stream.readable r.output
  when (Array.length arr' == 0 && stillReadable) do
    end r.output

toReadableOnlyStream :: forall w. Readable w -> Readable ()
toReadableOnlyStream = unsafeCoerce
