-- A majority of the below code was ported from this JavaScript library
-- https://github.com/sindresorhus/get-stream
-- Copyright `get-stream` contributors
-- MIT License: https://opensource.org/license/mit/
module Node.Library.Execa.GetStream where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (infinity)
import Effect (Effect)
import Effect.Aff (Aff, Error, effectCanceler, error, makeAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.EventEmitter (on)
import Node.Stream (Duplex, Readable, dataH, newPassThrough)
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

type Interface =
  { getBufferedValue :: Effect Buffer
  , getBufferedLength :: Effect Number
  , stream :: Duplex
  }

getStreamBuffer
  :: forall r
   . Readable r
  -> { maxBuffer :: Maybe Number }
  -> Aff { buffer :: Buffer, inputError :: Maybe Error }
getStreamBuffer inputStream initialOptions = do
  let options = { maxBuffer: fromMaybe infinity initialOptions.maxBuffer }
  interface <- liftEffect bufferStream
  -- PureScript implementation note:
  -- Execa gets the buffered data whether the stream
  -- fails or not. It also destroys the input stream if an error occurs
  -- but we'll handle that outside of this function.
  makeAff \cb -> do
    Stream.pipeline inputStream [] interface.stream \err -> do
      bufferedData <- interface.getBufferedValue
      cb $ Right { buffer: bufferedData, inputError: err }
    rmListener <- interface.stream # on dataH \_ -> do
      bufferedLen <- interface.getBufferedLength
      when (bufferedLen > options.maxBuffer) do
        bufferedData <- interface.getBufferedValue
        cb $ Right
          { buffer: bufferedData
          , inputError: Just $ error $ maybe
              ("Max buffer exceeded")
              (\size -> "Max buffer size exceeded. Buffer size was: " <> show size)
              initialOptions.maxBuffer
          }
    pure $ effectCanceler do
      interface.unsubcribe
      rmListener
  where
  -- PureScript implementation note:
  -- - object mode == false due to 'buffer' usage
  -- - encoding = null due to 'buffer' usage
  bufferStream = do
    chunksRef <- Ref.new []
    lengthRef <- Ref.new 0.0
    stream <- newPassThrough
    rmData <- stream # on dataH \buf -> do
      Ref.modify_ (\chunks -> Array.snoc chunks buf) chunksRef
      bufLen <- Buffer.size buf
      Ref.modify_ (_ + (toNumber bufLen)) lengthRef
    pure
      { getBufferedValue: do
          chunks <- Ref.read chunksRef
          len <- Ref.read lengthRef
          let
            -- PureScript implementation note:
            --  maxBufferLength = 2^32
            --  PS Int type = 2^31
            asTooLargeInt :: Number -> Int
            asTooLargeInt = unsafeCoerce
          Buffer.concat' chunks $ asTooLargeInt len
      , getBufferedLength: Ref.read lengthRef
      , stream
      , unsubcribe: rmData
      }
