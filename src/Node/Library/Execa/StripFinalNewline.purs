-- A majority of the below code was ported from this JavaScript library
-- https://github.com/sindresorhus/strip-final-newline
-- Copyright `strip-final-newline` contributors
-- MIT License: https://opensource.org/license/mit/
module Node.Library.Execa.StripFinalNewline where

import Prelude

import Data.Char (toCharCode)
import Data.Int as Int
import Data.String as String
import Effect (Effect)
import Node.Buffer (Buffer, BufferValueType(..), read, size, slice)

stripFinalNewline :: String -> String
stripFinalNewline s = do
  let
    { before: sDrop1, after: lastChar } = String.splitAt (String.length s - 1) s
  case lastChar of
    "\n" -> do
      let { before: sDrop2, after: sndLastChar } = String.splitAt (String.length sDrop1 - 1) sDrop1
      case sndLastChar of
        "\r" -> sDrop2
        _ -> sDrop1
    "\r" -> sDrop1
    _ -> s

stripFinalNewlineBuf :: Buffer -> Effect Buffer
stripFinalNewlineBuf b = do
  len <- size b
  -- PureScript implementation note: 
  -- The `BufferValueType` does not matter.
  -- Node docs state this about the blob's type and these
  -- docs have not changed across major version releases.
  -- """
  --   type <string> - The Blob content-type.
  --     The intent is for type to convey the MIME media type of the data, 
  --     however **no validation of the type format is performed**. (emphasis mine)
  -- """
  -- Source (v18): https://nodejs.org/docs/latest-v18.x/api/buffer.html#buffer_new_buffer_blob_sources_options
  -- Source (v16): https://nodejs.org/docs/latest-v16.x/api/buffer.html#buffer_new_buffer_blob_sources_options
  -- Source (v14): https://nodejs.org/docs/latest-v14.x/api/buffer.html#buffer_new_buffer_blob_sources_options
  case len of
    0 ->
      pure b
    1 -> do
      lastChar <- read UInt8 (len - 1) b
      if lastChar == charN || lastChar == charR then do
        pure $ slice 0 (len - 1) b
      else do
        pure b
    _ -> do
      lastChar <- read UInt8 (len - 1) b
      sndLastChar <- read UInt8 (len - 2) b
      if lastChar == charN && sndLastChar == charR then do
        pure $ slice 0 (len - 2) b
      else if lastChar == charN || lastChar == charR then do
        pure $ slice 0 (len - 1) b
      else do
        pure b
  where
  charN = Int.toNumber $ toCharCode '\n'
  charR = Int.toNumber $ toCharCode '\r'
