module Node.Library.Execa.Utils where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Exception (Error)
import Node.Stream (Duplex)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

newtype CustomError :: forall k. k -> Type
newtype CustomError rows = CustomError Error

toError :: forall rows. CustomError rows -> Error
toError (CustomError e) = e

getErrorOption
  :: forall sym a tail rows
   . IsSymbol sym
  => Row.Cons sym a tail rows
  => Row.Lacks "name" rows
  => Row.Lacks "message" rows
  => Proxy sym
  -> CustomError rows
  -> a
getErrorOption sym (CustomError e) = Record.get sym $ (unsafeCoerce :: Error -> { | rows }) e

foreign import buildCustomErrorImpl
  :: forall rows
   . Fn2 String { | rows } (CustomError rows)

buildCustomError
  :: forall rows
   . Row.Lacks "name" rows
  => Row.Lacks "message" rows
  => String
  -> { | rows }
  -> CustomError rows
buildCustomError msg info =
  runFn2 buildCustomErrorImpl msg info

bracketEffect :: forall a b. Effect a -> (a -> Effect Unit) -> (a -> Effect b) -> Effect b
bracketEffect open close use = do
  resource <- open
  b <- use resource
  b <$ close resource

foreign import newPassThroughStream :: Effect Duplex
