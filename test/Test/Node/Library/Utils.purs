module Test.Node.Library.Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Node.Platform (Platform(..))
import Node.Process as Process
import Test.Spec (class Example, SpecT, describe, it)

isWindows :: Boolean
isWindows = Process.platform == Just Win32

itWindows :: forall m t arg g. Monad m => Example t arg g => String -> t -> SpecT g arg m Unit
itWindows msg test = do
  when isWindows do
    it (msg <> " (windows only)") test

itNix :: forall m t arg g. Monad m => Example t arg g => String -> t -> SpecT g arg m Unit
itNix msg test = do
  unless isWindows do
    it (msg <> " (*nix only)") test

describeWindows :: forall g i m. Monad m => String → SpecT g i m Unit → SpecT g i m Unit
describeWindows msg test = do
  when isWindows do
    describe (msg <> " (windows only)") test

describeNix :: forall g i m. Monad m => String → SpecT g i m Unit → SpecT g i m Unit
describeNix msg test = do
  unless isWindows do
    describe (msg <> " (*nix only)") test
