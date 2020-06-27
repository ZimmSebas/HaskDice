module DEFS where

import Prelude
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

-- Dado
data Dice = D Int Int

-- Colecciones
type Collection = MultiSet (Int,Double)
