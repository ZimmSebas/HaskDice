module DEFS where

import Prelude
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

-- Tirada de dados, D representa K tiradas de dados de N caras comenzando en 1 (kdn)
-- Mientras que Z representa K tiradas de de dados de N caras comenzando en 0 (kZn)
--                 K   N
data DiceRoll = D Int Int
              | Z Int Int

-- 

-- Colecciones
type Collection = MultiSet (Int,Double)

