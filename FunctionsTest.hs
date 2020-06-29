module FunctionsTest where

import DEFS
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import System.IO
import System.Random 
import Prelude

-- ~ randomList :: Int -> [Double]
-- ~ randomList seed = randoms (mkStdGen seed) :: [Double]

-- Given a seed, generates 1 dice roll of n sides
diceroll :: StdGen -> Rolls -> [Int]
diceroll g (D k n) = take k (randomRs (0 :: Int,n) g)

diceRoll :: Rolls -> IO [Int]
diceRoll (D k n) = do
    g <- newStdGen
    let rolls = take k (randomRs (0::Int,n) g)
    return rolls

filterColl :: (Int -> Bool) -> [Int] -> [Int]
filterColl f l = filter f l

sumColl :: [Int] -> Int
sumColl l = sum l

countColl :: [Int] -> Int
countColl l = length l

(@@) :: [Int] -> [Int] -> [Int]
(@@) = (++)

mainFunc = do 
 g <- newStdGen -- Generador de aleatorios
 let r1 = diceroll g (D 3 6)
     r2 = diceroll g (D 4 7)
     r3 = diceroll g (D 3 6)
     r4 = diceroll g (D 3 6)
 print r2
 print r1
 print r3
 print r4
 print (sumColl r1)
 print (countColl (r1 @@ r2))
 
