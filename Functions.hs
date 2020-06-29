module Functions where

import DEFS
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import System.IO
import System.Random 
import Prelude

filterColl :: (Int -> Bool) -> [Int] -> [Int]
filterColl f l = filter f l

sumColl :: [Int] -> Int
sumColl l = sum l

countColl :: [Int] -> Int
countColl l = length l

(@@) :: [Int] -> [Int] -> [Int]
(@@) = (++)
