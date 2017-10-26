module Utils
  ( first
  , second
  , ordNub
  , maybeErr
  , traceIf
  , isSorted
  ) where

import Debug.Trace (trace)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
import Text.Printf

-- can also be found in Control.Arrow and Data.Bifunctor
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, b) = (f a, b)

second :: (a -> b) -> (c, a) -> (c, b)
second f (a, b) = (a, f b)

-- https://github.com/nh2/haskell-ordnub
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

maybeErr :: String -> Maybe a -> a
maybeErr str Nothing    = error str
maybeErr _   (Just val) = val

traceIf :: Bool -> String -> a -> a
traceIf cond txt x = if cond then trace txt x else x

isSorted :: Ord a => [a] -> Bool
isSorted xs = and (zipWith (<=) xs (tail xs))

-- WARNING: this function is a bit misleading because it does not take into account the offset compensation in adjustSampleInterval
printVec :: Double -> VU.Vector Double -> Int -> IO ()
printVec st values len = mapM_ (\(x, y) -> printf "%-18f %f\n" x y) $ take len $ zip [0,st..] (VU.toList values)
