{-# LANGUAGE DeriveFunctor #-}

module Data.Four
  ( Four (..)
  , fourBitToBlock
  , tupleToFour
  , toFour
  ) where


import qualified Data.Matrix as M


data Four a = Four a a
                   a a
  deriving (Show, Eq, Functor)


fromFourBit :: Four Bool -> Int
fromFourBit = f . fmap fromEnum
  where
    f :: Four Int -> Int
    f (Four a b c d) = 2 ^ 0 * a + 2 ^ 1 * b + 2 ^ 2 * c + 2 ^ 3 * d


fourBitToBlock :: Four Bool -> Char
fourBitToBlock = fourBitBlock . fromFourBit

fourBitBlock :: Int -> Char
fourBitBlock n = case n of
  0  -> ' '
  1  -> '▘'
  2  -> '▝'
  3  -> '▀'
  4  -> '▖'
  5  -> '▌'
  6  -> '▞'
  7  -> '▛'
  8  -> '▗'
  9  -> '▚'
  10 -> '▐'
  11 -> '▜'
  12 -> '▄'
  13 -> '▙'
  14 -> '▟'
  15 -> '█'


pair :: a -> [a] -> [(a, a)]
pair _   []         = []
pair def (a:[])     = [(a, def)]
pair def (a1:a2:as) = (a1, a2) : pair def as


zoom :: a -> Int -> [a] -> [(a, a)]
zoom def n = pair def . concatMap (replicate n)


tupleToFour :: ((a, a), (a, a)) -> Four a
tupleToFour ((a, b), (c, d)) = Four a b c d


toFour :: Int -> Int -> a -> [[a]] -> [[Four a]]
toFour x y def lss = map (map tupleToFour . uncurry zip)
                   $ zoom (replicate nc (def, def)) y $ map (zoom def x) lss
                   where
                     nc = length $ head lss
