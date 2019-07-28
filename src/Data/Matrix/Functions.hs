module Data.Matrix.Functions
  ( matrixToLists
  , padTop
  , padBottom
  , mono
  , extend
  , matrixToFourLists
  ) where


import qualified Data.Four   as F
import qualified Data.Matrix as M
import qualified Data.Vector as V


matrixToLists :: M.Matrix a -> [[a]]
matrixToLists m = map (\r -> V.toList $ M.getRow r m) [1..nr]
  where
    nr = M.nrows m


padTop :: a -> Int -> M.Matrix a -> M.Matrix a
padTop def n m = M.matrix n (M.ncols m) (const def) M.<-> m

padBottom :: a -> Int -> M.Matrix a -> M.Matrix a
padBottom def n m = m M.<-> M.matrix n (M.ncols m) (const def)


extend :: a -> Int -> Int -> M.Matrix a -> M.Matrix a
extend def newR newC m = m''
  where
   rDiff = newR - M.nrows m
   cDiff = newC - M.ncols m
   m'    = if rDiff <= 0 then m  else m  M.<-> mono def rDiff (M.ncols m)
   m''   = if cDiff <= 0 then m' else m' M.<|> mono def (M.nrows m') cDiff


mono :: a -> Int -> Int -> M.Matrix a
mono def r c = M.matrix r c (const def)


matrixToFourLists :: a -> Int -> Int -> Int -> Int -> M.Matrix a
                  -> [[F.Four a]]
matrixToFourLists def rx cx tPad lPad m = do
  r <- [1 .. (nr * rx + tPad) `divRUp` 2]
  return $ do
    c <- [1 .. (nc * cx + lPad) `divRUp` 2]
    let el sr sc = let r' = (pred r * 2 + sr) - tPad
                       c' = (pred c * 2 + sc) - lPad
                   in  if r' > 0 && c' > 0
                       then M.getElem (r' `divRUp` rx) (c' `divRUp` cx) m'
                       else def
    return $ F.Four (el 1 1) (el 1 2) (el 2 1) (el 2 2)
  where
    nr = M.nrows m
    nc = M.ncols m
    m' = extend def (succ nr) (succ nc) m


divRUp :: Integral a => a -> a -> a
divRUp x y = div (x + y - 1) y
