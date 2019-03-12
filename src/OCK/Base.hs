{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module OCK.Base where

import Graphics.Vty
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Time   as T
import Control.Monad      (forever)
import Control.Exception  (bracket)
import Data.Maybe         (listToMaybe,fromMaybe,catMaybes)
import Numeric (readHex)
import System.Timeout     (timeout)
import System.Environment (lookupEnv,setEnv,unsetEnv)


partition :: M.Matrix Bool
partition = M.colVector $ V.replicate 5 False


colon :: M.Matrix Bool
colon = M.colVector $ V.fromList [False, True, False, True, False]


_0 :: M.Matrix Bool
_0 = let { l = True; o = False; } in M.fromLists [ [l, l, l]
                                                 , [l, o, l]
                                                 , [l, o, l]
                                                 , [l, o, l]
                                                 , [l, l, l]
                                                 ]

_1 :: M.Matrix Bool
_1 = let { l = True; o = False; } in M.fromLists [ [o, o, l]
                                                 , [o, o, l]
                                                 , [o, o, l]
                                                 , [o, o, l]
                                                 , [o, o, l]
                                                 ]

_1' :: M.Matrix Bool
_1' = let { l = True; o = False; } in M.fromLists [ [o, l]
                                                  , [o, l]
                                                  , [o, l]
                                                  , [o, l]
                                                  , [o, l]
                                                  ]

_2 :: M.Matrix Bool
_2 = let { l = True; o = False; } in M.fromLists [ [l, l, l]
                                                 , [o, o, l]
                                                 , [l, l, l]
                                                 , [l, o, o]
                                                 , [l, l, l]
                                                 ]

_3 :: M.Matrix Bool
_3 = let { l = True; o = False; } in M.fromLists [ [l, l, l]
                                                 , [o, o, l]
                                                 , [l, l, l]
                                                 , [o, o, l]
                                                 , [l, l, l]
                                                 ]

_4 :: M.Matrix Bool
_4 = let { l = True; o = False; } in M.fromLists [ [l, o, l]
                                                 , [l, o, l]
                                                 , [l, l, l]
                                                 , [o, o, l]
                                                 , [o, o, l]
                                                 ]

_5 :: M.Matrix Bool
_5 = let { l = True; o = False; } in M.fromLists [ [l, l, l]
                                                 , [l, o, o]
                                                 , [l, l, l]
                                                 , [o, o, l]
                                                 , [l, l, l]
                                                 ]

_6 :: M.Matrix Bool
_6 = let { l = True; o = False; } in M.fromLists [ [l, l, l]
                                                 , [l, o, o]
                                                 , [l, l, l]
                                                 , [l, o, l]
                                                 , [l, l, l]
                                                 ]

_7 :: M.Matrix Bool
_7 = let { l = True; o = False; } in M.fromLists [ [l, l, l]
                                                 , [o, o, l]
                                                 , [o, o, l]
                                                 , [o, o, l]
                                                 , [o, o, l]
                                                 ]

_8 :: M.Matrix Bool
_8 = let { l = True; o = False; } in M.fromLists [ [l, l, l]
                                                 , [l, o, l]
                                                 , [l, l, l]
                                                 , [l, o, l]
                                                 , [l, l, l]
                                                 ]

_9 :: M.Matrix Bool
_9 = let { l = True; o = False; } in M.fromLists [ [l, l, l]
                                                 , [l, o, l]
                                                 , [l, l, l]
                                                 , [o, o, l]
                                                 , [l, l, l]
                                                 ]

data Four a = Four a a
                   a a
  deriving (Show, Eq, Functor)


fromFourBit :: Four Bool -> Int
fromFourBit = f . fmap fromEnum
  where
    f :: Four Int -> Int
    f (Four a b c d) = 2 ^ 0 * a + 2 ^ 1 * b + 2 ^ 2 * c + 2 ^ 3 * d


fourBit :: Attr -> Four Bool -> Image
fourBit attr = char attr . fourBit'

fourBit' :: Four Bool -> Char
fourBit' = fourBitBlock . fromFourBit

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


toFour :: Int -> Int -> a -> [[a]] -> [[Four a]]
toFour x y def lss = map (map tupleToFour . uncurry zip)
                   $ zoom (replicate nc (def, def)) y $ map (zoom def x) lss
                   where
                     nc = length $ head lss


tupleToFour :: ((a, a), (a, a)) -> Four a
tupleToFour ((a, b), (c, d)) = Four a b c d


zoom :: a -> Int -> [a] -> [(a, a)]
zoom def n = pair def . concatMap (replicate n)


pair :: a -> [a] -> [(a, a)]
pair _   []         = []
pair def (a:[])     = [(a, def)]
pair def (a1:a2:as) = (a1, a2) : pair def as


zoomMatrix :: Int -> Int -> M.Matrix a -> M.Matrix a
zoomMatrix x y m = undefined


matrixToLists :: M.Matrix a -> [[a]]
matrixToLists m = map (\r -> V.toList $ M.getRow r m) [1..nr]
  where
    nr = M.nrows m


matrixToFourLists :: a -> Int -> Int -> M.Matrix a -> [[Four a]]
matrixToFourLists def rx cx m = do
  r <- [1 .. (nr * rx) `divRUp` 2]
  return $ do
    c <- [1 .. (nc * cx) `divRUp` 2]
    let fr s     = (pred r * 2 + s) `divRUp` rx
        fc s     = (pred c * 2 + s) `divRUp` cx
        el sr sc = M.getElem (fr sr) (fc sc) m'
    return $ Four (el 1 1) (el 1 2) (el 2 1) (el 2 2)
  where
    nr = M.nrows m
    nc = M.ncols m
    m' = extend def (succ nr) (succ nc) m


matrixToFourLists' :: a -> Int -> Int -> Int -> Int -> M.Matrix a
                   -> [[Four a]]
matrixToFourLists' def rx cx tPad lPad m = do
  r <- [1 .. (nr * rx + tPad) `divRUp` 2]
  return $ do
    c <- [1 .. (nc * cx + lPad) `divRUp` 2]
    let el sr sc = let r' = (pred r * 2 + sr) - tPad
                       c' = (pred c * 2 + sc) - lPad
                   in  if r' > 0 && c' > 0
                       then M.getElem (r' `divRUp` rx) (c' `divRUp` cx) m'
                       else def
    return $ Four (el 1 1) (el 1 2) (el 2 1) (el 2 2)
  where
    nr = M.nrows m
    nc = M.ncols m
    m' = extend def (succ nr) (succ nc) m


data Env = Env
  { tbMarg    :: Int
  , lrMarg    :: Int
  , tbPadd    :: Int
  , lrPadd    :: Int
  , forceEven :: Bool
  , clckAttr  :: Attr
  , frmeAttr  :: Attr
  , backAttr  :: Attr
  }


main :: IO ()
main = do
  cfg       <- standardIOConfig
  out       <- outputForConfig cfg
  tbMarg    <- fromEnv "OCK_TopBottomMargine" 2  [RSome id]
  lrMarg    <- fromEnv "OCK_LeftRightMargine" 1  [RSome id]
  tbPadd    <- fromEnv "OCK_TopBottomPadding" 10 [RSome id]
  lrPadd    <- fromEnv "OCK_LeftRightPadding" 3  [RSome id]
  forceEven <- fromEnv "OCK_ForceEven" False
    [RSome id, RSome (== 1), RSome (== Yes), RSome (== On), RSome (== STrue)]
  backBCFun <- fromEnv "OCK_BackColor"      id bcRead
  clckBCFun <- fromEnv "OCK_ClockBackColor" id bcRead
  foreFun   <- fromEnv "OCK_ForeColor"      id fcRead
  frmeFCFun <- fromEnv "OCK_FrameColor"     id fcRead
  clckFCFun <- fromEnv "OCK_ClockForeColor" id fcRead
  let clckAttr = clckBCFun $ backBCFun $ clckFCFun $ foreFun defAttr
      frmeAttr = clckBCFun $ backBCFun $ frmeFCFun $ foreFun defAttr
      backAttr = backBCFun defAttr
  withVty cfg $ clock out $ Env {..}
    where
      bcRead = [RSome withBC, RSome $ withBC . toRGB]
      fcRead = [RSome withFC, RSome $ withFC . toRGB]


type RGB = (Int, Int, Int)
newtype FromHexRGB = FromHexRGB { toRGB :: RGB }

instance Read FromHexRGB where
  readsPrec _ str = case hex of
    r1:r2:g1:g2:b1:b2:str' -> do
          r <- filter (null . snd) $ readHex [r1, r2]
          g <- filter (null . snd) $ readHex [g1, g2]
          b <- filter (null . snd) $ readHex [b1, b2]
          return $ (FromHexRGB (fst r, fst g, fst b), str')
    _  -> []
    where
      hex = case str of
              '#':str' -> str'
              _        -> str


withBC :: RGB -> Attr -> Attr
withBC (r,g,b) = flip withBackColor $ rgbColor r g b

withFC :: RGB -> Attr -> Attr
withFC (r,g,b) = flip withForeColor $ rgbColor r g b


setBack :: Background -> Picture -> Picture
setBack bc pic@Picture {..} = pic { picBackground = bc }


data Yes = Yes deriving (Show, Eq, Enum)
instance Read Yes where
  readsPrec _ ('Y':'E':'S':str) = [(Yes,str)]
  readsPrec _ ('Y':'e':'s':str) = [(Yes,str)]
  readsPrec _ ('y':'e':'s':str) = [(Yes,str)]
  readsPrec _ ('y':str)         = [(Yes,str)]
  readsPrec _ _                 = []


data On = On deriving (Show, Eq, Enum)
instance Read On where
  readsPrec _ ('O':'N':str) = [(On,str)]
  readsPrec _ ('O':'n':str) = [(On,str)]
  readsPrec _ ('o':'n':str) = [(On,str)]
  readsPrec _ _             = []


data STrue = STrue deriving (Show, Eq, Enum)
instance Read STrue where
  readsPrec _ ('t':'r':'u':'e':str) = [(STrue,str)]
  readsPrec _ ('t':str)             = [(STrue,str)]
  readsPrec _ _                     = []


data ReadSomething a = forall b. Read b => RSome (b -> a)

readSomething :: [ReadSomething a] -> [Char] -> [Maybe a]
readSomething rs str = map (\(RSome f) -> fmap f $ maybeRead str) rs

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


fromEnv :: String -> a -> [ReadSomething a] -> IO a
fromEnv envVarName def rs = (fromMaybe def . match) <$> lookupEnv envVarName
  where
    match = (listToMaybe . catMaybes . readSomething rs =<<)


-- Command Line OCK
-- もしかしたら cTPad が正しくないかもしれない

clock :: Output -> Env -> Vty -> IO ()
clock out env@(Env {..}) vty = displayBounds out >>= draw
  where
  fx x = if forceEven then 2 * range 1 maxBound (x `div` 2)
         else range 1 maxBound x

  draw (tCols, tRows) = do
    time <- T.getZonedTime
    let picoSec = read $ T.formatTime T.defaultTimeLocale "%q" time
        dur     = (10 ^ 12 - picoSec) `div` (10 ^ 6)
        hms     = T.formatTime T.defaultTimeLocale "%T" time
        clckMtx = fromTime hms
        clckCol = (tCols - (lrMarg + lrPadd + 1) * 2) * 2
        clckRow = (tRows - (tbMarg + tbPadd + 1) * 2) * 2
        cx      = fx $ div clckCol $ M.ncols clckMtx
        rx      = fx $ div clckRow $ M.nrows clckMtx
        cm      = range 0 maxBound $ clckCol - M.ncols clckMtx * cx
        rm      = range 0 maxBound $ clckRow - M.nrows clckMtx * rx
        cTPad   = (rm `div` 2 + tbPadd * 2) `mod` 2
        cLPad   = (cm `div` 2 + lrPadd * 2) `mod` 2
        clckImg = center tRows tCols $ fourBits clckAttr
                $ matrixToFourLists' False rx cx cTPad cLPad clckMtx
        frmeImg = center tRows tCols
                $ square frmeAttr (tRows - tbMarg * 2) (tCols - lrMarg * 2)
        pic     = setBack (Background ' ' backAttr)
                $ picForImage frmeImg `addToTop` clckImg
    update vty pic
    timeout dur evLoop >>= maybe (clock out env vty) return

  evLoop = nextEvent vty >>= \case
    EvKey KEsc        _       -> return ()
    EvKey (KChar 'q') _       -> return ()
    EvKey (KChar 'c') [MCtrl] -> return ()
    EvResize cols rows        -> draw (cols, rows)
    _                         -> evLoop


padTop :: a -> Int -> M.Matrix a -> M.Matrix a
padTop def n m = M.matrix n (M.ncols m) (const def) M.<-> m

padBottom :: a -> Int -> M.Matrix a -> M.Matrix a
padBottom def n m = m M.<-> M.matrix n (M.ncols m) (const def)

-- TODO: Top Bottom


range :: (Ord a) => a -> a -> a -> a
range min max a | a < min   = min
                | a > max   = max
                | otherwise = a


center :: Int -> Int -> Image -> Image
center rHeight rWidth img = pad padLeft padTop 0 0
                          $ cropRight  rWidth
                          $ cropLeft   cropL
                          $ cropBottom rHeight
                          $ cropTop    cropT
                          $ img
  where
    iHeight     = imageHeight img
    iWidth      = imageWidth  img
    padLeft     = range 0 maxBound $ (rWidth  - iWidth)  `div` 2
    padTop      = range 0 maxBound $ (rHeight - iHeight) `div` 2
    cropL       = rWidth  + (iWidth  - rWidth)  `div` 2
    cropT       = rHeight + (iHeight - rHeight) `div` 2



sec :: IO Double
sec = read <$> T.formatTime T.defaultTimeLocale "%S%Q" <$> T.getZonedTime


withVty :: Config -> (Vty -> IO a) -> IO a
withVty cfg = bracket (mkVty cfg) shutdown


digit :: Int -> M.Matrix Bool
digit a = if q == 0 then f r else
    digit q M.<|> partition M.<|> f r
  where
    (q, r) = divMod a 10
    f b | b == 0 = _0
        | b == 1 = _1
        | b == 2 = _2
        | b == 3 = _3
        | b == 4 = _4
        | b == 5 = _5
        | b == 6 = _6
        | b == 7 = _7
        | b == 8 = _8
        | b == 9 = _9


fromTime :: String -> M.Matrix Bool
fromTime []        = mono False 0 0
fromTime (':':str) = colon M.<|> partition M.<|> fromTime str
fromTime ('0':str) = _0    M.<|> partition M.<|> fromTime str
fromTime ('1':str) = _1    M.<|> partition M.<|> fromTime str
fromTime ('2':str) = _2    M.<|> partition M.<|> fromTime str
fromTime ('3':str) = _3    M.<|> partition M.<|> fromTime str
fromTime ('4':str) = _4    M.<|> partition M.<|> fromTime str
fromTime ('5':str) = _5    M.<|> partition M.<|> fromTime str
fromTime ('6':str) = _6    M.<|> partition M.<|> fromTime str
fromTime ('7':str) = _7    M.<|> partition M.<|> fromTime str
fromTime ('8':str) = _8    M.<|> partition M.<|> fromTime str
fromTime ('9':str) = _9    M.<|> partition M.<|> fromTime str
fromTime (_  :str) = fromTime str


vertString :: Attr -> String -> Image
vertString attr = foldr (\c i -> i <-> char attr c) emptyImage

vertStrings :: Attr -> [String] -> Image
vertStrings attr = foldr (\s i -> i <-> string attr s) emptyImage


-- side margin
frame :: Int -> Int -> Attr -> Image -> Image
frame vmn hmn attr img = char attr '┌' <|> hor <|> char attr '┐'
                     <-> verL <|> (marg <-> img <-> marg) <|> verR
                     <-> char attr '└' <|> hor <|> char attr '┘'
  where
    hm   = replicate hmn ' '
    hor  = string      attr $ replicate (imageWidth img + hmn*2) '─'
    verL = vertStrings attr $ replicate (imageHeight img + vmn*2) $ "│"++hm
    verR = vertStrings attr $ replicate (imageHeight img + vmn*2) $ hm++"│"
    marg = vertStrings attr $ replicate vmn $ replicate (imageWidth img) ' '



square :: Attr -> Int -> Int -> Image
square attr v h = char attr '┌' <|> hor <|> char attr '┐'
              <-> ver           <|> mar <|> ver
              <-> char attr '└' <|> hor <|> char attr '┘'
  where
    hor = string attr $ replicate (h - 2) '─'
    ver = vertCat $ map (char attr) $ replicate (v - 2) '│'
    mar = vertCat $ replicate (v - 2) $ string attr $ replicate (h - 2) ' '





divRUp :: Integral a => a -> a -> a
divRUp x y = div (x + y - 1) y


extend :: a -> Int -> Int -> M.Matrix a -> M.Matrix a
extend def newR newC m = m''
  where
   rDiff = newR - M.nrows m
   cDiff = newC - M.ncols m
   m'    = if rDiff <= 0 then m  else m  M.<-> mono def rDiff (M.ncols m)
   m''   = if cDiff <= 0 then m' else m' M.<|> mono def (M.nrows m') cDiff


mono :: a -> Int -> Int -> M.Matrix a
mono def r c = M.matrix r c (const def)


fourBits :: Attr -> [[Four Bool]] -> Image
fourBits attr = foldMap (string attr . map fourBit')
