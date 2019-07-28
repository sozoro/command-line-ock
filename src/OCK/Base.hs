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
import Numeric            (readHex)
import System.Timeout     (timeout)
import System.Environment (lookupEnv,setEnv,unsetEnv)

import qualified Data.Four as F
import Data.Matrix.Functions


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
  tbPadd    <- fromEnv "OCK_TopBottomPadding" 4 [RSome id]
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


data YesNo
  = Yes
  | No
  deriving (Show, Eq, Enum)

instance Read YesNo where
  readsPrec _ ('Y':'E':'S':str) = [(Yes,str)]
  readsPrec _ ('Y':'e':'s':str) = [(Yes,str)]
  readsPrec _ ('y':'e':'s':str) = [(Yes,str)]
  readsPrec _ ('y':str)         = [(Yes,str)]
  readsPrec _ ('N':'O':str)     = [(No, str)]
  readsPrec _ ('N':'o':str)     = [(No, str)]
  readsPrec _ ('n':'o':str)     = [(No, str)]
  readsPrec _ ('n':str)         = [(No, str)]
  readsPrec _ _                 = []


data OnOff
  = On
  | Off
  deriving (Show, Eq, Enum)

instance Read OnOff where
  readsPrec _ ('O':'N':str)     = [(On, str)]
  readsPrec _ ('O':'n':str)     = [(On, str)]
  readsPrec _ ('o':'n':str)     = [(On, str)]
  readsPrec _ ('O':'F':'F':str) = [(Off,str)]
  readsPrec _ ('O':'f':'f':str) = [(Off,str)]
  readsPrec _ ('o':'f':'f':str) = [(Off,str)]
  readsPrec _ _                 = []


data SBool
  = STrue
  | SFalse
  deriving (Show, Eq, Enum)

instance Read SBool where
  readsPrec _ ('t':'r':'u':'e':str)     = [(STrue, str)]
  readsPrec _ ('t':str)                 = [(STrue, str)]
  readsPrec _ ('f':'a':'l':'s':'e':str) = [(SFalse,str)]
  readsPrec _ ('f':str)                 = [(SFalse,str)]
  readsPrec _ _                         = []


data ReadSomething a = forall b. Read b => RSome (b -> a)

readSomething :: [ReadSomething a] -> String -> [Maybe a]
readSomething rs str = map (\(RSome f) -> fmap f $ maybeRead str) rs

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


fromEnv :: String -> a -> [ReadSomething a] -> IO a
fromEnv envVarName def rs = (fromMaybe def . match) <$> lookupEnv envVarName
  where
    match = (listToMaybe . catMaybes . readSomething rs =<<)


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
                $ matrixToFourLists False rx cx cTPad cLPad clckMtx
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


withVty :: Config -> (Vty -> IO a) -> IO a
withVty cfg = bracket (mkVty cfg) shutdown


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


square :: Attr -> Int -> Int -> Image
square attr h w = char attr '┌' <|> hor <|> char attr '┐'
              <-> ver           <|> mar <|> ver
              <-> char attr '└' <|> hor <|> char attr '┘'
  where
    hor = string attr $ replicate (w - 2) '─'
    ver = vertCat $ map (char attr) $ replicate (h - 2) '│'
    mar = charFill attr ' ' (w - 2) (h - 2)


fourBits :: Attr -> [[F.Four Bool]] -> Image
fourBits attr = foldMap (string attr . map F.fourBitToBlock)
