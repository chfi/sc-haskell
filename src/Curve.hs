{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}


module Curve
  ( Curve(..)
  , createCurve
  , curveWire
  , drawCurve
  , drawCurve'
  , killCurve
  ) where


import Debug.Trace (trace)

import           Data.List.NonEmpty (NonEmpty ((:|)), cons)
import qualified Data.List.NonEmpty as NE
import Linear.Affine (Point(..))
import           Linear.Metric      (distance)
import           Linear.V2          (V2(..), angle)
import           Prelude                  hiding (id, (.))
import qualified Data.Set as Set


import Control.Wire (HasTime, Wire, (.), mkGen_, mkConst)
import FRP.Netwire


import qualified SDL as SDL


import Graphics (Color, rgbColor, drawCircleAt, drawWhiteCircleAt, createColoredCircleTexture)
import Input

  -- TODO should be configurable somewhere
curveRadius = 5
curveSpeed = 70


type Head = V2 Double
type Tail = NonEmpty (V2 Double)


data Curve = Curve { cHead  :: Head
                   , cTail  :: Tail
                   , cDist  :: Double
                   , cAlive :: Bool
                   , cColor :: Color
                   }

-- type CurveWire = Wire s e IO PlayerInput Curve

createCurve :: Head -> Color -> Curve
createCurve head color =
  Curve { cHead = head
        , cTail = head :| []
        , cDist = curveRadius / 2
        , cAlive = True
        , cColor = color
        }



curveExtendTail :: Curve -> Curve
curveExtendTail p@Curve{..} =
  let d = distance (NE.head cTail) cHead
      pT' = if d > cDist then cHead `cons` cTail else cTail
  in p { cTail = pT' }


curveSafeTail :: Curve -> [V2 Double]
curveSafeTail Curve{..} =
  NE.dropWhile (\th -> distance cHead th < curveRadius*2) cTail


curveCheckCollision :: Curve -> [V2 Double] -> Bool
curveCheckCollision Curve{..} p2t =
  any (\th -> distance cHead th < curveRadius*2) p2t


headToCurveCheckCollision :: Head -> Curve -> Bool
headToCurveCheckCollision h c@Curve{..} =
  any (\th -> distance h th < curveRadius * 2) $ curveSafeTail c


selfColWire :: HasTime t s => Wire s () IO Curve Bool
selfColWire = proc p -> do
  let tl = curveSafeTail p
      val = curveCheckCollision p tl
  returnA -< val


collideAgainstMany :: Curve -> [Curve] -> Bool
collideAgainstMany c cs =
  any (\c' -> headToCurveCheckCollision (cHead c) c') cs



killCurve :: Curve -> [Curve] -> Curve
killCurve c cs = c { cAlive = not (collideAgainstMany c cs )}


killableWire :: HasTime t s => Double -> Wire s () IO (PlayerInput, Bool) (V2 Double)
killableWire ang = proc (input, alive) -> do
  if alive
    then do velWire ang -< input
    else 0 -< ()


curveWire :: HasTime t s => Curve -> Double -> Wire s () IO (Curve, PlayerInput) Curve
curveWire c initAng = proc (curve, keys) -> do
  rec cT <- delay (cTail c) -< cT'
      cH <- integral (cHead c) . (killableWire initAng) -< (keys, cAlive curve)

      let c' = curveExtendTail c { cHead = cH, cTail = cT }
          cT' = cTail c'

  returnA -< c'

  -- feed the player angle wire into a wire which transforms it into
  -- a cartesian normalized vector and then multiply by curveSpeed
  -- to get movement vector
velWire :: HasTime t s =>  Double -> Wire s () IO PlayerInput (V2 Double)
velWire initAng = angWire initAng >>> arr angle >>> arr (* curveSpeed)


angWire :: HasTime t s => Double -> Wire s () IO PlayerInput Double
angWire initAng = proc keys -> do
  va <-  (-3) . when (isDown KeyLeft)
     <|> 3 . when (isDown KeyRight)
     <|> 0
          -< keys
  integral initAng -< va



  -- faster but uglier code
drawCurve :: SDL.Renderer -> Curve -> IO ()
drawCurve r Curve{..} = do
  t <- createColoredCircleTexture r cColor

  let getRect v = Just $ SDL.Rectangle (P $ fmap round v) (V2 10 10)
      draw x = SDL.copy r t Nothing x
  draw (getRect cHead)
  sequence_ $ fmap (draw . getRect) cTail

-- should probably move Color into a separate module,
-- or this into a separate Rendering module,
-- either way it shouldn't be in Curve
drawCurve' :: SDL.Renderer -> Curve -> IO ()
drawCurve' r Curve{..} = do
  let draw x = drawWhiteCircleAt r x
  draw cHead
  sequence_ $ fmap draw cTail
  -- drawWhiteCircleAt r cHead
  -- sequence_ $ fmap (drawWhiteCircleAt r ) cTail
  -- mapM_ (drawWhiteCircleAt r ) cTail
  -- drawCircleAt r cColor cHead
  -- mapM_ (drawCircleAt r cColor) cTail
