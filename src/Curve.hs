{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}


module Curve
  (Curve(..)
  ,createCurve
  ,curveWire
  ,killCurve
  ,curveToPoints)
  where


import           Data.List.NonEmpty (NonEmpty ((:|)), cons)
import qualified Data.List.NonEmpty as NE
import           Linear.Metric      (distance)
import           Linear.V2          (V2(..), angle)
import           Prelude                  hiding (id, (.))

import Control.Wire (HasTime, Wire, (.))
import FRP.Netwire

import Graphics.Texture (Color)
import Input

  -- TODO should be configurable somewhere
curveRadius :: Double
curveRadius = 5

curveSpeed :: Double
curveSpeed = 70


type Head = V2 Double
type Tail = NonEmpty (V2 Double)


data Curve = Curve { cHead  :: Head
                   , cTail  :: Tail
                   , cDist  :: Double
                   , cAlive :: Bool
                   , cColor :: Color
                   }


curveToPoints :: Curve -> [V2 Double]
curveToPoints Curve{..} = cHead : NE.toList cTail


createCurve :: Head -> Color -> Curve
createCurve pos color =
  Curve { cHead = pos
        , cTail = pos :| []
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
    NE.dropWhile
        (\th ->
              distance cHead th < curveRadius * 2)
        cTail


headToCurveCheckCollision :: Head -> Curve -> Bool
headToCurveCheckCollision h c@Curve{..} =
  any (\th -> distance h th < curveRadius * 2) $ curveSafeTail c


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
velWire initAng = angWire initAng >>> arr angle >>> arr (* pure curveSpeed)


angWire :: HasTime t s => Double -> Wire s () IO PlayerInput Double
angWire initAng = proc keys -> do
  va <-  (-3) . when (isDown KeyLeft)
     <|> 3 . when (isDown KeyRight)
     <|> 0
          -< keys
  integral initAng -< va
