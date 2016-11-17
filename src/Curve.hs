{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}


module Curve where


import           Data.List.NonEmpty (NonEmpty ((:|)), cons)
import qualified Data.List.NonEmpty as NE
import           Linear.Metric      (distance)
import           Linear.V2          (V2, angle)
import           Prelude                  hiding (id, (.))
import qualified Data.Set as Set


import Control.Wire (HasTime, Wire, (.))
import FRP.Netwire


import Graphics (Color, rgbColor)
import Input

  -- TODO should be configurable somewhere
curveRadius = 5


type Head = V2 Double
type Tail = NonEmpty (V2 Double)


data Curve = Curve { cHead  :: Head
                     , cTail  :: Tail
                     , cDist  :: Double
                     , cAlive :: Bool
                     , cColor :: Color
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


selfColWire :: HasTime t s => Wire s () IO Curve Bool
selfColWire = proc p -> do
  let tl = curveSafeTail p
      val = curveCheckCollision p tl
  returnA -< val


plWire :: HasTime t s => Curve -> Wire s () IO CurveInput Curve
plWire p = proc keys -> do
  pH <- plPosWire (cHead p) -< keys
  rec pT <- delay (cTail p) -< pT'
      let p' = curveExtendTail p { cHead = pH, cTail = pT }
          pT' = cTail p'
  alive <- (arr not) . selfColWire -< p'
  returnA -< p' { cAlive = alive }



plPosWire :: HasTime t s => (V2 Double) -> Wire s () IO CurveInput (V2 Double)
plPosWire init = proc keys -> do
  -- pos <- integral init . (angvel >>> polarVel) -< keys
  pos <- integral init . (angWire >>> polarVel) -< keys
  returnA -< pos

angWire :: HasTime t s => Double -> Wire s () IO CurveInput Double
angWire da = proc keys -> do
  -- pretty ugly --- need to create constant arrows to feed the speed, since it's a double.
  va <-  (arr (const (-da))) . when (isDown KeyLeft)
     <|> (arr (const   da )) . when (isDown KeyRight)
     <|> 0
          -< keys

  integral 0 -< va


-- angvel :: HasTime t s => Wire s () IO CurveInput Double
-- angvel = proc keys -> do
--   va <-  (-3) . when (isDown KeyLeft)
--      <|>   3  . when (isDown KeyRight)
--      <|> 0
--           -< keys
--   va' <- integral 0 -< va
--   returnA -< va'

-- wire that takes an angle and returns a cartesian vector
-- i suppose this should return a normalized movement vector
polarVel :: HasTime t s => Wire s () IO Double (V2 Double)
polarVel = proc ang -> do
  v <- arr angle -< ang
  returnA -< (v * 100)
