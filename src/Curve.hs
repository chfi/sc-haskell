{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}


module Curve
  (Curve(..)
  ,curveWire
  ) where


import           Data.List.NonEmpty (NonEmpty ((:|)), cons)
import qualified Data.List.NonEmpty as NE
import           Linear.Metric      (distance)
import           Linear.V2          (V2, angle)
import           Prelude                  hiding (id, (.))
import qualified Data.Set as Set


import Control.Wire (HasTime, Wire, (.))
import FRP.Netwire


import Input

  -- TODO should be configurable somewhere
curveRadius = 5


type Head = V2 Double
type Tail = NonEmpty (V2 Double)


data Curve = Curve
    { cHead :: Head
    , cTail :: Tail
    , cDist :: Double
    , cAlive :: Bool
    }



curveExtendTail :: Curve -> Curve
curveExtendTail c@Curve{..} =
  let d = distance (NE.head cTail) cHead
      cT' = if d > cDist then cHead `cons` cTail else cTail
  in c { cTail = cT' }


curveSafeTail :: Curve -> [V2 Double]
curveSafeTail Curve{..} =
  NE.dropWhile (\th -> distance cHead th < curveRadius*2) cTail


curveCheckCollision :: Curve -> [V2 Double] -> Bool
curveCheckCollision Curve{..} c2t =
  any (\th -> distance cHead th < curveRadius*2) c2t


selfColWire :: HasTime t s => Wire s () IO Curve Bool
selfColWire = proc c -> do
  let tl = curveSafeTail c
      val = curveCheckCollision c tl
  returnA -< val


curveWire :: HasTime t s => Curve -> Wire s () IO PlayerInput Curve
curveWire c = proc keys -> do
  --
  cH <- posWire (cHead c) -< keys
  rec cT <- delay (cTail c) -< cT'
      let c' = curveExtendTail c { cHead = cH, cTail = cT }
          cT' = cTail c'
  alive <- (arr not) . selfColWire -< c'
  returnA -< c' { cAlive = alive }


posWire :: HasTime t s => (V2 Double) -> Wire s () IO PlayerInput (V2 Double)
posWire init = proc keys -> do
  pos <- integral init . aliveVelWire -< keys
  returnA -< pos


-- TODO the turning rate shouldn't be hardcoded
aliveVelWire :: HasTime t s => Wire s () IO PlayerInput (V2 Double)
aliveVelWire = (angWire pi >>> polarWire)

deadVelWire :: HasTime t s => Wire s () IO a (V2 Double)
deadVelWire = returnA 0

angWire :: HasTime t s => Double -> Wire s () IO PlayerInput Double
angWire da = proc keys -> do
  -- pretty ugly --- need to create constant arrows to feed the speed, since it's a double.
  va <-  (arr (const (-da))) . when (isDown KeyLeft)
     <|> (arr (const   da )) . when (isDown KeyRight)
     <|> 0
          -< keys

  integral 0 -< va

-- wire that takes an angle and returns a cartesian vector
-- i suppose this should return a normalized movement vector
polarWire :: HasTime t s => Wire s () IO Double (V2 Double)
polarWire = proc ang -> do
  v <- arr angle -< ang
  returnA -< (v * 100)
