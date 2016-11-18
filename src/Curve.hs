{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}


module Curve
  ( Curve(..)
  , createCurve
  , curveWire
  , drawCurve
  ) where


import Debug.Trace (trace)

import           Data.List.NonEmpty (NonEmpty ((:|)), cons)
import qualified Data.List.NonEmpty as NE
import           Linear.Metric      (distance)
import           Linear.V2          (V2(..), angle)
import           Prelude                  hiding (id, (.))
import qualified Data.Set as Set


import Control.Wire (HasTime, Wire, (.), mkGen_, mkConst)
import FRP.Netwire


import qualified SDL as SDL


import Graphics (Color, rgbColor, drawCircleAt)
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

-- type CurveWire = Wire s e IO PlayerInput Curve

createCurve :: Head -> Color -> Curve
createCurve head color =
  Curve { cHead = head
        , cTail = head :| []
        , cDist = curveRadius
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


selfColWire :: HasTime t s => Wire s () IO Curve Bool
selfColWire = proc p -> do
  let tl = curveSafeTail p
      val = curveCheckCollision p tl
  returnA -< val



killableWire :: HasTime t s => Wire s () IO (PlayerInput, Bool) (V2 Double)
killableWire = proc (input, alive) -> do
  if alive
    then do posWire (V2 100 100) 0 -< input
    else 0 -< ()


curveWire :: HasTime t s => Curve -> Double -> Wire s () IO PlayerInput Curve
curveWire c initAng = proc keys -> do

  rec cT <- delay (cTail c) -< cT'
      a' <- delay True -< alive
      cH <- integral 0 . killableWire -< (keys, a')

      let c' = curveExtendTail c { cHead = cH, cTail = cT }
          cT' = cTail c'

      alive <- ((arr not) . selfColWire) -< c'

  returnA -< c' { cAlive = alive }


posWire :: HasTime t s => (V2 Double) -> Double -> Wire s () IO PlayerInput (V2 Double)
posWire initPos initAng = proc keys -> do
  pos <- ((angWire initAng) >>> polarVel) -< keys
  returnA -< pos


angWire :: HasTime t s => Double -> Wire s () IO PlayerInput Double
angWire init = proc keys -> do
  va <-  (-3) . when (isDown KeyLeft)
     <|> 3 . when (isDown KeyRight)
     <|> 0
          -< keys
  integral init -< va


angWire' :: HasTime t s => Double -> Wire s () IO PlayerInput Double
angWire' da = proc keys -> do
  -- pretty ugly --- need to create constant arrows to feed the speed, since it's a double.
  va <-  (arr (const (-da))) . when (isDown KeyLeft)
     <|> (arr (const   da )) . when (isDown KeyRight)
     <|> 0
          -< keys
  integral 0 -< va


-- angvel :: HasTime t s => Wire s () IO PlayerInput Double
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


-- should probably move Color into a separate module,
-- or this into a separate Rendering module,
-- either way it shouldn't be in Curve
drawCurve :: SDL.Renderer -> Curve -> IO ()
drawCurve r Curve{..} = do
  drawCircleAt r cColor cHead
  mapM_ (drawCircleAt r cColor) cTail
