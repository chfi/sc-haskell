{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}


module Player where


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
playerRadius = 5


type Head = V2 Double
type Tail = NonEmpty (V2 Double)


data Player = Player { pHead  :: Head
                     , pTail  :: Tail
                     , pDist  :: Double
                     , pAlive :: Bool
                     }



playerExtendTail :: Player -> Player
playerExtendTail p@Player{..} =
  let d = distance (NE.head pTail) pHead
      pT' = if d > pDist then pHead `cons` pTail else pTail
  in p { pTail = pT' }


playerSafeTail :: Player -> [V2 Double]
playerSafeTail Player{..} =
  NE.dropWhile (\th -> distance pHead th < playerRadius*2) pTail


playerCheckCollision :: Player -> [V2 Double] -> Bool
playerCheckCollision Player{..} p2t =
  any (\th -> distance pHead th < playerRadius*2) p2t


selfColWire :: HasTime t s => Wire s () IO Player Bool
selfColWire = proc p -> do
  let tl = playerSafeTail p
      val = playerCheckCollision p tl
  returnA -< val


plWire :: HasTime t s => Player -> Wire s () IO PlayerInput Player
plWire p = proc keys -> do
  pH <- plPosWire (pHead p) -< keys
  rec pT <- delay (pTail p) -< pT'
      let p' = playerExtendTail p { pHead = pH, pTail = pT }
          pT' = pTail p'
  alive <- (arr not) . selfColWire -< p'
  returnA -< p' { pAlive = alive }



plPosWire :: HasTime t s => (V2 Double) -> Wire s () IO PlayerInput (V2 Double)
plPosWire init = proc keys -> do
  -- pos <- integral init . (angvel >>> polarVel) -< keys
  pos <- integral init . (angWire >>> polarVel) -< keys
  returnA -< pos

angWire :: HasTime t s => Double -> Wire s () IO PlayerInput Double
angWire da = proc keys -> do
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
