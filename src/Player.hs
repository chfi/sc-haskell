-- A Player has a Keymapping and a Curve

module Player
  ( Player(..)
  , PlayerInputWire
  , PlayerCurveWire
  , PlayerTuple
  ) where


import Control.Wire

import Curve (Curve, createCurve, curveWire)

import Input (PlayerInput, InputMap)
import qualified Input as Input

  -- Really a player shouldn't have a curve -- the curve should be entirely
  -- kept in wires
data Player = Player InputMap PlayerInput Curve
-- data Player = Player InputMap

type PlayerInputWire s = Wire s () IO PlayerInput PlayerInput
type PlayerCurveWire s = Wire s () IO (Curve, PlayerInput) Curve


type PlayerTuple s = (Player, PlayerInputWire s, PlayerCurveWire s)

-- newPlayer :: InputMap -> V2 Double -> Color -> Player
-- newPlayer imap head col = let curve = createCurve head col in


-- createPlayerTuple :: Double -> PlayerTuple s
-- createPlayerTuple ang =


-- createPlayerWire :: HasTime t s => Player -> Double -> Wire s () IO PlayerInput Curve
-- createPlayerWire (Player imap _ curve) ang = curveWire curve ang
