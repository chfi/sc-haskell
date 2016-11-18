module Player
  (Player(..)
  ,PlayerInputWire
  ,PlayerCurveWire
  ,PlayerTuple)
  where

import           Control.Wire
import           Curve        (Curve)
import           Input        (InputMap, PlayerInput)

-- Really a player shouldn't have a curve -- the curve should be entirely
-- kept in wires
data Player = Player InputMap PlayerInput Curve

type PlayerInputWire s = Wire s () IO PlayerInput PlayerInput
type PlayerCurveWire s = Wire s () IO (Curve, PlayerInput) Curve


type PlayerTuple s = (Player, PlayerCurveWire s)
