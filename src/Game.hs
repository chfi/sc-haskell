
-- Contains the wires for the game logic

module Game where

import Control.Arrow
import Control.Category

import Data.Set (Set)
import qualified Data.Set as Set

import SDL (($=))
import qualified SDL as SDL

import Control.Wire
import FRP.Netwire

import Linear (V4(..))

import Debug.Trace as Debug

import Curve (Curve(..), drawCurve, drawCurve', curveWire, killCurve)
import qualified Curve as Curve
import Input (PlayerInput, inputWire)
import qualified Input as Input

import Player (Player(..), PlayerInputWire, PlayerCurveWire, PlayerTuple)





-- createGameWire :: HasTime t s => [Player] -> Wire s e IO [Player] [Player]
-- createGameWire ps =


-- gameWire :: Wire s e IO [Player] [Player]
-- gameW ire =



-- runGameWire' :: SDL.Renderer -> Session IO s -> [Wire s e IO Player Player] -> IO b
-- runGameWire'


-- we should have... many input wires?
-- one per player, really.
-- should have playerwires...
-- or: [(Player, Wire s () IO PlayerInput PlayerInput, Wire s () IO (Curve, PlayerInput) Curve)]
  -- would sort of solve it without requiring a larger rewrite
-- runGameWire'' :: (Show s) => SDL.Renderer
--             -> [Player]
--             -> Session IO s
--             -> [Wire s () IO PlayerInput PlayerInput]
--             -> [Wire s () IO (Curve, PlayerInput) Curve]
--             -- -> Wire s e IO Player Player
--             -> IO b
-- runGameWire'' r ps s iws cws = do
--   SDL.rendererDrawColor r $= V4 0 0 0 0
--   SDL.clear r

--   (ds,s') <- stepSession s
--   -- get input for each wire
--   fmap (\iw -> stepWire iw ds $ undefined)
--   (Right i', _) <- stepWire iw ds $ Right i
--   -- update curves for each wire
--   (Right cs', cw') <- stepWire cw ds $ Right (c, i')
--   -- collide curves for each wire


--   -- draw each curve
--   drawCurve r c'

--   SDL.present r

--   runGameWire'' r ps' s' iws cws'

runGameWire' :: (Show s) => SDL.Renderer
            -> Player
            -> Session IO s
            -> Wire s () IO PlayerInput PlayerInput
            -> Wire s () IO (Curve, PlayerInput) Curve
            -- -> Wire s e IO Player Player
            -> IO b
runGameWire' r (Player imap i c ) s iw cw = do
  SDL.rendererDrawColor r $= V4 0 0 0 0
  SDL.clear r

  (ds,s') <- stepSession s
  -- get input
  (Right i', _) <- stepWire iw ds $ Right i
  -- update curves
  (Right c', cw') <- stepWire cw ds $ Right (c, i')
  -- collide curves

  putStrLn $ show $ ds

  drawCurve' r c'

  SDL.present r

  runGameWire' r (Player imap i' c') s' iw cw'




-- runPlayerInput :: PlayerTuple s -> (Either )
runPlayerInput :: s
               -> PlayerTuple s
               -> IO (Either () PlayerInput, PlayerInputWire s)
runPlayerInput ds ((Player imap pinput c), iw, cw) =
  stepWire iw ds $ Right pinput




runPlayerCurve :: s
               -> PlayerTuple s
               -> IO (Either () Curve, PlayerCurveWire s)
runPlayerCurve ds ((Player imap pinput c), iw, cw) =
  stepWire cw ds $ Right (c, pinput)
  -- stepWire cw ds $ Right (trace (show (cAlive c)) c, pinput)




runPlayerTuple :: s
               -> PlayerTuple s
               -> IO (PlayerTuple s)
runPlayerTuple ds pt@((Player imap i c), iw, cw) = do
  (Right i', _) <- runPlayerInput ds pt
  (Right c', cw') <- runPlayerCurve ds pt
  return ((Player imap i' c'), iw, cw')

runPlayerTuple' :: s
                -> [SDL.Event]
                -> PlayerTuple s
                -> IO (PlayerTuple s)
runPlayerTuple' ds evs pt@((Player imap i c), iw, cw) = do
  let i' = Input.parseEvents imap i evs
  -- (Right i', _) <- runPlayerInput ds pt
  (Right c', cw') <- runPlayerCurve ds pt
  return ((Player imap i' c'), iw, cw')




collideTuples :: [PlayerTuple s] -> [PlayerTuple s]
collideTuples ts = fmap (\((Player imap i c), iw, cw) ->
                           (Player imap i (killCurve c cs), iw, cw)) ts
  where cs = fmap (\((Player _ _ c), _, _) -> c) ts



runGameWireTuple :: (Show s) => SDL.Renderer
                 -> Session IO s
                 -> [(Player, PlayerInputWire s, PlayerCurveWire s)]
                 -> IO b
runGameWireTuple r s ts = do

  (ds, s') <- stepSession s

  evs <- SDL.pollEvents

  ts' <- collideTuples <$> mapM (runPlayerTuple' ds evs) ts

  -- ts' <- mapM (runPlayerTuple' ds evs) ts
  -- let ts'' = collideTuples ts'


  SDL.rendererDrawColor r $= V4 0 0 0 0
  SDL.clear r

  sequence_ $ fmap (\((Player _ _ c),_,_) -> drawCurve r c) ts'

  SDL.present r

  runGameWireTuple r s' ts'




-- playerToTuple :: Player -> (Player, PlayerInputWire s, PlayerCurveWire s)
-- playerToTuple p@(Player imap input curve) =
--   (p, inputWire imap, curveWire' curve 0)

-- this could be done as a reader monad I guess... the player inputmap and inputwire are constant
runGameWire :: (Show s) => SDL.Renderer
            -> Player
            -> Session IO s
            -> Wire s () IO PlayerInput PlayerInput
            -> Wire s () IO PlayerInput Curve
            -- -> Wire s e IO Player Player
            -> IO b
runGameWire r (Player imap i c ) s iw cw = do
  SDL.rendererDrawColor r $= V4 0 0 0 0
  SDL.clear r

  (ds,s') <- stepSession s
  (Right i', _) <- stepWire iw ds $ Right i
  (Right c', cw') <- stepWire cw ds $ Right i

  putStrLn $ show $ cAlive c'

  drawCurve r c'

  SDL.present r

  runGameWire r (Player imap i' c') s' iw cw'
  -- return ()


  -- step the player wire


  -- step all player wires independently
  -- mapM (\wire' -> stepWire wire' ds $ )


  -- collide playerwires & update alive status???
  -- or should all that be done in a single wire. just all the player wires combined in one. maybe.
  -- or use events for this? ugh.


  -- (Right ev,_) <- stepWire inputWire ds $ Right keys
  -- (Right p',w') <- stepWire w ds $ Right ev
  -- CM.when (playerCheckCollision p' (playerSafeTail p')) (putStrLn "Player colliding")

  -- SDL.rendererDrawColor r $= V4 0 0 maxBound maxBound
  -- drawPlayer r p'
  -- SDL.present r

  -- SDL.delay (1000 `div` 10)
  -- runGameWire r ps' s' w'



-- runGameWire
--     :: Set SDL.Keysym
--     -> SDL.Renderer
--     -> Session IO s
--     -> Wire s e IO PlayerInput Curve
--     -- -> Wire s e IO (Set SDL.Keysym) Player
--     -> IO b
-- runGameWire = undefined
