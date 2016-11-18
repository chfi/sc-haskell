module Game
  (runGameWireTuple)
  where

import Control.Monad (unless)
import           Control.Category
import           SDL              (($=))
import qualified SDL
import           Control.Wire (Session, stepWire, stepSession)
import           Linear           (V4 (..))
import           Prelude          hiding ((.))
import           Curve            (killCurve, Curve(..))
import qualified Input
import           Graphics.Draw    (drawCurve)
import           Player           (Player (..), PlayerTuple)

runPlayerTuple :: s
                -> [SDL.Event]
                -> PlayerTuple s
                -> IO (PlayerTuple s)
runPlayerTuple ds evs (Player imap i c, cw) = do
  let i' = Input.parseEvents imap i evs
  (Right c', cw') <- stepWire cw ds $ Right (c, i)
  return (Player imap i' c', cw')

collideTuples :: [PlayerTuple s] -> [PlayerTuple s]
collideTuples ts = fmap (\(Player imap i c, cw) ->
                          (Player imap i (killCurve c cs), cw)) ts
  where cs = fmap (\(Player _ _ c, _) -> c) ts

runGameWireTuple :: (Show s) => SDL.Renderer
                 -> Session IO s
                 -> [PlayerTuple s]
                 -> IO ()
runGameWireTuple r s ts = do
  (ds, s') <- stepSession s
  evs <- SDL.pollEvents
  ts' <- collideTuples <$> mapM (runPlayerTuple ds evs) ts

  SDL.rendererDrawColor r $= V4 0 0 0 0
  SDL.clear r
  sequence_ $ fmap (drawCurve r . \(Player _ _ c,_) -> c) ts'
  let aliveCount = length $ filter (\(Player _ _ c,_) -> True == cAlive c) ts'
  -- putStrLn $ show aliveCount
  SDL.present r

  if (aliveCount < 2) then runGameWireTuple r s' ts' else return ()


gameOver :: SDL.Renderer -> Session IO s -> IO ()
gameOver r s = do
  SDL.rendererDrawColor r $= V4 100 100 100 255
  SDL.clear r

  (ds, s') <- stepSession s

  SDL.present r
