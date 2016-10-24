{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}


module Main where

import qualified Data.Vector.Storable.Mutable as Vec

import Control.Arrow
import Control.Category
import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty((:|)), cons)
import qualified Data.List.NonEmpty as NE
import Foreign.C.Types
import Linear
import Linear.Affine (P(..))
import Linear.Metric (distance)
import SDL (($=))
import qualified SDL
import qualified SDL.Input.Keyboard.Codes as Key
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Wire hiding (when)
-- import FRP.Netwire

import System.IO
import Prelude hiding ((.), id)

import Curve (Curve, Curve(..), curveWire)
import qualified Curve

import Input (PlayerInput, inputWire)
-- import Control.Applicative

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)


circleTexture :: SDL.Renderer -> IO SDL.Texture
circleTexture r = do
  t <- SDL.createTexture r SDL.RGBA8888 SDL.TextureAccessTarget (V2 20 20)
  SDL.rendererRenderTarget r $= Just t

  SDL.rendererDrawColor r $= V4 0 0 0 0
  SDL.clear r
  let w = 20 :: CInt
      h = 20 :: CInt
      p = 4 :: CInt
      inCircle :: V2 CInt -> Bool
      inCircle p = distance (fmap fromIntegral p) (V2 10 10) < 10
      points = [(x,y) | x <- [0..19], y <- [0..19], inCircle (V2 x y)]

  SDL.rendererDrawColor r $= V4 maxBound maxBound maxBound maxBound
  sequence_ $ fmap (\(x,y) -> SDL.drawPoint r (P $ V2 x y)) points

  SDL.rendererRenderTarget r $= Nothing
  SDL.textureBlendMode t $= SDL.BlendAlphaBlend
  return t


  -- TODO move this to Curve.hs or some other module
drawPlayer :: SDL.Renderer -> Curve -> IO ()
drawPlayer r Curve{..} = do
  t <- circleTexture r
  let hd = Just $ SDL.Rectangle (P $ fmap round cHead) (V2 10 10)
      tl = fmap (\c -> Just $ SDL.Rectangle (P $ fmap round c) (V2 10 10)) cTail
      draw' x = SDL.copy r t Nothing x
  draw' hd
  sequence_ (fmap draw' tl)
  return ()


circleSurf :: IO SDL.Surface
circleSurf = do
  let w = 20 :: CInt
      h = 20 :: CInt
      p = 4 :: CInt
      inCircle x y = (x*x + y*y) < 25
  vec <- Vec.new (fromIntegral (w*h*w*p))
  Vec.set vec 0x00
  let points = [(x,y) | x <- [0..19], y <- [0..19], inCircle x y]
      drawP i v = sequence_ $ fmap (\x -> Vec.write v x 0xFF) i
      is = [[i, i+1, i+2, i+3] | i <- [0..20*20], i `mod` 5 == 0]
  sequence_ $ fmap (\x -> drawP x vec) is
  SDL.createRGBSurfaceFrom vec (V2 w h) (w*p) SDL.RGBA8888


-- Main loop
runGameWire
    :: PlayerInput
    -> SDL.Renderer
    -> Session IO s
    -> Wire s e IO PlayerInput Curve
    -> IO b
runGameWire keys r s w = do
    SDL.rendererDrawColor r $= V4 0 0 0 0
    SDL.clear r
    (ds,s') <- stepSession s
    (Right ev,_) <- stepWire inputWire ds $ Right keys
    (Right p',w') <- stepWire w ds $ Right ev
    -- when (playerCheckCollision p' (playerSafeTail p')) (putStrLn "Player colliding")
    SDL.rendererDrawColor r $= V4 0 0 maxBound maxBound
    drawPlayer r p'
    SDL.present r
    -- SDL.delay (1000 `div` 10)
    runGameWire ev r s' w'


curve :: Curve
curve =
    Curve
    { cHead = (V2 300 300)
    , cTail = (V2 300 300) :| []
    , cDist = 1
    , cAlive = True
    }


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality
       when (renderQuality /= SDL.ScaleLinear) $
           putStrLn "Warning: Linear texture filtering not enabled!"
    window <-
        SDL.createWindow
            "SDL Tutorial"
            SDL.defaultWindow
            { SDL.windowInitialSize = V2 screenWidth screenHeight
            }
    SDL.showWindow window
    renderer <-
        SDL.createRenderer
            window
            (-1)
            SDL.RendererConfig
            { SDL.rendererType = SDL.AcceleratedVSyncRenderer
            , SDL.rendererTargetTexture = False
            }
    _ <- runGameWire Set.empty renderer clockSession_ $ curveWire curve
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
