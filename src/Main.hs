{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad    (when)
import           Control.Wire     (clockSession_)
import qualified Data.Set         as Set
import           Foreign.C.Types  (CInt)
import           Linear           (V2 (..))
import           SDL              (($=))
import qualified SDL              as SDL
import           System.IO

import           Curve            (createCurve, curveWire)
import           Game             (runGameWireTuple)
import           Graphics.Texture (rgbColor)
import           Input            (adInputMap, fgInputMap)
import           Player           (Player (..))


-- TODO move into game config
screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600


player1, player2 :: Player
player1 = Player adInputMap Set.empty (createCurve (V2 30.0 30.0) (rgbColor 255 0 0))
player2 = Player fgInputMap Set.empty (createCurve (V2 770.0 570.0) (rgbColor 0 255 0))


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

  let (Player _ _ curve1) = player1
      (Player _ _ curve2) = player2
      player1t = (player1, (curveWire curve1 0.7))
      player2t = (player2, (curveWire curve2 (-2.3)))

  _ <- runGameWireTuple renderer clockSession_ [player1t, player2t]

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
