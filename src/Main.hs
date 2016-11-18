{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)

-- import Data.Set (Set)
import qualified Data.Set as Set

import SDL (($=))
import qualified SDL as SDL

import Linear (V2(..))

import System.IO


import Control.Wire (clockSession_)


import Curve

import Input
import Game
import Player (Player(..))
import Graphics (rgbColor)

-- TODO move into game config
screenWidth = 800
screenHeight = 600

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

  let (Player imap1 input1 curve1) = player1
      (Player imap2 input2 curve2) = player2
      player1t = (player1, (inputWire imap1), (curveWire curve1 0.7))
      player2t = (player2, (inputWire imap2), (curveWire curve2 (-2.3)))

  runGameWireTuple renderer clockSession_ [player1t, player2t]

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
