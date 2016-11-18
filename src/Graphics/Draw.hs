module Graphics.Draw
  ( drawCircle
  , drawCircles
  , drawCurve
  ) where

import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))
import Linear.Metric (distance)

import SDL (($=))
import qualified SDL as SDL


import Curve (Curve(..), curveToPoints)
import Graphics.Texture (Color, createColoredCircleTexture)

drawCircle :: (RealFrac a) => SDL.Renderer -> Color -> V2 a -> IO ()
drawCircle r c p = do
  let p' = fmap round p
  t <- createColoredCircleTexture r c
  let c = Just $ SDL.Rectangle (P $ p') (V2 10 10) -- TODO magic numbers...
  SDL.copy r t Nothing c


drawCircles :: (RealFrac a) => SDL.Renderer -> Color -> [V2 a] -> IO ()
drawCircles r c ps = do
  t <- createColoredCircleTexture r c
  let getRect v = Just $ SDL.Rectangle (P $ fmap round v) (V2 10 10)
      draw x = SDL.copy r t Nothing x
  sequence_ $ fmap (draw . getRect) ps


drawCurve :: SDL.Renderer -> Curve -> IO ()
drawCurve r c = drawCircles r (cColor c) (curveToPoints c)
