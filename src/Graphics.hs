{-# LANGUAGE RecordWildCards #-}

module Graphics
  ( drawCircleAt
  , drawWhiteCircleAt
  , createColoredCircleTexture
  , Color
  , rgbColor
  ) where


import Debug.Trace (trace)
import Control.Applicative

import Data.Word (Word8)

import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))
import Linear.Metric (distance)

import SDL (($=))
import qualified SDL as SDL

-- import Curve (Curve(..))
-- import qualified Curve as Curve


data Color = Color Word8 Word8 Word8


rgbColor :: Int -> Int -> Int -> Color
rgbColor r g b = Color (fromIntegral r) (fromIntegral g) (fromIntegral b)

colorToSdl :: Color -> V4 Word8
colorToSdl (Color r g b) = V4 r g b maxBound


-- TODO: fix magic numbers for circle radius
createColoredCircleTexture :: SDL.Renderer -> Color -> IO SDL.Texture
createColoredCircleTexture r c = do
  -- putStrLn "creating texture"
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

  SDL.rendererDrawColor r $= colorToSdl c
  sequence_ $ fmap (\(x,y) -> SDL.drawPoint r (P $ V2 x y)) points

  SDL.rendererRenderTarget r $= Nothing
  SDL.textureBlendMode t $= SDL.BlendAlphaBlend
  return t

circleTexture :: SDL.Renderer -> IO SDL.Texture
circleTexture r = createColoredCircleTexture r (rgbColor 255 255 255)



drawWhiteCircleAt :: (RealFrac a) => SDL.Renderer -> V2 a -> IO ()
drawWhiteCircleAt r p = do
  -- putStrLn "drawing"
  let p' = fmap round p
  t <- circleTexture r
  let c = Just $ SDL.Rectangle (P $ p') (V2 10 10) -- TODO magic numbers...
  SDL.copy r t Nothing c



-- drawCircleAt' :: (RealFrac a) => SDL.Renderer -> Color -> (V2 a -> IO ())
-- drawCircleAt' r c = do
--   -- let p' = fmap round p
--   t <- createColoredCircleTexture r c
--   let getRect x = Just $ SDL.Rectangle (P $ x') (V2 10 10) -- TODO magic numbers...
--           where x' = fmap round x
--   \x -> SDL.copy r t Nothing (getRect x)

drawCircleAt :: (RealFrac a) => SDL.Renderer -> Color -> V2 a -> IO ()
drawCircleAt r c p = do
  let p' = fmap round p
  t <- createColoredCircleTexture r c
  let c = Just $ SDL.Rectangle (P $ p') (V2 10 10) -- TODO magic numbers...
  SDL.copy r t Nothing c
