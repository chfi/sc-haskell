module Graphics.Texture
  ( Color
  , rgbColor
  , createColoredCircleTexture
  ) where

import Control.Applicative

import Data.Word (Word8)

import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))
import Linear.Metric (distance)

import SDL (($=))
import qualified SDL as SDL

data Color = Color Word8 Word8 Word8


rgbColor :: Int -> Int -> Int -> Color
rgbColor r g b = Color (fromIntegral r) (fromIntegral g) (fromIntegral b)

colorToSdl :: Color -> V4 Word8
colorToSdl (Color r g b) = V4 r g b maxBound

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
