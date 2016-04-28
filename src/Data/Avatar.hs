{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ConstraintKinds    #-}
module Data.Avatar (bilinear, resize, dynamicResize, dynamicPixelMap') where

import Codec.Picture
import Codec.Picture.Types
import Data.Word

type IntegralPixel a = (Integral (PixelBaseComponent a), Pixel a)

-- From: http://qiita.com/fumieval/items/2c761afb18e65c1fad06
bilinear :: IntegralPixel a => a -> a -> a -> a -> Float -> Float -> a
bilinear p q r s u v = mixWith (f v) (mixWith (f u) p q) (mixWith (f u) r s)
  where
    f t _ x y = floor $ fromIntegral x * (1 - t) + fromIntegral y * t

-- From: http://qiita.com/fumieval/items/2c761afb18e65c1fad06
resize ::  IntegralPixel a => (Int, Int) -> Image a -> Image a
resize (w, h) img@(Image w0 h0 _) = generateImage f w h where
    f x y = let x' = fromIntegral x / fromIntegral w * fromIntegral w0
                y' = fromIntegral y / fromIntegral h * fromIntegral h0
            in bilinear
                (pixelAt img (floor x') (floor y'))
                (pixelAt img (floor x' + 1) (floor y'))
                (pixelAt img (floor x') (floor y' + 1))
                (pixelAt img (floor x' + 1) (floor y' + 1))
                (x' - fromInteger (floor x'))
                (y' - fromInteger (floor y'))

-- From Codec.Picture.Saving
componentToLDR :: Float -> Word8
componentToLDR = truncate . (255 *) . min 1.0 . max 0.0

-- From Codec.Picture.Saving
toStandardDef :: Image PixelRGBF -> Image PixelRGB8
toStandardDef = pixelMap pixelConverter
  where pixelConverter (PixelRGBF rf gf bf) = PixelRGB8 r g b
          where r = componentToLDR rf
                g = componentToLDR gf
                b = componentToLDR bf

-- From Codec.Picture.Saving
greyScaleToStandardDef :: Image PixelF -> Image Pixel8
greyScaleToStandardDef = pixelMap componentToLDR

dynamicResize :: (Int, Int) -> DynamicImage -> DynamicImage
dynamicResize d = dynamicPixelMap' (resize d)

-- Derived from Codec.Picture.dynamicPixelMap, converting components with floats to 8bit
-- using the same functions as Codec.Picture.Saving.imageToPng.
dynamicPixelMap' :: (forall pixel . IntegralPixel pixel => Image pixel -> Image pixel)
                -> DynamicImage -> DynamicImage
dynamicPixelMap' f = aux
  where
    aux (ImageY8     i) = ImageY8     $ f i
    aux (ImageY16    i) = ImageY16    $ f i
    aux (ImageYF     i) = ImageY8     $ f (greyScaleToStandardDef i)
    aux (ImageYA8    i) = ImageYA8    $ f i
    aux (ImageYA16   i) = ImageYA16   $ f i
    aux (ImageRGB8   i) = ImageRGB8   $ f i
    aux (ImageRGB16  i) = ImageRGB16  $ f i
    aux (ImageRGBF   i) = ImageRGB8   $ f (toStandardDef (promoteImage i))
    aux (ImageRGBA8  i) = ImageRGBA8  $ f i
    aux (ImageRGBA16 i) = ImageRGBA16 $ f i
    aux (ImageYCbCr8 i) = ImageYCbCr8 $ f i
    aux (ImageCMYK8  i) = ImageCMYK8  $ f i
    aux (ImageCMYK16 i) = ImageCMYK16 $ f i
