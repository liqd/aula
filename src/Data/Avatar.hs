{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Data.Avatar
    (-- * main
      makeAvatar

    -- unused
    , dynamicResize

    -- * internals
    , resize
    , bilinear
    , dynamicPixelMap'
    , scaleUp
    , scaleDown
    , dist
    , clipCircle
    , clipSquare) where

import Codec.Picture
import Codec.Picture.Types

type IntegralPixel a = (Integral (PixelBaseComponent a), Pixel a)

-- From: http://qiita.com/fumieval/items/2c761afb18e65c1fad06
bilinear :: IntegralPixel a => a -> a -> a -> a -> Float -> Float -> a
bilinear p q r s u v = mixWith (f v) (mixWith (f u) p q) (mixWith (f u) r s)
  where
    f t _ x y = floor $ fromIntegral x * (1 - t) + fromIntegral y * t

dist :: (Integral a, Floating d, p ~ (a, a)) => p -> p -> d
dist (x0,y0) (x1,y1) = sqrt (sqr (x1 - x0) + sqr (y1 - y0))
    where sqr x = fromIntegral x ^ (2 :: Int)

clipSquare :: Pixel a => Image a -> Image a
clipSquare img@(Image w h _) = generateImage f d d where
    d = min w h
    padx = (w - d) `div` 2
    pady = (h - d) `div` 2
    -- x in 0..w-1
    -- y in 0..h-1
    f x y = pixelAt img (padx + x) (pady + y)

-- I must have missed something in JuicyPixels
class Pixel a => ChangeOpacity a where
    changeOpacity :: (PixelBaseComponent a -> PixelBaseComponent a) -> a -> a

instance ChangeOpacity PixelRGBA16 where
    changeOpacity f (PixelRGBA16 r g b a) = PixelRGBA16 r g b (f a)

-- The `changeOpacity` opacity function takes an opacity value ranging from 0 to 1.
clipCircle :: (ChangeOpacity a, Integral (PixelBaseComponent a), Bounded (PixelBaseComponent a))
           => Image a -> Image a
clipCircle img@(Image w h _) = generateImage f d d where
    center = (w `div` 2, h `div` 2)
    d = min w h
    radius = d `div` 2 - max (d `div` 80) 2
    -- x in 0..w-1
    -- y in 0..h-1
    f x y {-| inCircle x y = pixelAt img x y
          | otherwise    =-}
        = changeOpacity newOpacity (pixelAt img x y)
      where
        z = dist center (x, y) - fromIntegral radius
        newOpacity old
            | z < 0     = old
            | otherwise = componentToLDR (1 / z ** 3)

-- From: http://qiita.com/fumieval/items/2c761afb18e65c1fad06
scaleDown :: IntegralPixel a => (Int, Int) -> Image a -> Image a
scaleDown (w, h) img@(Image w0 h0 _) = generateImage f w h where
    -- x in 0..w-1
    -- y in 0..h-1
    f x y = let x' = min (fromIntegral $ w0 - 1) (fromIntegral x * fromIntegral w0 / fromIntegral w)
                y' = min (fromIntegral $ h0 - 1) (fromIntegral y * fromIntegral h0 / fromIntegral h)
            in bilinear
                (pixelAt img (floor x') (floor y'))
                (pixelAt img (floor x' + 1) (floor y'))
                (pixelAt img (floor x') (floor y' + 1))
                (pixelAt img (floor x' + 1) (floor y' + 1))
                (x' - fromInteger (floor x'))
                (y' - fromInteger (floor y'))

scaleUp :: Pixel a => (Int,Int) -> Image a -> Image a
scaleUp (w, h) img@(Image w0 h0 _) = generateImage f w h where
    f x y = let x' :: Float = fromIntegral x * fromIntegral w0 / fromIntegral w
                y' :: Float = fromIntegral y * fromIntegral h0 / fromIntegral h
            in pixelAt img (floor x') (floor y')

resize :: IntegralPixel a => (Int, Int) -> Image a -> Image a
resize (w, h) img@(Image w0 h0 _)
    | w == w0 && h == h0 = img
    | w >= w0 && h >= h0 = scaleUp (w, h) img
    | w <= w0 && h <= h0 = scaleDown (w, h) img
    | otherwise          = error $ unwords ["TODO unsupported resize:", show (w0, h0), "to", show (w, h)]

-- From Codec.Picture.Saving
componentToLDR :: forall a. (Integral a, Bounded a) => Float -> a
componentToLDR = truncate . (fromIntegral (maxBound :: a) *) . min 1.0 . max 0.0

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

makeAvatar :: Int -> DynamicImage -> DynamicImage
makeAvatar d = ImageRGBA16 . clipCircle . resize (d,d) . clipSquare . convertImageToRGBA16

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

-- Derived from Codec.Picture.dynamicPixelMap, converting components with floats to 8bit
-- using the same functions as Codec.Picture.Saving.imageToPng.
convertImageToRGBA16 :: DynamicImage -> Image PixelRGBA16
convertImageToRGBA16 = \case
    ImageY8     i -> promoteImage (promoteImage i :: Image PixelRGBA8)
    ImageY16    i -> promoteImage i
    ImageYF     i -> promoteImage (toStandardDef (promoteImage i))
    ImageYA8    i -> promoteImage (promoteImage i :: Image PixelRGBA8)
    ImageYA16   i -> promoteImage i
    ImageRGB8   i -> promoteImage i
    ImageRGB16  i -> promoteImage i
    ImageRGBF   i -> promoteImage (toStandardDef i)
    ImageRGBA8  i -> promoteImage i
    ImageRGBA16 i -> promoteImage i
    ImageYCbCr8 i -> promoteImage (convertImage i :: Image PixelRGB8)
    ImageCMYK8  i -> promoteImage (convertImage i :: Image PixelRGB8)
    ImageCMYK16 i -> promoteImage (convertImage i :: Image PixelRGB16)
