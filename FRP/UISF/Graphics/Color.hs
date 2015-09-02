-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Graphics.Color
-- Copyright   :  (c) Daniel Winograd-Cort 2015
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental

{-# LANGUAGE BangPatterns, FlexibleInstances, TypeSynonymInstances #-}
module FRP.UISF.Graphics.Color (
  Color(..), RGB, colorToRGB, rgb, rgbE, extractRGB,
  ) where

import Data.Ix (Ix)
import Control.DeepSeq


-- | We provide a data type for colors to allow users to easily 
--  and clearly specify common colors.  Primary and secondary 
--  RGB colors are represented along with a few beige colors for use 
--  in many GUI elements.
data Color = Black
           | Blue
           | Green
           | Cyan
           | Red
           | Magenta
           | Yellow
           | White
           | Gray
           | VLightBeige
           | LightBeige -- ^ This is the default background color for the UI window.
           | MediumBeige
           | DarkBeige
  deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

instance NFData Color where
  rnf (!_) = ()

-- | RGB can be used to specify colors more precisely.  Create them with 
--  one of the two smart constructors 'rgb' or 'rgbE'.
newtype RGB = RGB (Int, Int, Int)
  deriving (Eq)

instance Show RGB where
  show (RGB (r, g, b)) = "{R="++show r++",G="++show g++",B="++show b++"}"

instance NFData RGB where
  rnf (RGB rgb) = rnf rgb

-- | Generally used as an internal function for converting Color to RGB, 
--  but can be used by anyone.
colorToRGB :: Color -> RGB
colorToRGB Black   = RGB (0, 0, 0)
colorToRGB Blue    = RGB (0, 0, 255)
colorToRGB Green   = RGB (0, 255, 0)
colorToRGB Cyan    = RGB (0, 255, 255)
colorToRGB Red     = RGB (255, 0, 0)
colorToRGB Magenta = RGB (255, 0, 255)
colorToRGB Yellow  = RGB (255, 255, 0)
colorToRGB White   = RGB (255, 255, 255)
colorToRGB Gray    = RGB (128, 128, 128)
colorToRGB VLightBeige = rgbE 0xf1 0xef 0xe2
colorToRGB LightBeige  = rgbE 0xec 0xe9 0xd8
colorToRGB MediumBeige = rgbE 0xac 0xa8 0x99
colorToRGB DarkBeige   = rgbE 0x71 0x6f 0x64
-- In previous versions, there was a color called "blue3".
-- blue3 = rgbE 0x31 0x3c 0x79 --dark slate blue



-- | This function takes three integral values between 0 and 255 
--  inclusive and create an RGB value with them.  If any of the 
--  values fall outside the acceptable range, Nothing is returned.
rgb :: (Integral r, Integral g, Integral b) => r -> g -> b -> Maybe RGB
rgb r g b = do
    r' <- bound r
    g' <- bound g
    b' <- bound b
    return $ RGB (r',g',b')
  where
    bound :: (Integral i, Integral o) => i -> Maybe o
    bound i = if i > 255 || i < 0 then Nothing else Just (fromIntegral i)

-- | This is a version of 'rgb' that throws an error when a given 
--  value falls outside the acceptable 0-255 range.  The error message 
--  shows the bad input, so the extra Show constraint is necessary.
rgbE :: (Integral r, Integral g, Integral b,
         Show r, Show g, Show b) => r -> g -> b -> RGB
rgbE r g b = case rgb r g b of
  Just x  -> x
  Nothing -> error $ "Invalid values given to rgbE: " ++ show (r,g,b)

-- | Use this to extract the values from an RGB color.
extractRGB :: (Integral r, Integral g, Integral b) => RGB -> (r,g,b)
extractRGB (RGB (r, g, b)) = (fromIntegral r, fromIntegral g, fromIntegral b)
