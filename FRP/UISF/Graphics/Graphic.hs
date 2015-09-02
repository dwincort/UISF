-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Graphics.Graphic
-- Copyright   :  (c) Daniel Winograd-Cort 2015
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental

{-# LANGUAGE BangPatterns #-}
module FRP.UISF.Graphics.Graphic (
  -- $graphics
  -- * Graphics
  Graphic(..),
  nullGraphic,
  overGraphic,
  withColor, withColor',
  text, textLines,
  ellipse, shearEllipse, line, polygon, polyline, polybezier, arc,
  circleFilled, circleOutline, rectangleFilled, rectangleOutline,
  translateGraphic, rotateGraphic, scaleGraphic,
  boundGraphic
  ) where


import FRP.UISF.Graphics.Color
import FRP.UISF.Graphics.Text
import FRP.UISF.Graphics.Types
import Control.DeepSeq

{- $graphics
This module provides an abstract representation for graphics in the GUI 
along with a rendering function specific to OpenGL.

The Graphic data type encodes an abstract graphic, which can be created 
and combined with smart constructors.  This means that Graphics are 
inherently restricted to what is possible in this abstract form.  For 
example, OpenGL may support 3D rotations, but because the abstract 
Graphic does not, they cannot be performed in UISF.

For now, we have a lean set of graphics that should satisfy most GUI 
needs.  Additionally, this layer of abstraction should make it easier 
to add more graphical back ends (perhaps a web back end in the future?).  
If UISF grows to include more graphical representations (e.g. more ways 
to render text), we can add them as necessary.

-}

------------------------------------------------------------
-- Graphic
------------------------------------------------------------

-- | The main Graphic data type stores graphic information.  
--  Constructors are not directly exposed to encourage the use of 
--  the smart constructors.
--
--  If you would like to add custom rendering functions for Graphic, 
--  you will clearly need access to the constructors to destruct the 
--  graphics.  Please request this, and I can either export them 
--  or we can discuss adding more rendering functions to this library.

data Graphic = 
    NoGraphic
  | GText Point UIText
  | GPolyLine [Point]
  | GPolygon [Point]
  | GArc Rect Angle Angle
  | GEllipse Rect
  | GBezier [Point]
  | GTranslate Point Graphic
  | GRotate Point Angle Graphic
  | GScale Double Double Graphic
  | GColor RGB Graphic
  | GBounded Rect Graphic
  | OverGraphic Graphic Graphic
  deriving (Eq, Show)

instance NFData Graphic where
  rnf NoGraphic = ()
  rnf (GText (!_,!_) str) = rnf str
  rnf (GPolyLine !pts) = ()
  rnf (GPolygon !pts) = ()
  rnf (GArc ((!_,!_),(!_,!_)) !_ !_) = ()
  rnf (GEllipse ((!_,!_),(!_,!_))) = ()
  rnf (GBezier !pts) = ()
  rnf (GTranslate (!_,!_) g) = rnf g
  rnf (GRotate (!_,!_) !_ g) = rnf g
  rnf (GScale !_ !_ g) = rnf g
  rnf (GColor !_ g) = rnf g
  rnf (GBounded ((!_,!_),(!_,!_)) g) = rnf g
  rnf (OverGraphic g1 g2) = rnf g1 `seq` rnf g2

-- | The absence of a graphic.
nullGraphic :: Graphic
nullGraphic = NoGraphic

-- | The overlay of two graphics, the first over the second.
overGraphic :: Graphic -> Graphic -> Graphic
overGraphic g1 NoGraphic = g1
overGraphic NoGraphic g2 = g2
overGraphic g1 g2 = OverGraphic g1 g2

----------
-- Text --
----------

-- | Paint the given text at the given point.
text :: UITexty s => Point -> s -> Graphic
text p = GText p . toUIText

-- | A convenience function for painting a set of (Point,String) pairs.
textLines :: UITexty s => [(Point, s)] -> Graphic
textLines = foldl (\g (p,s) -> overGraphic (text p s) g) nullGraphic

------------
-- Colors --
------------

-- | Use the given color to paint the given graphic.
withColor :: Color -> Graphic -> Graphic
withColor = withColor' . colorToRGB

-- | Use the given RGB color to paint the given graphic.
withColor' :: RGB -> Graphic -> Graphic
withColor' _ NoGraphic = NoGraphic
withColor' c g = GColor c g


------------
-- Shapes --
------------

-- | Draw an ellipse bounded by the given rectangle.
ellipse :: Rect -> Graphic
ellipse = GEllipse

-- | Draw a shear ellipse bounded by the given rectangle.  This code 
--  was written originally by Paul Liu.
shearEllipse :: Point -> Rect -> Graphic
shearEllipse (x0,y0) r = 
  let ((x1,y1), (w, h)) = normaliseRect r
      (x2,y2) = (x1 + w, y1 + h)
      x =  (x1 + x2) / 2  -- centre of parallelogram
      y =  (y1 + y2) / 2
      dx1 = (x1 - fromIntegral x0) / 2 -- distance to corners from centre
      dy1 = (y1 - fromIntegral y0) / 2
      dx2 = (x2 - fromIntegral x0) / 2
      dy2 = (y2 - fromIntegral y0) / 2
      pts = [ (round $ x + c*dx1 + s*dx2, round $ y + c*dy1 + s*dy2)
            | (c,s) <- cos'n'sins ]
      cos'n'sins = [ (cos a, sin a) | a <- segment 0 (2 * pi) (40 / (w + h))]
  in GPolygon pts

-- | Draw a line segment connecting the given two points.
line :: Point -> Point -> Graphic
line p q = GPolyLine [p,q]

-- | Draw a filled polygon with corners defined by the given points.
polygon :: [Point] -> Graphic
polygon = GPolygon

-- | Draw a sequence of line segments defined by the given points.
polyline :: [Point] -> Graphic
polyline = GPolyLine

-- | Draw a Bezier curve defined by the given points.
polybezier :: [Point] -> Graphic
polybezier = GBezier

-- | Draw an arc of the ellipse bounded by the given rectangle that 
--  starts at the first angle measure and ends at the second.
arc :: Rect -> Angle -> Angle -> Graphic
arc = GArc

-- | Draw a filled circle with given center and radius.
circleFilled :: Point -> Int -> Graphic
circleFilled (x,y) r = GEllipse ((x-r,y-r),(2*r,2*r))

-- | Draw the outline of a circle with given center and radius.
circleOutline :: Point -> Int -> Graphic
circleOutline (x,y) r = GArc ((x-r,y-r),(2*r,2*r)) 0 360

-- | Draw a filled rectangle.
rectangleFilled :: Rect -> Graphic
rectangleFilled ((x,y), (w, h)) = GPolygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- | Draw the outline of a rectangle.
rectangleOutline :: Rect -> Graphic
rectangleOutline ((x,y), (w, h)) = GPolyLine [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]


---------------------
-- Transformations --
---------------------

-- | Translate the given graphic so that its origin is at the given 
--  point.
translateGraphic :: Point -> Graphic -> Graphic
translateGraphic _ NoGraphic = NoGraphic
translateGraphic p g = GTranslate p g

-- | Rotate the given graphic around the given point by the given 
--  number of degrees.
rotateGraphic :: Point -> Angle -> Graphic -> Graphic
rotateGraphic _ _ NoGraphic = NoGraphic
rotateGraphic p a g = GRotate p a g

-- | Scale the given graphic in the X and Y dimension respectively.
scaleGraphic :: Double -> Double -> Graphic -> Graphic
scaleGraphic _ _ NoGraphic = NoGraphic
scaleGraphic x y g = GScale x y g


--------------
-- Bounding --
--------------

-- | Cut the given graphic so that nothing outside of the given 
--  rectangle is visible.
boundGraphic :: Rect -> Graphic -> Graphic
boundGraphic _ NoGraphic = NoGraphic
boundGraphic r g = GBounded r g



------------------------------------------------------------
-- Helper functions
------------------------------------------------------------
normaliseRect :: Rect -> ((Double, Double),(Double, Double))
normaliseRect ((x, y), (w, h)) = ((fromIntegral x', fromIntegral y'), (fromIntegral w', fromIntegral h'))
  where (x',w') = if w < 0 then (x+w, 0-w) else (x, w)
        (y',h') = if h < 0 then (y+h, 0-h) else (y, h)

segment :: (Num t, Ord t) => t -> t -> t -> [t]
segment start stop step = ts start
  where ts i = if i >= stop then [stop] else i : ts (i + step)

