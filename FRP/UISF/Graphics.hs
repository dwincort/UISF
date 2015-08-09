-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Graphics
-- Copyright   :  (c) Daniel Winograd-Cort 2015
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental

{-# LANGUAGE BangPatterns #-}
module FRP.UISF.Graphics (
  -- $graphics
  -- * Useful Types
  Point, Angle, Dimension, Rect,
  Color(..), RGB, colorToRGB, rgb, rgbE, extractRGB,
  -- * Graphics
  Graphic,
  nullGraphic,
  overGraphic,
  withColor, withColor',
  text, textLines,
  textWidth, textWithinPixels, textHeight,
  WrapSetting(..), prepText, 
  ellipse, shearEllipse, line, polygon, polyline, polybezier, arc,
  circleFilled, circleOutline, rectangleFilled, rectangleOutline,
  translateGraphic, rotateGraphic, scaleGraphic,
  boundGraphic,
  -- * Rendering Graphics in OpenGL
  renderGraphicInOpenGL
  ) where


import qualified Graphics.Rendering.OpenGL as GL
import qualified FRP.UISF.FontSupport as FS
import Graphics.Rendering.OpenGL (($=), GLfloat)
import qualified Graphics.UI.GLUT as GLUT
import Data.Ix (Ix)
import Control.DeepSeq
import Data.Char (isSpace)

defaultFont = GLUT.Fixed9By15

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
-- Useful Types (Including Colors)
------------------------------------------------------------

-- | Point describes a point on the GUI.
type Point = (Int, Int)

-- | A dimension specifies size.
type Dimension = (Int, Int)

-- | A rectangle has a (bottom left) corner point and a dimension.
type Rect = (Point, Dimension)

-- | Angles are used when making arcs, circles, etc. or when 
--  performing rotations.  Angles are measured in Degrees
type Angle = Double

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
  | GText Point String
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


----------
-- Text --
----------

-- In the future, these text functions should be parametrized by font.

-- | Paint the given text at the given point.
text :: Point -> String -> Graphic
text = GText

-- | A convenience function for painting a set of (Point,String) pairs.
textLines :: [(Point, String)] -> Graphic
textLines = foldl (\g (p,s) -> overGraphic (text p s) g) nullGraphic

-- | Returns the width of the String in pixels as it will be rendered
textWidth :: String -> Int
textWidth = FS.textWidth defaultFont

-- | Given a String and a number of pixels, returns the leading 
--  substring that fits within the horizontal number of pixels along 
--  with the remaining text of the String.
textWithinPixels :: Int -> String -> (String, String)
textWithinPixels = FS.textWithinPixels defaultFont

-- | Returns the height of the String in pixels as it will be rendered
textHeight :: String -> Int
textHeight = FS.textHeight defaultFont

-- | The Wrap Setting is used to determine how to split up a long piece 
--  of text.
data WrapSetting = NoWrap | CharWrap | WordWrap
  deriving (Eq, Show)

-- | Turn the given String into a list of Strings.  If the wrap setting 
--  is NoWrap, then this is basically just the lines function.  If it 
--  is CharWrap or WordWrap, then no string in the list will be wider 
--  than the width of the bounding box.  The returned list of points 
--  indicate each Point where a line should be drawn.  Note that this 
--  list may not be the same length as the list of strings.
--
--  Typically, this will be used in conjunction with zip and textLines 
--  to produce text graphics.
prepText :: WrapSetting    -- ^ Whether we prefer newer or older text
         -> Double             -- ^ Line spacing
         -> Rect               -- ^ Bounding Box
         -> String             -- ^ The text to print (which is allowed to have new lines)
         -> ([Point], [String])
prepText wrap spacing ((x,y),(w,h)) s = (pts, outStrs) where
  lineHeight = round (fromIntegral (textHeight s) * spacing)
  numLines = h `div` lineHeight
  pts = zip (replicate numLines x) [y, y+lineHeight..]
  outStrs = concatMap (wrapText wrap w) (linesWith s)

-- | wrapText takes a wrap setting, a width, and a string, and turns 
--  it into a list of strings representing each wrapped line.  Strings 
--  are assumed to have no line breaks in them.  Calling unlines on the 
--  output will create a String that is wrapped.
wrapText :: WrapSetting -> Int -> String -> [String]
wrapText NoWrap _ s = [s]
wrapText CharWrap _ [] = []
wrapText CharWrap i s = let (t,d) = textWithinPixels i s in t:wrapText CharWrap i d
wrapText WordWrap i s = f i "" (wordsWith s) where
  f _ [] [] = []
  f _ sofar [] = [sofar]
  f _ [] (w:ws) = if textWidth w > i 
                  then let (t,d) = textWithinPixels i w in t:f i "" (d:ws)
                  else f (i-textWidth w) w ws
  f j sofar (w:ws) = if textWidth w > j 
                     then sofar:f i "" (w:ws)
                     else f (j-textWidth w) (sofar++w) ws

wordsWith :: String -> [String]
wordsWith s = case break isSpace s of
                 (word,"") -> [word]
                 (word,s') -> (word++wh) : wordsWith s''
                                where (wh, s'') = span isSpace s'

linesWith s = cons (case break (== '\n') s of
                      (l, "") -> (l,[])
                      (l, s') -> (l++"\n", case s' of
                                             []    -> []
                                             _:s'' -> linesWith s''))
  where
    cons ~(h, t) =  h : t

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
-- Rendering Graphics in OpenGL
------------------------------------------------------------

-- | This function takes the current dimensions of the window 
--  (necessary for the bounding operation 'boundGraphic') and a Graphic 
--  and produces the OpenGL IO action that actually performs the 
--  rendering.  Two notes about it:
--
--  - Currently, it is using 'Graphics.UI.GLUT.Fixed8By13' for 
--    rendering text.
--
--  - I have had some trouble with nesting uses of PreservingMatrix 
--    and scissoring, so bounded graphics (and perhaps other graphic 
--    transformations in general) may be a little buggy.
renderGraphicInOpenGL :: Dimension -> Graphic -> IO ()

renderGraphicInOpenGL _ NoGraphic = return ()

renderGraphicInOpenGL s (GColor (RGB (r,g,b)) graphic) = (GL.color color >> renderGraphicInOpenGL s graphic) where
  color = GL.Color3 (c2f r) (c2f g) (c2f b :: GLfloat)
  c2f i = fromIntegral i / 255

renderGraphicInOpenGL _ (GText (x,y) str) = 
  let tlines = zip (lines str) [0..]
      drawLine (s,i) = do 
--      This code is used for Bitmap fonts (raster offset values may need to be adjusted)
        let mod = (i * textHeight s) + (textHeight s `div` 2) + 3
        GL.currentRasterPosition $= GLUT.Vertex4 (fromIntegral x) 
            (fromIntegral $ y + mod) 0 1
        GLUT.renderString defaultFont s
--      This code is used for Stroke fonts (scale and translate values may need to be adjusted)
--        GL.translate (vector (x, y+16*(i+1)))
--        GL.scale 0.12 (-0.12) (1::GLfloat)
--        GLUT.renderString GLUT.MonoRoman s
  in GL.preservingMatrix $ mapM_ drawLine tlines

renderGraphicInOpenGL _ (GPolyLine ps) = 
  GL.renderPrimitive GL.LineStrip (mapM_ vertex ps)

renderGraphicInOpenGL _ (GPolygon ps) = 
  GL.renderPrimitive GL.Polygon (mapM_ vertex ps)

renderGraphicInOpenGL _ (GEllipse rect) = GL.preservingMatrix $ do
  let ((x, y), (width, height)) = normaliseRect rect
      r@(r1,r2) = (width / 2, height / 2)
  GL.translate $ vectorR (x + r1, y + r2) --r
  GL.renderPrimitive GL.Polygon $ mapM_ vertexR 
    [ (r1 * cos i, r2 * sin i) | i <- segment 0 (2 * pi) (6 / (r1 + r2)) ]

renderGraphicInOpenGL _ (GArc rect start extent) = GL.preservingMatrix $ do
  let ((x, y), (width, height)) = normaliseRect rect
      r@(r1, r2) = (width / 2, height / 2)
  GL.translate $ vectorR (x + r1, y + r2)
  GL.renderPrimitive GL.LineStrip $ mapM_ vertexR 
    [ (r1 * cos i, r2 * sin i) | i <- segment (-(start + extent) * pi / 180) 
        (-start * pi / 180) (6 / (r1 + r2)) ]

renderGraphicInOpenGL _ (GBezier []) = return ()
renderGraphicInOpenGL s (GBezier ps) = renderGraphicInOpenGL s (GPolyLine ps') where
  ps' = map (bezier ps) (segment 0 1 dt)
  dt = 1 / (lineLength ps / 8)
  lineLength :: [Point] -> Double
  lineLength ((x1,y1):(x2,y2):ps') = 
    let dx = fromIntegral $ x2 - x1
        dy = fromIntegral $ y2 - y1
    in sqrt (dx * dx + dy * dy) + lineLength ((x2,y2):ps')
  lineLength _ = 0
  bezier :: [Point] -> Double -> Point
  bezier [(x1,y1)] _t = (x1, y1)
  bezier [(x1,y1),(x2,y2)] t = (x1 + round (fromIntegral (x2 - x1) * t), 
                                y1 + round (fromIntegral (y2 - y1) * t))
  bezier ps t = bezier (map (\ (p, q) -> bezier [p,q] t) (zip ps (tail ps))) t

renderGraphicInOpenGL s (GTranslate (x,y) g) = 
  GL.translate (vector (x,y)) >> renderGraphicInOpenGL s g >> GL.translate (vector (0-x,0-y))
--renderGraphicInOpenGL (GTranslate p g) = 
--  GL.preservingMatrix $ GL.translate (vector p) >> renderGraphicInOpenGL g

renderGraphicInOpenGL s@(_,windowY) (GBounded ((x,y), (w,h)) g) = do
    let [x', y', w', h'] = map fromIntegral [x, windowY-y-h, w, h]
    oldScissor <- GL.get GL.scissor
    let ((x'',y''),(w'',h'')) = maybe ((x',y'),(w',h'))
            (\(GL.Position a b, GL.Size c d) -> intersect ((x',y'),(w',h')) ((a,b),(c,d))) oldScissor
    -- FIXME: This intersection of scissors may not be right, but I'm not sure what's better
    GL.scissor $= Just (GL.Position x'' y'', GL.Size w'' h'')
    renderGraphicInOpenGL s g
    GL.scissor $= oldScissor
  where
    intersect ((x,y),(w,h)) ((x',y'),(w',h')) = ((x'',y''),(w'',h'')) where
      x'' = min x x'
      y'' = min y y'
      w'' = max 0 $ (min (x+w) (x'+w')) - x''
      h'' = max 0 $ (min (y+h) (y'+h')) - y''


renderGraphicInOpenGL s (GRotate p a' g) = 
  GL.preservingMatrix $ GL.rotate a (vector p) >> renderGraphicInOpenGL s g
--  GL.rotate a (vector p) >> renderGraphicInOpenGL g >> GL.rotate (0-a) (vector p)
  where a = realToFrac a'

renderGraphicInOpenGL s (GScale x' y' g) = 
  GL.preservingMatrix $ GL.scale x y (1::GLfloat) >> renderGraphicInOpenGL s g
--  GL.scale x y (1::GLfloat) >> renderGraphicInOpenGL g >> GL.scale (1/x) (1/y) (1::GLfloat)
  where x = realToFrac x'
        y = realToFrac y'

renderGraphicInOpenGL s (OverGraphic over base) = 
  renderGraphicInOpenGL s base >> renderGraphicInOpenGL s over



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

vertex :: Point -> IO ()
vertex (x,y) = GL.vertex $ GL.Vertex3 (fromIntegral x) (fromIntegral y) (0::GLfloat)

vertexR :: (Double,Double) -> IO ()
vertexR (x,y) = GL.vertex $ GL.Vertex3 (realToFrac x) (realToFrac y) (0::GLfloat)

vector :: (Int, Int) -> GL.Vector3 GLfloat
vector (x,y) = GL.Vector3 (fromIntegral x) (fromIntegral y) 0

vectorR :: (Double,Double) -> GL.Vector3 GLfloat
vectorR (x,y) = GL.Vector3 (realToFrac x) (realToFrac y) 0

