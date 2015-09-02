-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Render.GLUT
-- Copyright   :  (c) Daniel Winograd-Cort 2015
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental

module FRP.UISF.Render.GLUT (
  -- $glut
  Window,
  WindowData (..),
  openWindow,
  closeWindow,
  -- * Rendering Graphics in OpenGL
  renderGraphicInOpenGL,
  glutKeyToKey
  ) where


import Graphics.UI.GLUT hiding (Key(..), SpecialKey(..), MouseButton(..), vertex, Rect)
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Control.Monad (when)
import Data.IORef
import Data.List (unfoldr)

import FRP.UISF.UITypes
import FRP.UISF.Graphics
import FRP.UISF.Graphics.Graphic (Graphic(..))
import FRP.UISF.Graphics.Text (uitextLines)

{- $glut
This module provides the functions for UISF's direct interface with 
GLUT and the GUI window itself.  The main function for this is 
'openWindow', and once a window is open, almost all communication is 
handled through the returned 'WindowData' object.  The one exception 
to this is that one can externally close the window, 
terminating the GUI altogether (although this requires the window 
object, which is found in the WindowData).

Note that the values in WindowData are all IO actions.  Thus, to get 
the "current" value of the window's dimensions, one should run the 
'windowDim' action "now".

Note also that the 'Window' type is being re-exported here as it is 
used in the 'WindowData' type.
-}

-------------------
-- Window Functions
-------------------

-- | The WindowData object is used for communication between the 
--  logic (UISF) and the window (GLUT).
data WindowData = WindowData {
  setGraphics  :: (Graphic, DirtyBit) -> IO (),
  -- ^  This action allows a caller to set the current Graphic to display 
  --    along with a 'DirtyBit' indicating if the Graphic needs to be 
  --    redrawn.
  getWindow    :: IO (Maybe Window),
  -- ^  This action retrieves the active window.  For now, this is used 
  --    both to check if the GUI is still running (a result of Nothing 
  --    indicates that it is not) and to externally close the window.  
  --    Note that if GLUT closes the window (e.g. the user clicks the 
  --    close button), this reference will be updated to Nothing to 
  --    prevent double closure.
  getWindowDim :: IO Dimension,
  -- ^  This action retrieves the window's current dimensions.  There 
  --    is no way to set this value outside of the initial dimension 
  --    provided by openWindow (perhaps a future feature).
  getNextEvent :: IO UIEvent,
  -- ^  This action retrieves the next keyboard/mouse event to be 
  --    processed.  In the case that there is no new event, NoUIEvent 
  --    is provided.
  peekNextEvent :: IO UIEvent,
  -- ^  This action peeks at the next keyboard/mouse event to be 
  --    processed.  In the case that there is no new event, NoUIEvent 
  --    is provided.  This was added for a potential performance boost.
  getElapsedGUITime :: IO Double
  -- ^  This action retrieves the number of real time seconds that have 
  --    elapsed since the GUI began.
}

-- | This function creates the GUI window.  It takes as arguments 
--  a default background color, a title for the window, and the initial 
--  dimensions for the window; it produces a WindowData object to use 
--  as communication to the window.
--
--  Note that the main GLUT loop is run in a separate OS thread produced 
--  by forkOS.
openWindow :: RGB -> String -> Dimension -> IO WindowData
openWindow rgb title (x,y) = do
  gRef  <- newIORef (nullGraphic, False)
  wRef  <- newIORef Nothing
  wdRef <- newIORef (x,y)
  eChan <- atomically newTChan
  continue <- newEmptyMVar
  let w = WindowData (writeIORef gRef) (readIORef wRef)
                     (readIORef wdRef) (nextEvent tryReadTChan eChan) 
                     (nextEvent tryPeekTChan eChan) guiTime
  -- REMARK: forkIO seems to work fine, but if GLUT starts misbehaving, 
  --    this may need to change to forkOS.
  forkIO (f gRef wRef wdRef eChan continue)
  takeMVar continue
  return w
 where 
  nextEvent r c = do
    me <- atomically $ r c
    case me of
      Nothing -> return NoUIEvent
      Just e  -> return e
  f gRef wRef wdRef eChan continue = do
    -- Initialize and create the window.
    (_progName, otherArgs) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    w <- createWindow title
    windowSize $= Size (fromIntegral x) (fromIntegral y)
    -- Update the WindowData Window reference to point to this new window.
    writeIORef wRef (Just w)
    -- We want the program to be able to continue when the window closes.
    actionOnWindowClose $= ContinueExecution
    -- Set the default background color.
    setBackgroundColor rgb
    
    -- Set up the various call back functions.
    displayCallback $= displayCB gRef
    idleCallback $= Just (idleCB gRef)
    reshapeCallback $= Just (reshapeCB wdRef)
    keyboardMouseCallback $= Just (keyboardMouseCB eChan)
    motionCallback $= Just (motionCB eChan)
    passiveMotionCallback $= Just (motionCB eChan)
    closeCallback $= Just (closeCB wRef)
    
    -- These 4 settings are pulled from elsewhere.
    -- They're probably good?
    lineSmooth $= Enabled
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lineWidth $= 1.5
    
    -- Indicate to the main thread that the window is good to go.
    putMVar continue ()
    -- Begin the main loop.
    mainLoop

-- | When provided with an active window, this function will close 
--  the window.
closeWindow :: Window -> IO ()
closeWindow = destroyWindow

-- | Set the default background color for the GUI window.
setBackgroundColor :: RGB -> IO ()
setBackgroundColor rgb = clearColor $= Color4 r g b 0
  where (r',g',b') = extractRGB rgb
        r = fromIntegral r' / 255
        g = fromIntegral g' / 255
        b = fromIntegral b' / 255

-- | The callback to update the display.
displayCB :: IORef (Graphic, DirtyBit) -> DisplayCallback
displayCB ref = do 
  (g, _) <- readIORef ref
  loadIdentity
  clear [ColorBuffer, StencilBuffer]
  (Size x y) <- get windowSize
  renderGraphicInOpenGL (fromIntegral x, fromIntegral y) g
  swapBuffers

-- | When the GUI is idle, we should check if the dirty bit is set.  
--  If so, we signal a redraw of the display.
idleCB :: IORef (Graphic, DirtyBit) -> IdleCallback
idleCB ref = do
  db <- atomicModifyIORef ref (\(g,db) -> ((g,False),db))
  when db $ postRedisplay Nothing

-- | When the window is resized, we perform this mess to make sure 
--  everything is drawn properly.  This model assumes no stretching 
--  and instead forces the user to deal with exact pixel sizes.
reshapeCB :: IORef Dimension -> ReshapeCallback
reshapeCB wdref size@(Size w h) = do
  writeIORef wdref (fromIntegral w, fromIntegral h)
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (realToFrac w) (realToFrac h) 0
  matrixMode $= Modelview 0
  loadIdentity
  postRedisplay Nothing

-- | When a keyboard or mouse event comes in, send it to the 'WindowData' 
--  object for external processing.  Also, update the global keyState so 
--  that 'isKeyPressed' and the has***Modifier functions work as expected.
keyboardMouseCB :: TChan UIEvent -> KeyboardMouseCallback
keyboardMouseCB chan key d modifiers (Position x y) = do
  let k = glutKeyToKey key
      down = (d == Down)
      p = (fromIntegral x, fromIntegral y)
  mods <- updateKeyState k down
  case k of
    (Char c) -> 
      atomically $ writeTChan chan Key{ char = c, modifiers = mods, isDown = down}
    (SpecialKey sk) -> 
      atomically $ writeTChan chan SKey{ skey = sk, modifiers = mods, isDown = down}
    (MouseButton mb) -> 
      atomically $ writeTChan chan Button{ pt = p, mbutton = mb, isDown = down}

-- | When the mouse moves at all, add an event to the 'WindowData' for 
--  external processing.
motionCB :: TChan UIEvent -> MotionCallback
motionCB chan (Position x y) = 
  atomically $ writeTChan chan MouseMove{ pt = (fromIntegral x, fromIntegral y)}

-- | When the window closes, update the window stored in the 'WindowData'.
closeCB :: IORef (Maybe Window) -> CloseCallback
closeCB ref = writeIORef ref Nothing

-- | Converts the GUI's elapsed time from GLUT's integral millisecond 
--  standard into floating point seconds.
guiTime :: IO Double
guiTime = do
  mills <- get elapsedTime
  return $ fromIntegral mills / 1000


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

renderGraphicInOpenGL s (GColor rgb graphic) = (GL.color color >> renderGraphicInOpenGL s graphic) where
  (r,g,b) = extractRGB rgb
  color = GL.Color3 (c2f r) (c2f g) (c2f b) :: GL.Color3 GLfloat
  c2f i = fromIntegral i / 255

renderGraphicInOpenGL _ (GText (x,y) uistr) = 
  let tlines = zip (uitextLines uistr) [0..]
      drawLine (s,i) = do 
        -- We need to zipWith like this to get the String x-offsets.
        let ss = unfoldr buildList (0,unwrapUIT s)
            buildList (_,[]) = Nothing
            buildList (x,(c,f,str):rest) = Just ((x,c,f,str), (x+textWidth' f str, rest))
            th = textHeight s
            yoff = (i * th) + (th `div` 2) + 3
        mapM_ (drawStr yoff) ss
      drawStr yoff (xoff, c, f, str) = GL.preservingMatrix $ do
        case c of
          Nothing -> return ()
          Just rgb -> GL.color color where
            (r,g,b) = extractRGB rgb
            color = GL.Color3 (c2f r) (c2f g) (c2f b) :: GL.Color3 GLfloat
            c2f i = fromIntegral i / 255
--      This code is used for Bitmap fonts (raster offset values may need to be adjusted)
        GL.currentRasterPosition $= GLUT.Vertex4 
            (fromIntegral $ x + xoff) 
            (fromIntegral $ y + yoff) 0 1
        GLUT.renderString f str
--      This code is used for Stroke fonts (scale and translate values may need to be adjusted)
--        GL.translate (vector (x, y+16*(i+1)))
--        GL.scale 0.12 (-0.12) (1::GLfloat)
--        GLUT.renderString GLUT.MonoRoman s
  in mapM_ drawLine tlines

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


------------------------------------------------------------
-- Key support
------------------------------------------------------------

-- | Convert GLUT's key codes to UISF's internal ones.
glutKeyToKey :: GLUT.Key -> Key
glutKeyToKey key 
 = case key of
        GLUT.Char '\13'                            -> SpecialKey KeyEnter
        GLUT.Char '\9'                             -> SpecialKey KeyTab
        GLUT.Char '\ESC'                           -> SpecialKey KeyEsc
        GLUT.Char '\DEL'                           -> SpecialKey KeyDelete
        GLUT.Char '\BS'                            -> SpecialKey KeyBackspace
        GLUT.Char c                                -> Char c
        GLUT.SpecialKey GLUT.KeyF1                 -> SpecialKey KeyF1
        GLUT.SpecialKey GLUT.KeyF2                 -> SpecialKey KeyF2
        GLUT.SpecialKey GLUT.KeyF3                 -> SpecialKey KeyF3
        GLUT.SpecialKey GLUT.KeyF4                 -> SpecialKey KeyF4
        GLUT.SpecialKey GLUT.KeyF5                 -> SpecialKey KeyF5
        GLUT.SpecialKey GLUT.KeyF6                 -> SpecialKey KeyF6
        GLUT.SpecialKey GLUT.KeyF7                 -> SpecialKey KeyF7
        GLUT.SpecialKey GLUT.KeyF8                 -> SpecialKey KeyF8
        GLUT.SpecialKey GLUT.KeyF9                 -> SpecialKey KeyF9
        GLUT.SpecialKey GLUT.KeyF10                -> SpecialKey KeyF10
        GLUT.SpecialKey GLUT.KeyF11                -> SpecialKey KeyF11
        GLUT.SpecialKey GLUT.KeyF12                -> SpecialKey KeyF12
        GLUT.SpecialKey GLUT.KeyLeft               -> SpecialKey KeyLeft
        GLUT.SpecialKey GLUT.KeyUp                 -> SpecialKey KeyUp
        GLUT.SpecialKey GLUT.KeyRight              -> SpecialKey KeyRight
        GLUT.SpecialKey GLUT.KeyDown               -> SpecialKey KeyDown
        GLUT.SpecialKey GLUT.KeyPageUp             -> SpecialKey KeyPageUp
        GLUT.SpecialKey GLUT.KeyPageDown           -> SpecialKey KeyPageDown
        GLUT.SpecialKey GLUT.KeyHome               -> SpecialKey KeyHome
        GLUT.SpecialKey GLUT.KeyEnd                -> SpecialKey KeyEnd
        GLUT.SpecialKey GLUT.KeyInsert             -> SpecialKey KeyInsert
        GLUT.SpecialKey GLUT.KeyNumLock            -> SpecialKey KeyNumLock
        GLUT.SpecialKey GLUT.KeyBegin              -> SpecialKey KeyBegin
        GLUT.SpecialKey GLUT.KeyDelete             -> SpecialKey KeyDelete
        GLUT.SpecialKey (GLUT.KeyUnknown i)        -> SpecialKey (KeyUnknown i)
        GLUT.SpecialKey GLUT.KeyShiftL             -> SpecialKey KeyShiftL
        GLUT.SpecialKey GLUT.KeyShiftR             -> SpecialKey KeyShiftR
        GLUT.SpecialKey GLUT.KeyCtrlL              -> SpecialKey KeyCtrlL
        GLUT.SpecialKey GLUT.KeyCtrlR              -> SpecialKey KeyCtrlR
        GLUT.SpecialKey GLUT.KeyAltL               -> SpecialKey KeyAltL
        GLUT.SpecialKey GLUT.KeyAltR               -> SpecialKey KeyAltR
        GLUT.MouseButton GLUT.LeftButton           -> MouseButton LeftButton
        GLUT.MouseButton GLUT.MiddleButton         -> MouseButton MiddleButton
        GLUT.MouseButton GLUT.RightButton          -> MouseButton RightButton
        GLUT.MouseButton GLUT.WheelUp              -> MouseButton WheelUp
        GLUT.MouseButton GLUT.WheelDown            -> MouseButton WheelDown
        GLUT.MouseButton (GLUT.AdditionalButton i) -> MouseButton (AdditionalButton i)