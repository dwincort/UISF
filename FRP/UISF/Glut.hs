-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Glut
-- Copyright   :  (c) Daniel Winograd-Cort 2015
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental

module FRP.UISF.Glut (
  -- $glut
  Window,
  WindowData (..),
  openWindow,
  closeWindow
  ) where


import Graphics.UI.GLUT hiding (Key(..), SpecialKey(..), MouseButton(..))
import qualified Graphics.UI.GLUT as GLUT
import FRP.UISF.Keys

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Control.Monad (when)
import Data.IORef

import FRP.UISF.UITypes (DirtyBit)
import FRP.UISF.Graphics

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
