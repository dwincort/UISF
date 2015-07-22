-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Keys
-- Copyright   :  (c) Daniel Winograd-Cort 2015
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental

module FRP.UISF.Keys (
  UIEvent(..),
  Key(..),
  SpecialKey(..),
  MouseButton(..),
  -- * Key State Checks
  hasShiftModifier, hasCtrlModifier, hasAltModifier,
  isKeyPressed,
  -- * Framework Connections
  -- $frameworkconnections
  updateKeyState,
  glutKeyToKey
  ) where


import qualified Graphics.UI.GLUT as GLUT
import FRP.UISF.Graphics (Point)
import Data.IORef
import Data.List (delete)
import System.IO.Unsafe (unsafePerformIO)

{- $frameworkconnections
The 'updateKeyState' and 'glutKeyToKey' functions are for use by the 
GUI framework to hook properly into this module.  They are not intended 
for use unless one wants to build an alternative to GLUT.

In fact, it may be the case that they actually belong in the 
'FRP.UISF.Glut' module itself and should just never be exposed at all.  
But, that would mean moving the KeyState stuff to that module as well, 
and I'm not quite ready to do that.
-}


-- | The UIEvent data type captures the various types of events that 
--  the UI can produce.  These are covered by regular keys, special 
--  keys, mouse button presses, and mouse movement.  Any key event 
--  is accompanied by a list of 'Key's that were down when the given 
--  event took place.
data UIEvent = 
  -- | A Key UIEvent indicates that the user has typed a regular key 
  --  on his/her keyboard.  These will either be upper or lowercase 
  --  characters.
    Key {
      char :: Char,
      modifiers :: [Key],
      isDown :: Bool
    }
  -- | A SKey UIEvent indicates that the user has typed a special 
  --  key.  These are Enter, Backspace, Tab, Delete, etc.  See 
  --  'SpecialKey' for more.
  | SKey {
      skey :: SpecialKey,
      modifiers :: [Key],
      isDown :: Bool
    }
  -- | A Button UIEvent indicates that the user has pressed a mouse 
  --  button.
  | Button {
     pt :: Point,
     mbutton :: MouseButton,
     isDown :: Bool
    }
  -- | Every time the mouse moves, a MouseMove UIEvent will fire.
  | MouseMove {
      pt :: Point
    }
  -- | The NoUIEvent fires when nothing else is going on.  It is 
  --  important that this happens to allow interaction-independent 
  --  processing to continue (e.g. timers, animations, etc.).
  | NoUIEvent
 deriving (Eq,Show)


-------------------
-- Key state
-------------------
{-
The key state is kept around so that it is easy to check if a given 
key or button is currently pressed down.  Unfortunately, I've coded it 
as a global IORef, which means I'm using unsafePerformIO.
-}

-- | The global IORef storing the state of all current key presses.
keyState :: IORef [Key]
keyState = unsafePerformIO $ newIORef []

-- | This should be called by the GUI engine (GLUT) whenever the user 
--  presses or releases a key/button.  As long as it is called every 
--  time, it will keep an accurate key state.
updateKeyState :: Key   -- ^ The Key pressed/released.
               -> Bool  -- ^ True if pressed, False if released.
               -> IO [Key]  -- ^ The updated key state.
updateKeyState k s = case s of
    True  -> atomicModifyIORef keyState (dup . add)
    False -> atomicModifyIORef keyState (dup . remove)
  where
    add ks = if k `elem` ks then ks else k:ks
    remove ks = delete k ks
    dup x = (x,x)

-- | This is a convenience function that tests whether either of the 
--  right or left shift keys is in the given list.
hasShiftModifier :: [Key] -> Bool
hasShiftModifier ks = elem (SpecialKey KeyShiftL) ks || elem (SpecialKey KeyShiftR) ks

-- | This is a convenience function that tests whether either of the 
--  right or left control keys is in the given list.
hasCtrlModifier :: [Key] -> Bool
hasCtrlModifier ks = elem (SpecialKey KeyCtrlL) ks || elem (SpecialKey KeyCtrlR) ks

-- | This is a convenience function that tests whether either of the 
--  right or left alt keys is in the given list.
hasAltModifier :: [Key] -> Bool
hasAltModifier ks = elem (SpecialKey KeyAltL) ks || elem (SpecialKey KeyAltR) ks

-- | Checks the global key state to determine whether the given key is 
--  currently pressed down.
isKeyPressed :: Key -> IO Bool
isKeyPressed k = do
    ks <- readIORef keyState
    return $ elem k ks





-- | A Key can either be a character, a special key, or a mouse button.
data Key
   = Char Char
   | SpecialKey SpecialKey
   | MouseButton MouseButton
   deriving ( Eq, Ord, Show )

-- | A special key is any non-standard character key.  According to 
--  GLUT, 'KeyUnknown' should never be used, probably because it will 
--  be treated as a weird Char instead of a SpecialKey.
data SpecialKey
   = KeyF1
   | KeyF2
   | KeyF3
   | KeyF4
   | KeyF5
   | KeyF6
   | KeyF7
   | KeyF8
   | KeyF9
   | KeyF10
   | KeyF11
   | KeyF12
   | KeyLeft
   | KeyUp
   | KeyRight
   | KeyDown
   | KeyPageUp
   | KeyPageDown
   | KeyHome
   | KeyEnd
   | KeyInsert
   | KeyNumLock
   | KeyBegin
   | KeyDelete
   | KeyShiftL
   | KeyShiftR
   | KeyCtrlL
   | KeyCtrlR
   | KeyAltL
   | KeyAltR
   | KeyEnter
   | KeyTab
   | KeyEsc
   | KeyBackspace
   | KeyUnknown Int
   deriving ( Eq, Ord, Show )

-- | The standard mouse buttons are represented, but for specialty mice, 
--  one can also use the 'AdditionalButton' value.
data MouseButton
   = LeftButton
   | MiddleButton
   | RightButton
   | WheelUp
   | WheelDown
   | AdditionalButton Int
   deriving ( Eq, Ord, Show )

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