-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.UIMonad
-- Copyright   :  (c) Daniel Winograd-Cort 2014
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental

{-# LANGUAGE RecursiveDo #-}

module FRP.UISF.UITypes (
  -- * UI Types
  -- $uitypes
  TerminationProc(..), nullTP, mergeTP,
  -- * Rendering Context
  CTX(..), Flow(..),
  -- * UI Layout
  makeLayout, LayoutType(..), nullLayout, Layout(..),
  -- * Context and Layout Functions
  divideCTX, mergeLayout, 
  -- * Graphics
  mergeGraphics,
  -- * System State
  DirtyBit, Focus, WidgetID, FocusInfo(..),
  -- * UIEvent
  UIEvent(..),
  Key(..),
  SpecialKey(..),
  MouseButton(..),
  -- * Key State Checks
  hasShiftModifier, hasCtrlModifier, hasAltModifier,
  isKeyPressed,
  -- * Framework Connections
  -- $frameworkconnections
  updateKeyState
  ) where

import FRP.UISF.Graphics
import Data.IORef
import Data.List (delete)
import System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------
-- * UI Types
------------------------------------------------------------

{- $uitypes
In this module, we will declare the various types to make creating the 
overall UI possible.  We will discuss the ideas for widgets in some 
detail, but for specifics on the type of a widget (the 'UISF' type), 
see the UISF type in "FRP.UISF.UISF", and for information on specific 
widgets, see "FRP.UISF.Widget".

Widgets are arrows that map multiple inputs to multiple outputs.  
Additionally, they have a relatively static layout argument that, 
while it can change over time, is not dependent on any of its 
inputs at any given moment.

On the input end, a widget will accept:

 - a graphical context, 

 - some information about which widget is in focus (for the purposes 
   of routing key presses and mouse clicks and potentially for drawing 
   the widget differently), 

 - and the current time.

 - an event with data relating to UI actions.

On the output end, a widget will produce from these inputs:

 - an indicator of whether the widget needs to be redrawn,

 - any focus information that needs to be conveyed to future widgets, 

 - the graphics to render to display this widget,

 - and a procedure to run upon termination (for proper shutdown when finished).

Additionally, as widgets are generic arrows, there will be a parameterized 
input and output types.

-}

------------------------------------------------------------
-- * Control Data
------------------------------------------------------------

-- | The termination procedure is simply a potential IO action.
type TerminationProc = Maybe (IO ())

-- | The null termination procedure is no action.
nullTP :: TerminationProc
nullTP = Nothing

-- | A method for merging two termination procedures.
mergeTP :: TerminationProc -> TerminationProc -> TerminationProc
mergeTP Nothing     Nothing     = Nothing
mergeTP le@(Just _) Nothing     = le
mergeTP Nothing     re@(Just _) = re
mergeTP (Just l)    (Just r)    = Just (l >> r)


------------------------------------------------------------
-- * Rendering Context
------------------------------------------------------------

-- | A rendering context specifies the following:

data CTX = CTX 
  { flow   :: Flow 
        -- ^ A layout direction to flow widgets. 
  
  , bounds :: Rect 
        -- ^ A rectangle bound of current drawing area to render a UI
        --   component. It specifies the max size of a widget, not the
        --   actual size. It's up to each individual widget to decide
        --   where in this bound to put itself.
  
  , isConjoined :: Bool 
        -- ^ A flag to tell whether we are in a conjoined state or not.  
        -- A conjoined context will duplicate itself for subcomponents 
        -- rather than splitting.  This can be useful for making compound 
        -- widgets when one widget takes up space and the other performs 
        -- some side effect having to do with that space.
  } deriving Show

-- | Flow determines widget ordering.
data Flow = TopDown | BottomUp | LeftRight | RightLeft deriving (Eq, Show)


------------------------------------------------------------
-- * UI Layout
------------------------------------------------------------

-- $ The layout of a widget provides data to calculate its actual size
-- in a given context.  
-- Layout calculation makes use of lazy evaluation to do everything in one pass.  
-- Although the UI function maps from Context to Layout, all of the fields of 
-- Layout must be independent of the Context so that they are avaiable before 
-- the UI function is even evaluated.

-- | Layouts for individual widgets typically come in a few standard flavors, 
--   so we have this convenience function for their creation.
--   This function takes layout information for first the horizontal 
--   dimension and then the vertical.
makeLayout :: LayoutType  -- ^ Horizontal Layout information
           -> LayoutType  -- ^ Vertical Layout information
           -> Layout
makeLayout (Fixed w) (Fixed h) = Layout 0 0 w h 0 0 0
makeLayout (Stretchy wMin) (Fixed h) = Layout 1 0 0 h wMin 0 0
makeLayout (Fixed w) (Stretchy hMin) = Layout 0 1 w 0 0 hMin 0
makeLayout (Stretchy wMin) (Stretchy hMin) = Layout 1 1 0 0 wMin hMin 0

-- | A dimension can either be:
data LayoutType = 
        Stretchy { minSize :: Int }
        -- ^ Stretchy with a minimum size in pixels
      | Fixed { fixedSize :: Int }
        -- ^ Fixed with a size measured in pixels

-- | The null layout is useful for \"widgets\" that do not appear or 
--   take up space on the screen.
nullLayout = NullLayout --Layout 0 0 0 0 0 0 0


-- | More complicated layouts can be manually constructed with direct 
-- access to the Layout data type.
--
-- 1. wStretch and hStretch specify how much stretching space (in comparative 
--    units) in the width and height should be allocated for this widget.
-- 
-- 2. wFixed and hFixed specify how much non-stretching space (in pixels) 
--    of width and height should be allocated for this widget.
-- 
-- 3. wMin and hMin specify minimum values (in pixels) of width and height 
--    for the widget's stretchy dimensions.
--
-- 4. lFill specifies how much expanding space (in comparative units) this 
--    widget should fill out in excess space that would otherwise be unused.

data Layout = NullLayout | Layout
  { wStretch :: Int
  , hStretch :: Int
  , wFixed   :: Int
  , hFixed   :: Int
  , wMin     :: Int
  , hMin     :: Int
  , lFill    :: Int
  } deriving (Eq, Show)



------------------------------------------------------------
-- * Context and Layout Functions
------------------------------------------------------------

---------------
-- divideCTX --
---------------
-- | Divides the CTX among the two given layouts.

divideCTX :: CTX -> Layout -> Layout -> (CTX, CTX)
divideCTX ctx@(CTX a ((x, y), (w, h)) c) l1 l2 = if c then (ctx,ctx) else case (l1,l2) of
  (NullLayout, _) -> (CTX a ((0,0),(0,0)) c, ctx)
  (_, NullLayout) -> (ctx, CTX a ((0,0),(0,0)) c)
  ((Layout wStretch  hStretch  wFixed  hFixed  wMin  hMin  lFill), 
   (Layout wStretch' hStretch' wFixed' hFixed' wMin' hMin' lFill')) ->
      case a of
        TopDown   -> (CTX a ((x, y),                 (w1T, h1T)) c, 
                      CTX a ((x, y + h1T),           (w2T, h2T)) c)
        BottomUp  -> (CTX a ((x, y + h - h1T),       (w1T, h1T)) c, 
                      CTX a ((x, y + h - h1T - h2T), (w2T, h2T)) c)
        LeftRight -> (CTX a ((x, y),                 (w1L, h1L)) c, 
                      CTX a ((x + w1L, y),           (w2L, h2L)) c)
        RightLeft -> (CTX a ((x + w - w1L, y),       (w1L, h1L)) c, 
                      CTX a ((x + w - w1L - w2L, y), (w2L, h2L)) c)
      where
        (w1L,w2L,w1T,w2T) = calc w wStretch wStretch' wFixed wFixed' wMin wMin' lFill lFill'
        (h1T,h2T,h1L,h2L) = calc h hStretch hStretch' hFixed hFixed' hMin hMin' lFill lFill'
        calc len stretch stretch' fixed fixed' lmin lmin' fill fill' = (st1, st2, fi1, fi2) where
          portion s = div' (s * (len - fixed - fixed')) (stretch + stretch')
          (st1,st2) = let u = min len $ fixed  + max lmin  (portion stretch)
                          v =           fixed' + max lmin' (portion stretch')
                          por f = div' (f * (len - u - v)) (fill + fill')
                      in if u+v > len then (u, len-u) else (u + por fill, v + por fill')
          fi1 = if fill  > 0 then len else max lmin  (if stretch  == 0 then fixed  else len)
          fi2 = if fill' > 0 then len else max lmin' (if stretch' == 0 then fixed' else len)
        div' b 0 = 0
        div' b d = div b d


-----------------
-- mergeLayout --
-----------------
-- | Merge two layouts into one.

mergeLayout :: Flow -> Layout -> Layout -> Layout
mergeLayout a NullLayout l = l
mergeLayout a l NullLayout = l
mergeLayout a (Layout n m u v minw minh lFill) (Layout n' m' u' v' minw' minh' lFill') = 
  case a of
    TopDown   -> Layout (max' n n') (m + m') (max u u') (v + v') (max minw minw') (minh + minh') lFill''
    BottomUp  -> Layout (max' n n') (m + m') (max u u') (v + v') (max minw minw') (minh + minh') lFill''
    LeftRight -> Layout (n + n') (max' m m') (u + u') (max v v') (minw + minw') (max minh minh') lFill''
    RightLeft -> Layout (n + n') (max' m m') (u + u') (max v v') (minw + minw') (max minh minh') lFill''
  where
    max' 0 0 = 0
    max' _ _ = 1
    lFill'' = lFill + lFill'


------------------------------------------------------------
-- * Graphics
------------------------------------------------------------

-- | Merging two graphics can be achieved with overGraphic, but 
-- the mergeGraphic function additionally constrains the graphics 
-- based on their layouts and the context.
-- TODO: Make sure this works as well as it should
mergeGraphics :: CTX -> (Graphic, Layout) -> (Graphic, Layout) -> Graphic
mergeGraphics ctx (g1, l1) (g2, l2) = case (l1, l2) of
  (NullLayout,  NullLayout)  -> nullGraphic
  (NullLayout,  _) -> g2
  (_, NullLayout)  -> g1
  (_, _) -> overGraphic g2 g1


------------------------------------------------------------
-- * System State
------------------------------------------------------------
-- $ The DirtyBit and Focus types are for system state.

-- | The dirty bit is a bit to indicate if the widget needs to be redrawn.
type DirtyBit = Bool

-- | The Focus type helps focusable widgets communicate with each 
-- other about which widget is in focus.  It consists of a WidgetID 
-- and a FocusInfo.
type Focus = (WidgetID, FocusInfo)

-- | The WidgetID for any given widget is dynamic based 
-- on how many focusable widgets are active at the moment.  It is designed 
-- basically as a counter that focusable widgets will automatically (via the 
-- focusable function) increment.
type WidgetID = Int

-- | The FocusInfo means one of the following:
data FocusInfo = 
        HasFocus
        -- ^ Indicates that this widget is a subwidget of 
        --   a widget that is in focus.  Thus, this widget too is in focus, and 
        --   this widget should pass HasFocus forward.
      | NoFocus 
        -- ^ Indicates that there is no focus information to 
        --   communicate between widgets.
      | SetFocusTo WidgetID
        -- ^ Indicates that the widget whose id is given 
        --   should take focus.  That widget should then pass NoFocus onward.
      | DenyFocus
        -- ^ Any widget that sees this value should recognize that 
        --   they are no longer in focus.  This is useful for nested focus.
  deriving (Show, Eq)


------------------------------------------------------------
-- * UIEvent
------------------------------------------------------------


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
{- $frameworkconnections
The 'updateKeyState' function is for use by the GUI framework.  It is 
not intended for use unless one wants to build their own framework.

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


