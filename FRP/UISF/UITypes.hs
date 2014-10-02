-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.UIMonad
-- Copyright   :  (c) Daniel Winograd-Cort 2014
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental

{-# LANGUAGE RecursiveDo #-}

module FRP.UISF.UITypes where

import FRP.UISF.SOE

import Control.Concurrent (ThreadId)


------------------------------------------------------------
-- * UI Types
------------------------------------------------------------

-- $uitypes
-- Widgets are arrows that map multiple inputs to multiple outputs.  
-- Additionally, they have a relatively static layout argument that, 
-- while it can change over time, is not dependent on any of its 
-- inputs at any given moment.
--
-- On the input end, a widget will accept:
--
--  - a graphical context, 
--
--  - some information about which widget is in focus (for the purposes 
--    of routing key presses and mouse clicks and potentially for drawing 
--    the widget differently), 
--
--  - and the current time.
--
-- On the output end, a widget will produce from these inputs:
-- 
--  - an indicator of whether the widget needs to be redrawn,
-- 
--  - any focus information that needs to be conveyed to future widgets, 
-- 
--  - the graphics to render to display this widget,
-- 
--  - and any new ThreadIds to keep track of (for proper shutdown when finished).
--
-- Additionally, as widgets are generic arrows, there will be a parameterized 
-- inputs and output type.
--
-- In this file, we will declare the various types to make creating the overall 
-- UI possible.  For the widget type itself, see UISF in FRP.UISF.UISF.


------------------------------------------------------------
-- * Control Data
------------------------------------------------------------

-- | The control data is simply a list of Thread Ids.
type ControlData = [ThreadId]

-- | No new thread ids.
nullCD :: ControlData
nullCD = []

-- | A method for merging to control data objects.
mergeCD :: ControlData -> ControlData -> ControlData
mergeCD = (++)


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
  }

-- | Flow determines widget ordering.
data Flow = TopDown | BottomUp | LeftRight | RightLeft deriving (Eq, Show)
-- | A dimension specifies size.
type Dimension = (Int, Int)
-- | A rectangle has a corner point and a dimension.
type Rect = (Point, Dimension)


------------------------------------------------------------
-- * UI Layout
------------------------------------------------------------

-- $ctc The layout of a widget provides data to calculate its actual size
-- in a given context.  
-- Layout calculation makes use of lazy evaluation to do everything in one pass.  
-- Although the UI function maps from Context to Layout, all of the fields of 
-- Layout must be independent of the Context so that they are avaiable before 
-- the UI function is even evaluated.

-- | Layouts for individual widgets typically come in a few standard flavors, 
--   so we have this convenience function for their creation.
--   This function takes layout information for first the horizontal 
--   dimension and then the vertical.
makeLayout :: LayoutType ->     -- ^ Horizontal Layout information
              LayoutType ->     -- ^ Vertical Layout information
              Layout
makeLayout (Fixed h) (Fixed v) = Layout 0 0 h v h v
makeLayout (Stretchy minW) (Fixed v) = Layout 1 0 0 v minW v
makeLayout (Fixed h) (Stretchy minH) = Layout 0 1 h 0 h minH
makeLayout (Stretchy minW) (Stretchy minH) = Layout 1 1 0 0 minW minH

-- | A dimension can either be:
data LayoutType = 
        Stretchy { minSize :: Int }
        -- ^ Stretchy with a minimum size in pixels
      | Fixed { fixedSize :: Int }
        -- ^ Fixed with a size measured in pixels

-- | The null layout is useful for \"widgets\" that do not appear or 
--   take up space on the screen.
nullLayout = Layout 0 0 0 0 0 0


-- | More complicated layouts can be manually constructed with direct 
-- access to the Layout data type.
--
-- 1. hFill and vFill specify how much stretching space (in comparative 
--    units) in the horizontal and vertical directions should be 
--    allocated for this widget.
-- 
-- 2. hFixed and vFixed specify how much non-stretching space (in pixels) 
--    of width and height should be allocated for this widget.
-- 
-- 3. minW and minH specify minimum values (in pixels) of width and height 
--    for the widget's dimensions.

data Layout = Layout
  { hFill  :: Int
  , vFill  :: Int
  , hFixed :: Int
  , vFixed :: Int
  , minW   :: Int
  , minH   :: Int
  } deriving (Eq, Show)



------------------------------------------------------------
-- * Context and Layout Functions
------------------------------------------------------------

---------------
-- divideCTX --
---------------
-- | Divides the CTX according to the ratio of a widget's layout and the 
-- overall layout of the widget that receives this CTX.  Therefore, the 
-- first layout argument should basically be a sublayout of the second.

divideCTX :: CTX -> Layout -> Layout -> (CTX, CTX)
divideCTX ctx@(CTX a ((x, y), (w, h)) c) 
          ~(Layout m n u v minw minh) ~(Layout m' n' u' v' minw' minh') =
  if c then (ctx, ctx) else
  case a of
    TopDown   -> (CTX a ((x, y), (w'', h')) c, 
                  CTX a ((x, y + h'), (w, h - h')) c)
    BottomUp  -> (CTX a ((x, y + h - h'), (w'', h')) c, 
                  CTX a ((x, y), (w, h - h')) c)
    LeftRight -> (CTX a ((x, y), (w', h'')) c, 
                  CTX a ((x + w', y), (w - w', h)) c)
    RightLeft -> (CTX a ((x + w - w', y), (w', h'')) c, 
                  CTX a ((x, y), (w - w', h)) c)
  where
    w' = max minw (m * div' (w - u') m' + u)
    h' = max minh (n * div' (h - v') n' + v)
    w'' = max minw (if m == 0 then u else w)
    h'' = max minh (if n == 0 then v else h)
    div' b 0 = 0
    div' b d = div b d


-----------------
-- mergeLayout --
-----------------
-- | Merge two layouts into one.

mergeLayout :: Flow -> Layout -> Layout -> Layout
mergeLayout a (Layout n m u v minw minh) (Layout n' m' u' v' minw' minh') = 
  case a of
    TopDown   -> Layout (max' n n') (m + m') (max u u') (v + v') (max minw minw') (minh + minh')
    BottomUp  -> Layout (max' n n') (m + m') (max u u') (v + v') (max minw minw') (minh + minh')
    LeftRight -> Layout (n + n') (max' m m') (u + u') (max v v') (minw + minw') (max minh minh')
    RightLeft -> Layout (n + n') (max' m m') (u + u') (max v v') (minw + minw') (max minh minh')
  where
    max' 0 0 = 0
    max' _ _ = 1


------------------------------------------------------------
-- * Graphics and System State
------------------------------------------------------------

-- | Merging two graphics can be achieved with overGraphic, but 
-- the mergeGraphic function additionally constrains the graphics 
-- based on their layouts and the context.
-- TODO: Make sure this works as well as it should
mergeGraphics :: CTX -> (Graphic, Layout) -> (Graphic, Layout) -> Graphic
mergeGraphics ctx (g1, l1) (g2, l2) = case (l1 == nullLayout, l2 == nullLayout) of
  (True,  True)  -> nullGraphic
  (True,  False) -> g2
  (False, True)  -> g1
  (False, False) -> overGraphic g2 g1


-- The Focus and DirtyBit types are for system state.

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
  deriving (Show, Eq)

-- | The dirty bit is a bit to indicate if the widget needs to be redrawn.
type DirtyBit = Bool




