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
import FRP.UISF.AuxFunctions (mergeE)

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
mergeTP = mergeE (>>)


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
-- | A dimension specifies size.
type Dimension = (Int, Int)
-- | A rectangle has a corner point and a dimension.
type Rect = (Point, Dimension)


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
makeLayout (Fixed h) (Fixed v) = Layout 0 0 h v 0 0
makeLayout (Stretchy minW) (Fixed v) = Layout 1 0 0 v minW 0
makeLayout (Fixed h) (Stretchy minH) = Layout 0 1 h 0 0 minH
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
--    for the widget's stretchy dimensions.

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
-- | Divides the CTX among the two given layouts.

divideCTX :: CTX -> Layout -> Layout -> (CTX, CTX)
divideCTX ctx@(CTX a ((x, y), (w, h)) c) 
          ~(Layout wFill  hFill  wFixed  hFixed  wMin  hMin) 
          ~(Layout wFill' hFill' wFixed' hFixed' wMin' hMin') =
  if c then (ctx, ctx) else
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
    -- The commented out code here forces the contexts to match exactly 
    -- what the layout requests.  The code in place matches to the first 
    -- layout and then gives the rest of the context to the second.
    -- A more robust design may require a special "filler" layout that 
    -- is not stretchy but will accept any leftover pixels.  We could 
    -- then have a filler widget that is essentially (arr id) with this 
    -- special layout.
    wportion fill = div' (fill * (w - wFixed - wFixed')) (wFill + wFill')
    (w1L,w2L) = let w1 = wFixed  + max wMin  (wportion wFill)
                    w2 = wFixed' + max wMin' (wportion wFill')
                in (w1, w-w1) --if w1+w2 > w then (w1, w-w1) else (w1, w2)
    h1L = h --max hMin  (if hFill  == 0 then hFixed  else h)
    h2L = h --max hMin' (if hFill' == 0 then hFixed' else h)
    hportion fill = div' (fill * (h - hFixed - hFixed')) (hFill + hFill')
    (h1T,h2T) = let h1 = hFixed  + max hMin  (hportion hFill)
                    h2 = hFixed' + max hMin' (hportion hFill')
                in (h1, h-h1) --if h1+h2 > h then (h1, h-h1) else (h1, h2)
    w1T = w --max wMin  (if wFill  == 0 then wFixed  else w)
    w2T = w --max wMin' (if wFill' == 0 then wFixed' else w)
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
-- * Graphics
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




