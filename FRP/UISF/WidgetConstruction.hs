-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.WidgetConstruction
-- Copyright   :  (c) Daniel Winograd-Cort 2015
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental
--
-- This module provides functions and utilities that help in the 
-- construction of new widgets.  They are used by FRP.UISF.Widget, 
-- and can be used for any custom widgets as well.

{-# LANGUAGE RecursiveDo, Arrows #-}

module FRP.UISF.WidgetConstruction where

import FRP.UISF.Graphics
import FRP.UISF.Keys
import FRP.UISF.UITypes
import FRP.UISF.UISF
import FRP.UISF.AuxFunctions (SEvent, delay, constA)

import Control.Arrow
import Data.Maybe (fromMaybe)


------------------------------------------------------------
-- Shorthand and Helper Functions
------------------------------------------------------------

-- | Default padding between border and content.
padding :: Int
padding = 3 

-- | The default assumed background color of the GUI window.
bg :: Color
bg = LightBeige

-- | An infix shorthand for overGraphic.
(//) :: Graphic -> Graphic -> Graphic
(//) = overGraphic

-- | A nice way to make a graphic under only certain conditions.
whenG :: Bool -> Graphic -> Graphic
whenG True  g = g
whenG False _ = nullGraphic

-- | Tests whether a Point is within the bounds of a rectangle.
inside :: Point -> Rect -> Bool
inside (u, v) ((x, y), (w, h)) = u >= x && v >= y && u < x + w && v < y + h



------------------------------------------------------------
-- * Widget Builders
------------------------------------------------------------

-- | mkWidget is a helper function to make stateful widgets easier to write.  
-- In essence, it breaks down the idea of a widget into 4 constituent 
-- components: state, layout, computation, and drawing.
-- 
-- As 'mkWidget' allows for making stateful widgets, the first parameter is 
-- simply the initial state.
-- 
-- The layout is the static layout that this widget will use.  It 
-- cannot be dependent on any streaming arguments, but a layout can have 
-- \"stretchy\" sides so that it can expand/shrink to fit an area.  Learn 
-- more about making layouts in 'UIType's UI Layout section -- specifically, 
-- check out the 'makeLayout' function and the 'LayoutType' data type.
-- 
-- The computation is where the logic of the widget is held.  This 
-- function takes as input the streaming argument a, the widget's state, 
-- a Rect of coordinates indicating the area that has been allotted for 
-- this widget, and the 'UIEvent' that is triggering this widget's activation 
-- (see the definition of 'UIEvent' in SOE).  The output consists of the 
-- streaming output, the new state, and the dirty bit, which represents 
-- whether the widget needs to be redrawn.
-- 
-- Lastly, the drawing routine takes the same Rect as the computation, a 
-- Bool that is true when this widget is in focus and false otherwise, 
-- and the current state of the widget (technically, this state is the 
-- one freshly returned from the computation).  Its output is the Graphic 
-- that this widget should display.

mkWidget :: s                                 -- ^ initial state
         -> Layout                            -- ^ layout
         -> (a -> s -> Rect -> UIEvent ->
             (b, s, DirtyBit))                -- ^ computation
         -> (Rect -> Bool -> s -> Graphic)    -- ^ drawing routine
         -> UISF a b
mkWidget i layout comp draw = proc a -> do
  rec s  <- delay i -< s'
      (b, s') <- mkUISF layout aux -< (a, s)
  returnA -< b
    where
      aux (ctx,f,t,e,(a,s)) = (db, f, g, nullTP, (b, s'))
        where
          rect = bounds ctx
          (b, s', db) = comp a s rect e
          g = {-scissorGraphic rect $ -} draw rect (snd f == HasFocus) s'

-- | Occasionally, one may want to display a non-interactive graphic in 
-- the UI.  'mkBasicWidget' facilitates this.  It takes a layout and a 
-- simple drawing routine and produces a non-interacting widget.
mkBasicWidget :: Layout               -- ^ layout
              -> (Rect -> Graphic)    -- ^ drawing routine
              -> UISF a a
mkBasicWidget layout draw = mkUISF layout $ \(ctx, f, _, _, a) ->
  (False, f, draw $ bounds ctx, nullTP, a)


-- | The toggle is useful in the creation of both 'checkbox'es and 'radio' 
-- buttons.  It displays on/off according to its input, and when the mouse 
-- is clicked on it, it will output True (otherwise it outputs False).
-- 
-- The UISF returned from a call to toggle accepts the state stream and 
-- returns whether the toggle is being clicked.

toggle :: (Eq s) => s                     -- ^ Initial state value
       -> Layout                          -- ^ The layout for the toggle
       -> (Rect -> Bool -> s -> Graphic)  -- ^ The drawing routine
       -> UISF s Bool
toggle iState layout draw = focusable $ 
  mkWidget iState layout process draw
  where
    process s s' _ e = (on, s, s /= s')
      where 
        on = case e of
          Button _ LeftButton True -> True
          SKey KeyEnter _     True -> True
          Key  ' '      _     True -> True
          _ -> False 

-- | The mkSlider widget builder is useful in the creation of all sliders.

mkSlider :: Eq a => Bool              -- ^ True for horizontal, False for vertical
         -> (a -> Int -> Int)         -- ^ A function for converting a value to a position
         -> (Int -> Int -> a)         -- ^ A function for converting a position to a value
         -> (Int -> Int -> a -> a)    -- ^ A function for determining how much to jump when 
                                      -- a click is on the slider but not the target
         -> a                         -- ^ The initial value for the slider
         -> UISF (SEvent a) a
mkSlider hori val2pos pos2val jump val0 = focusable $ 
  mkWidget (val0, Nothing) d process draw 
  where
    rotP p@(x,y) ((bx,by),_) = if hori then p else (bx + y - by, by + x - bx)
    rotR r@(p,(w,h)) bbx = if hori then r else (rotP p bbx, (h,w))
    (minw, minh) = (16 + padding * 2, 16 + padding * 2)
    (tw, th) = (16, 8)
    d = if hori then makeLayout (Stretchy minw) (Fixed minh)
                else makeLayout (Fixed minh) (Stretchy minw)
    val2pt val ((bx,by), (bw,_bh)) = 
      let p = val2pos val (bw - padding * 2 - tw)
      in (bx + p + padding, by + 8 - th `div` 2 + padding) 
    bar ((x,y),(w,_h)) = ((x + padding + tw `div` 2, y + 6 + padding), 
                         (w - tw - padding * 2, 4))
    draw b inFocus (val, _) = 
      let p@(mx,my) = val2pt val (rotR b b)
      in  shadowBox popped (rotR (p, (tw, th)) b) 
          // whenG inFocus (shadowBox marked $ rotR (p, (tw-2, th-2)) b) 
          // withColor bg (rectangleFilled $ rotR ((mx + 2, my + 2), (tw - 4, th - 4)) b) 
          // shadowBox pushed (rotR (bar (rotR b b)) b)
    process ea (val, s) b evt = (val', (val', s'), val /= val') 
      where
        (val', s') = case ea of
          Just a -> (a, s)
          Nothing -> case evt of
            Button pt' LeftButton down -> let pt = rotP pt' bbx in
              case (pt `inside` target, down) of
                (True, True) -> (val, Just (ptDiff pt val))
                (_, False)   -> (val, Nothing)
                (False, True) | pt `inside` bar' -> (clickonbar pt, s)
                _ -> (val, s)
            MouseMove pt' -> let pt = rotP pt' bbx in
              case s of
                Just pd -> (pt2val pd pt, Just pd)
                Nothing -> (val, s)
            SKey KeyLeft  _ True -> if hori then (jump (-1) bw val, s) else (val, s)
            SKey KeyRight _ True -> if hori then (jump 1    bw val, s) else (val, s)
            SKey KeyUp    _ True -> if hori then (val, s) else (jump (-1) bw val, s)
            SKey KeyDown  _ True -> if hori then (val, s) else (jump 1    bw val, s)
            SKey KeyHome  _ True -> (pos2val 0  (bw - 2 * padding - tw), s)
            SKey KeyEnd   _ True -> (pos2val bw (bw - 2 * padding - tw), s)
            _ -> (val, s)
        bbx@((bx,_by),(bw,_bh)) = rotR b b
        bar' = let ((x,y),(w,h)) = bar bbx in ((x,y-4),(w,h+8))
        target = (val2pt val bbx, (tw, th)) 
        ptDiff (x,_) val = 
          let (x', y') = val2pt val bbx
          in (x' + tw `div` 2 - x, y' + th `div` 2 - x)
        pt2val (dx, _dy) (x,_y) = pos2val (x + dx - bx - tw `div` 2) (bw - 2 * padding - tw)
        clickonbar (x',_y') = 
          let (x,_y) = val2pt val bbx
          in jump (if x' < x then -1 else 1) bw val


---------------
-- Cycle Box --
---------------
-- | cyclebox is a clickable widget that cycles through a predefined set 
--   set of appearances/output values.
cyclebox :: Layout -> [(Rect -> Bool -> Graphic, b)] -> Int -> UISF () b
cyclebox d lst start = constA Nothing >>> cycleboxS d lst start

-- | cycleboxS is a cyclebox that additionally accepts input events that 
--   can set it to a particular appearance/output.
cycleboxS :: Layout -> [(Rect -> Bool -> Graphic, b)] -> Int -> UISF (SEvent Int) b
cycleboxS d lst start = focusable $ 
  mkWidget start d process draw
  where
    len = length lst
    incr i = (i+1) `mod` len
    draw b inFocus i = (fst (lst!!i)) b inFocus
    process ei i b evt = (snd (lst!!i'), i', i /= i')
      where 
        j = fromMaybe i ei
        i' = case evt of
          Button _ LeftButton True -> incr j
          SKey KeyEnter _     True -> incr j
          Key ' ' _           True -> incr j
          _ -> j


------------------------------------------------------------
-- * Focus
------------------------------------------------------------

-- $ Any widget that wants to accept mouse button clicks or keystrokes 
-- must be focusable.  The focusable function below achieves this.

-- | Making a widget focusable makes it accessible to tabbing and allows 
-- it to see any mouse button clicks and keystrokes when it is actually 
-- in focus.
focusable :: UISF a b -> UISF a b
focusable (UISF layout f) = proc x -> do
  rec hasFocus <- delay False -< hasFocus'
      (y, hasFocus') <- UISF layout (h f) -< (x, hasFocus)
  returnA -< y
 where
  h fun (ctx, (myid,focus),t, inp, (a, hasFocus)) = do
    let (f, hasFocus') = case (focus, hasFocus, inp) of
          (HasFocus, _, _) -> (HasFocus, True)
          (SetFocusTo n, _, _) | n == myid -> (NoFocus, True)
          (DenyFocus, _, _) -> (DenyFocus, False)
          (_, _,    Button pt _ True) -> (NoFocus, pt `inside` bounds ctx)
          (_, True, SKey KeyTab ms True) -> if hasShiftModifier ms 
                                            then (SetFocusTo (myid-1), False) 
                                            else (SetFocusTo (myid+1), False)
          (_, _, _) -> (focus, hasFocus)
        focus' = if hasFocus' then HasFocus else DenyFocus
        inp' = if hasFocus' then (case inp of 
              SKey KeyTab _ _ -> NoUIEvent
              _ -> inp)
               else (case inp of 
              Button _ _ True -> NoUIEvent -- TODO: why "True" and not "_"?
              Key  _ _ _      -> NoUIEvent
              SKey _ _ _      -> NoUIEvent
              _ -> inp)
        redraw = hasFocus /= hasFocus'
    (db, _, g, cd, b, UISF newLayout fun') <- fun (ctx, (myid,focus'), t, inp', a)
    return (db || redraw, (myid+1,f), g, cd, (b, hasFocus'), UISF newLayout (h fun'))

-- | Although mouse button clicks and keystrokes will be available once a 
-- widget marks itself as focusable, the widget may also simply want to 
-- know whether it is currently in focus to change its appearance.  This 
-- can be achieved with the following signal function.
isInFocus :: UISF () Bool
isInFocus = getFocusData >>> arr ((== HasFocus) . snd)


------------------------------------------------------------
-- * Supplemental Drawing Function
------------------------------------------------------------

-- | A convenience function for making a box that appears to have a 
--  shadow.  This is accomplished by using four colors representing:
--
--  (Top outside, Top inside, Bottom inside, Bottom Outside).
--
--  This is designed to be used with the below values 'pushed', 
--  'popped', and 'marked'.
shadowBox :: (Color,Color,Color,Color) -> Rect -> Graphic
shadowBox (to,ti,bi,bo) ((x, y), (w, h)) = 
     withColor to (line (x, y) (x, y + h - 1) 
                // line (x, y) (x + w - 2, y)) 
  // withColor bo (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) 
                // line (x + w - 1, y) (x + w - 1, y + h - 1))
  // withColor ti (line (x + 1, y + 1) (x + 1, y + h - 2) 
                // line (x + 1, y + 1) (x + w - 3, y + 1)) 
  // withColor bi (line (x + 2, y + h - 2) (x + w - 2, y + h - 2) 
                // line (x + w - 2, y + 1) (x + w - 2, y + h - 2))

pushed, popped, marked :: (Color,Color,Color,Color)
-- | A 'pushed' 'shadowBox' appears as if it is pushed inward.
pushed = (MediumBeige, DarkBeige, VLightBeige, White)
-- | A 'popped' 'shadowBox' appears as if it pops outward.
popped = (VLightBeige, White, MediumBeige, DarkBeige)
-- | A 'marked' 'shadowBox' appears somewhat between popped and pushed 
--  and is designed to indicate that the box is at the ready.
marked = (MediumBeige, White, MediumBeige, White)


