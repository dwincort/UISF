{-# LANGUAGE Arrows #-}

-- Last modified by: Daniel Winograd-Cort
-- Last modified on: 5/25/2013

-- This file is a set of various UI examples showing off the features 
-- of the various widgets in UISF.

module FRP.UISF.Examples.Examples where

import FRP.UISF
import FRP.UISF.Graphics

import Numeric (showHex)

-- | This example displays the time from the start of the GUI application.
timeEx :: UISF () ()
timeEx = title "Time" $ accumTime >>> display <<< spacer

-- | This example shows off 'button's and state by presenting a plus and 
-- minus button with a counter that is adjusted by them.
buttonEx :: UISF () ()
buttonEx = title "Buttons" $ topDown $ proc _ -> do
  (x,y) <- leftRight (proc _ -> do
    x <- edge <<< button "+" -< ()
    y <- edge <<< button "-" -< ()
    returnA -< (x, y)) -< ()
  rec v <- delay 0 -< (case (x,y) of
            (Just _, Nothing) -> v+1
            (Nothing, Just _) -> v-1
            _ -> v)
  display -< v
  spacer -< ()

-- | This example shows off the 'checkbox' widgets.
checkboxEx :: UISF () ()
checkboxEx = title "Checkboxes" $ topDown $ proc _ -> do
  x <- checkbox "Monday" False -< ()
  y <- checkbox "Tuesday" True -< ()
  z <- checkbox "Wednesday" True -< ()
  let v = bin x ++ bin y ++ bin z
  displayStr -< v
  spacer -< ()
  where
    bin True = "1"
    bin False = "0"

-- | This example shows off the 'radio' button widget.
radioButtonEx :: UISF () ()
radioButtonEx = title "Radio Buttons" $ topDown $ radio list 0 >>> arr (list!!) >>> displayStr >>> spacer
  where
    list = ["apple", "orange", "banana"]

-- | This example shows off integral sliders (horizontal 'hiSlider's in 
--   this case).
shoppinglist :: UISF () ()
shoppinglist = title "Shopping List" $ topDown $ proc _ -> do
  a <- spacer <<< title "apples"  (hiSlider 1 (0,10) 3) -< ()
  b <- spacer <<< title "bananas" (hiSlider 1 (0,10) 7) -< () 
  title "total" display -< (a + b)

-- | This example shows off both vertical sliders as well as the 'canvas' 
-- widget.  The canvas widget can be used to easily create custom graphics 
-- in the GUI.  Here, it is used to make a color swatch that is 
-- controllable with RGB values by the sliders.
colorDemo :: UISF () ()
colorDemo = title "Color" $ leftRight $ proc _ -> do
  r <- newColorSlider (coloredUIText Red "R") -< ()
  g <- newColorSlider (coloredUIText Green "G") -< ()
  b <- newColorSlider (coloredUIText Blue "B") -< ()
  changed <- unique -< (r,g,b)
  pad (4,8,0,0) $ canvas' layout rect -< changed
  where
    layout = makeLayout (Stretchy 10) (Stretchy 10)
    newColorSlider l = title l $ topDown $ proc _ -> do
      v <- viSlider 16 (0,255) 0 -< ()
      _ <- setSize (22,22) displayStr -< showHex v ""
      returnA -< v
    rect (r,g,b) d = withColor' (rgbE r g b) (rectangleFilled ((0,0),d))

-- | This example shows off the 'textbox' widget.  Text can be typed in, and 
-- that text is transferred to the 'display' widget below when the button 
-- is pressed.
textboxdemo :: UISF () ()
textboxdemo = title "Saving Text" $ topDown $ proc _ -> do
  str <- leftRight $ label "Text: " >>> textbox "" -< Nothing
  b <- button "Save text to below" -< ()
  rec str' <- delay "" -< if b then str else str'
  leftRight $ label "Saved value: " >>> displayStr -< str' 

uitext :: UIText
uitext = coloredUIText Red  "H" `appendUIText`
    coloredUIText Yellow    "e" `appendUIText`
    coloredUIText Green     "l" `appendUIText`
    coloredUIText Cyan      "l" `appendUIText`
    coloredUIText Blue      "o" `appendUIText`
    coloredUIText Magenta  " W" `appendUIText`
    coloredUIText Red       "o" `appendUIText`
    coloredUIText Yellow    "r" `appendUIText`
    coloredUIText Green     "l" `appendUIText`
    coloredUIText Cyan      "d" `appendUIText`
    coloredUIText Blue      "!"
uitext' = fontUIText Helvetica18 uitext

uitextdemo = title "Color and Fonts" $ constA Nothing >>> textField CharWrap uitext' >>> constA ()

-- | This is the main demo that incorporates all of the other examples 
-- together.  In addition to demonstrating how 
-- different widgets can connect, it also shows off the tabbing 
-- behavior built in to the GUI.  Pressing tab cycles through focusable 
-- elements, and pressing shift-tab cycles in reverse.
main :: IO ()
main = runUI (defaultUIParams {uiSize=(500, 520), uiCloseOnEsc=True}) $ 
  (leftRight $ (bottomUp $ timeEx >>> buttonEx) >>> checkboxEx >>> radioButtonEx) >>>
  (leftRight $ shoppinglist >>> colorDemo) >>> textboxdemo >>> uitextdemo

linesWith s = cons (case break (== '\n') s of
                      (l, "") -> (l,[])
                      (l, s') -> (l++"\n", case s' of
                                             []    -> []
                                             _:s'' -> linesWith s''))
  where
    cons ~(h, t) =  h : t