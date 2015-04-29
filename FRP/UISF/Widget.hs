-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Widget
-- Copyright   :  (c) Daniel Winograd-Cort 2014
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental
--
-- A simple Graphical User Interface based on FRP. It uses the SOE
-- graphics library, and draws custom widgets on the screen.
-- 
-- SOE graphics uses OpenGL as the primitive drawing routine, and
-- GLFW library to provide window and input support.
-- 
-- The monadic UI concept is borrowed from Phooey by Conal Elliott.

{-# LANGUAGE RecursiveDo, Arrows, TupleSections #-}
{-# OPTIONS_HADDOCK prune #-}

module FRP.UISF.Widget where

import FRP.UISF.SOE
import FRP.UISF.UITypes
import FRP.UISF.UISF
import FRP.UISF.AuxFunctions (SEvent, Time, timer, edge, delay, constA, concatA)

import Control.Arrow
import Data.Maybe (fromMaybe)

------------------------------------------------------------
-- Shorthand and Helper Functions
------------------------------------------------------------

-- Default padding between border and content
padding :: Int
padding = 3 

-- Introduce a shorthand for overGraphic
(//) :: Graphic -> Graphic -> Graphic
(//) = overGraphic

-- And a nice way to make a graphic under only certain conditions
whenG :: Bool -> Graphic -> Graphic
whenG b g = if b then g else nullGraphic

------------------------------------------------------------
-- * Widgets
------------------------------------------------------------

----------------
-- Text Label --
----------------
-- | Labels are always left aligned and vertically centered.
label :: String -> UISF a a
label s = mkBasicWidget layout draw
  where
    (minw, minh) = (length s * 8 + padding * 2, 16 + padding * 2)
    layout = makeLayout (Fixed minw) (Fixed minh)
    draw ((x, y), (w, h)) = withColor Black $ text (x + padding, y + (h `div` 2) - 8) s

-----------------
-- Display Box --
-----------------
-- | DisplayStr is an output widget showing the instantaneous value of
--   a signal of strings.
displayStr :: UISF String ()
displayStr = mkWidget "" d (\v v' _ _ -> ((), v, v /= v')) draw 
  where
    minh = 16 + padding * 2
    d = makeLayout (Stretchy 8) (Fixed minh)
    draw b@((x,y), (w, _h)) _ s = 
      let n = (w - padding * 2) `div` 8
      in withColor Black (text (x + padding, y + padding) (take n s)) 
         // box pushed b 
         // withColor White (block b)

-- | displayAll is a widget that takes any show-able value and displays it.
displayAll :: Show a => UISF a ()
displayAll = arr show >>> displayStr

-- | displayKeylog is a widget that takes any show-able value (like that from a keylogger)
--  and displays it in a displayOverflow
displayKeylog :: Show a => UISF a ()
displayKeylog = arr show >>> displayOverflow         

-- | display is a widget that takes any show-able value and displays it.
display :: Show a => UISF a ()
display = arr show >>> displayStr

-- | withDisplay is a widget modifier that modifies the given widget 
--   so that it also displays its output value.
withDisplay :: Show b => UISF a b -> UISF a b
withDisplay sf = proc a -> do
  b <- sf -< a
  display -< b
  returnA -< b

-- | displayField is a stretchable widget that shows the value of a signal of strings,
--   and if there is overflow, the latter characters get cut off
displayField :: UISF String ()
displayField = mkWidget "" layout (\v v' _ _ -> ((), v, v /= v')) draw
 where  
   layout = makeLayout (Stretchy (8 + padding * 2)) (Stretchy (16 + padding * 2))
   draw b@((x, y), (w, h)) _ s =
     let n = (w - padding * 2) `div` 8 -- number of chars that can fit on a line
         maxH = floor (fromIntegral (h - padding * 2) / fromIntegral 16) -- max height
         dfRecur c = if c == maxH - 1 
                       then dfRecurHelp c
                         else dfRecurHelp c // dfRecur (c + 1)
         dfRecurHelp c = withColor Black 
           (text (x + padding, y + padding + c * 16)
           (take n (drop (n * c) s)))
     in (dfRecur 0)
         // box pushed b
         // withColor White (block b)

-- | displayOverflow is the same as displayField, but when characters overflow
--   the earlier characters are replaced
displayOverflow :: UISF String ()
displayOverflow = mkWidget "" layout (\v v' _ _ -> ((), v, v /= v')) draw
 where  
   layout = makeLayout (Stretchy (8 + padding * 2)) (Stretchy (16 + padding * 2))
   draw b@((x, y), (w, h)) _ s =
     let n = (w - padding * 2) `div` 8 -- number of chars that can fit on a line
         maxH = floor (fromIntegral (h - padding * 2) / fromIntegral 16)
         excess = (length s) - (maxH * n)  -- number of chars over the max
         ofRecur c = if c == maxH - 1 
                       then ofRecurHelp c
                         else ofRecurHelp c // ofRecur (c + 1)
         ofRecurHelp c = if excess 0 
           then withColor Black 
             (text (x + padding, y + padding + c * 16)
             (take n (drop (n * c + excess) s)))
           else withColor Black
             (text (x + padding, y + padding + c * 16)
             (take n (drop (n * c) s)))
     in (ofRecur 0)
         // box pushed b
         // withColor White (block b)  

----------------
-- Keylogging --
----------------
-- | The keylog widgets allow the user to collect keyboard data at various
--   levels of granularity. As of now, the
--   keyloggers still do not collect data unless focused. It currently supports
--   all special keys as well as regular keys.

-- | The basic keylogger collects keystrokes and puts them into a string that
--   separates each stroke by a space. This allows you to differentiate between
--   special key presses and actually typing out the name of the SKey in all caps
--   EX: "BACKSPACE" vs "B A C K S P A C E" vs "B  A  C  K  S  P  A  C  E"
keylogBasic :: UISF a [Char]
keylogBasic = mkWidget "" nullLayout comp (\_ _ _ -> nullGraphic)
  where comp _ s _ e = (s', s', False)
         where s' = case e of
                     SKey BACKSPACE _ True -> s ++ "BACKSPACE "
                     SKey DEL       _ True -> s ++ "DEL "
                     SKey LEFT      _ True -> s ++ "LEFT "
                     SKey RIGHT     _ True -> s ++ "RIGHT "
                     SKey UP        _ True -> s ++ "UP "
                     SKey DOWN      _ True -> s ++ "DOWN "
                     SKey END       _ True -> s ++ "END "
                     SKey HOME      _ True -> s ++ "HOME "
                     SKey ENTER     _ True -> s ++ "ENTER "
                     Key k _ True ->  s ++ [k, ' ']
                     _ -> s

-- | The toggle keylogger allows the user to set an "on" bit to determine whether
--   or not the widget should be collecting data
keylogToggle :: UISF Bool [Char]
keylogToggle = mkWidget (False, "") nullLayout comp (\_ _ _ -> nullGraphic)
  where comp a s _ e = if a == True then ((snd s'), s', False) else ((snd s), s, False)
         where s' = case e of
                     SKey BACKSPACE _ True -> (True, (snd s) ++ "BACKSPACE ")
                     SKey DEL       _ True -> (True, (snd s) ++ "DEL ")
                     SKey LEFT      _ True -> (True, (snd s) ++ "LEFT ")
                     SKey RIGHT     _ True -> (True, (snd s) ++ "RIGHT ")
                     SKey UP        _ True -> (True, (snd s) ++ "UP ")
                     SKey DOWN      _ True -> (True, (snd s) ++ "DOWN ")
                     SKey END       _ True -> (True, (snd s) ++ "END ")
                     SKey HOME      _ True -> (True, (snd s) ++ "HOME ")
                     SKey ENTER     _ True -> (True, (snd s) ++ "ENTER ")
                     Key k _ True ->  (True, (snd s) ++ [k, ' '])
                     _ -> s

-- | The readable keylogger outputs a string without spaces between elements.
--   This was not made with the toggle bit, as parsing this string is not safe, anyways.
--   This widget acts like a textbox without a cursor that registers SKeys
keylogReadable :: UISF a [Char]
keylogReadable = mkWidget "" nullLayout comp (\_ _ _ -> nullGraphic)
  where comp _ s _ e = (s', s', False) 
         where s' = case e of
                     SKey BACKSPACE _ True -> s ++ "BACKSPACE"
                     SKey DEL       _ True -> s ++ "DEL"
                     SKey LEFT      _ True -> s ++ "LEFT"
                     SKey RIGHT     _ True -> s ++ "RIGHT"
                     SKey UP        _ True -> s ++ "UP"
                     SKey DOWN      _ True -> s ++ "DOWN"
                     SKey END       _ True -> s ++ "END"
                     SKey HOME      _ True -> s ++ "HOME"
                     SKey ENTER     _ True -> s ++ "ENTER"
                     Key k _ True ->  s ++ [k]
                     _ -> s

-- | This experimental keylogger takes in a tuple of an "on bit" and the current time
--   and outputs a list of tuples of a tuple of the key pressed and the time since a
--   key was last pressed and the total time elapsed.
keylogExp :: UISF (Bool, Time) (Bool, [([Char], Time)], Time)
keylogExp = mkWidget (False, [], 0) nullLayout comp (\_ _ _ -> nullGraphic)
  where comp (on, t') s@(_, l, t) _ e = if on == True then (s', s', False) else (s, s, False)
         where s' = case e of
                     SKey BACKSPACE _ True -> (True, l ++ [("BACKSPACE", t' - t )], t')
                     SKey DEL       _ True -> (True, l ++ [("DEL", t' - t )], t')
                     SKey LEFT      _ True -> (True, l ++ [("LEFT", t' - t )], t')
                     SKey RIGHT     _ True -> (True, l ++ [("RIGHT", t' - t )], t')
                     SKey UP        _ True -> (True, l ++ [("UP", t' - t )], t')
                     SKey DOWN      _ True -> (True, l ++ [("DOWN", t' - t )], t')
                     SKey END       _ True -> (True, l ++ [("END", t' - t )], t')
                     SKey HOME      _ True -> (True, l ++ [("HOME", t' - t )], t')
                     SKey ENTER     _ True -> (True, l ++ [("ENTER", t' - t )], t')
                     Key  k         _ True -> (True, l ++ [([k], t' - t)], t')
                     _                     -> s

--unsafe and really bad code used to format the output of keylogExp better
sel2 (_, elt2, _) = elt2
sel3 (_, _, elt3) = elt3

--------------
-- Text Box --
--------------
-- | Textbox is a widget showing the instantaneous value of a signal of 
-- strings.
-- 
-- The textbox widget will often be used with ArrowLoop (the rec keyword).  
-- However, it uses 'delay' internally, so there should be no fear of a blackhole.
-- 
-- The textbox widget supports mouse clicks and typing as well as the 
-- left, right, end, home, delete, and backspace special keys.
textbox :: UISF String String
textbox = focusable $ 
  conjoin $ proc s -> do
    inFocus <- isInFocus -< ()
    k <- getEvents -< ()
    ctx <- getCTX -< ()
    rec let (s', i) = if inFocus then update s iPrev ctx k else (s, iPrev)
        iPrev <- delay 0 -< i
    displayStr -< seq i s'
    inf <- delay False -< inFocus
    b <- if inf then timer -< 0.5 else returnA -< Nothing
    b' <- edge -< not inFocus --For use in drawing the cursor
    rec willDraw <- delay True -< willDraw'
        let willDraw' = maybe willDraw (const $ not willDraw) b --if isJust b then not willDraw else willDraw
    canvas' displayLayout drawCursor -< case (inFocus, b, b', i == iPrev) of
              (True,  Just _, _, _) -> Just (willDraw, i)
              (True,  _, _, False)  -> Just (willDraw, i)
              (False, _, Just _, _) -> Just (False, i)
              _ -> Nothing
    returnA -< s'
  where
    minh = 16 + padding * 2
    displayLayout = makeLayout (Stretchy 8) (Fixed minh)
    update s  i _ (Key c _ True)          = (take i s ++ [c] ++ drop i s, i+1)
    update s  i _ (SKey BACKSPACE _ True) = (take (i-1) s ++ drop i s, max (i-1) 0)
    update s  i _ (SKey DEL       _ True) = (take i s ++ drop (i+1) s, i)
    update s  i _ (SKey LEFT      _ True) = (s, max (i-1) 0)
    update s  i _ (SKey RIGHT     _ True) = (s, min (i+1) (length s))
    update s _i _ (SKey END       _ True) = (s, length s)
    update s _i _ (SKey HOME      _ True) = (s, 0)
    update s _i c (Button (x,_) True True) = (s, min (length s) $ (x - xoffset c) `div` 8)
    update s  i _ _                        = (s, max 0 $ min i $ length s)
    drawCursor (False, _) _ = nullGraphic
    drawCursor (True, i) (w,_h) = 
        let linew = padding + i*8
        in if linew > w then nullGraphic else withColor Black $
            line (linew, padding) (linew, 16+padding)
    xoffset = fst . fst . bounds

-- | This variant of the textbox takes a static argument that is 
--   the initial value in the textbox.  Then, it takes a stream of 
--   'SEvent String' and only externally updates the contents of the 
--   textbox when an event occurs.
textboxE :: String -> UISF (SEvent String) String
textboxE startingVal = proc ms -> do
  rec s  <- delay startingVal -< ts
      ts <- textbox -< maybe s id ms
  returnA -< ts

-----------
-- Title --
-----------
-- | Title frames a UI by borders, and displays a static title text.
title :: String -> UISF a b -> UISF a b
title str (UISF fl f) = UISF layout h where
  (tw, th) = (length str * 8, 16)
  drawit ((x, y), (w, h)) = 
    withColor Black (text (x + 10, y) str) 
    // withColor' bg (block ((x + 8, y), (tw + 4, th))) 
    // box marked ((x, y + 8), (w, h - 16))
  layout ctx = let l = fl ctx in l { hFixed = hFixed l + tw, vFixed = vFixed l + 36 }
                                    -- ,minW = max (tw + 20) (minW l), minH = max 36 (minH l) }
  h (CTX flow bbx@((x,y), (w,h)) cj,foc,t,inp, a) = 
    let ctx' = CTX flow ((x + 4, y + 20), (w - 8, h - 32)) cj
    in do (db, foc', g, cd, b, uisf) <- f (ctx', foc, t, inp, a)
          return (db, foc', drawit bbx // g, cd, b, title str uisf)


------------
-- Button --
------------
-- | A button is a focusable input widget with a state of being on or off.  
-- It can be activated with either a button press or the enter key.
-- (Currently, there is no support for the space key due to non-special 
--  keys not having Release events.)
-- Buttons also show a static label.
-- 
-- The regular button is down as long as the mouse button or key press is 
-- down and then returns to up.
button :: String -> UISF () Bool
button = genButton False

-- | This version of the button is used for the keylogger so that SPACE
--   and ENTER do not stop the logging.
keylogButton :: String -> UISF () Bool
keylogButton = genButton' False

-- | The sticky button, on the other hand, once 
-- pressed, remains depressed until is is clicked again to be released.
-- Thus, it looks like a button, but it behaves more like a checkbox.
stickyButton :: String -> UISF () Bool
stickyButton = genButton True

-- | This version of the button is used for the keylogger so that SPACE
--   and ENTER do not stop the logging.
keylogStickyButton :: String -> UISF () Bool
keylogStickyButton = genButton' True

-- This is used to create the buttons.
genButton :: Bool -> String -> UISF () Bool
genButton sticky l = focusable $ 
  mkWidget False d (if sticky then processSticky else processRegular) draw
  where
    (tw, th) = (8 * length l, 16) 
    (minw, minh) = (tw + padding * 2, th + padding * 2)
    d = makeLayout (Stretchy minw) (Fixed minh)
    draw b@((x,y), (w,h)) inFocus down = 
      let x' = x + (w - tw) `div` 2 + if down then 0 else -1
          y' = y + (h - th) `div` 2 + if down then 0 else -1
      in withColor Black (text (x', y') l) 
         // whenG inFocus (box marked b)
         // box (if down then pushed else popped) b
    processRegular _ s b evt = (s', s', s /= s')
      where 
        s' = case evt of
          Button pt True down | pt `inside` b -> case (s, down) of
            (False, True) -> True
            (True, False) -> False
            _ -> s
          MouseMove pt       -> (pt `inside` b) && s
          SKey ENTER _ down -> down
          Key ' ' _ down -> down
          _ -> s
    processSticky _ s b evt = (s', s', s /= s')
      where 
        s' = case evt of
          Button pt True True | pt `inside` b -> not s
          SKey ENTER _ True -> not s
          Key ' ' _ True -> not s
          _ -> s

 -- This is used to create the keylogging buttons
genButton' :: Bool -> String -> UISF () Bool
genButton' sticky l = focusable $ 
  mkWidget False d (if sticky then processSticky else processRegular) draw
    where
      (tw, th) = (8 * length l, 16) 
      (minw, minh) = (tw + padding * 2, th + padding * 2)
      d = makeLayout (Stretchy minw) (Fixed minh)
      draw b@((x,y), (w,h)) inFocus down = 
        let x' = x + (w - tw) `div` 2 + if down then 0 else -1
            y' = y + (h - th) `div` 2 + if down then 0 else -1
        in withColor Black (text (x', y') l) 
           // whenG inFocus (box marked b)
           // box (if down then pushed else popped) b
      processRegular _ s b evt = (s', s', s /= s')
        where 
          s' = case evt of
            Button pt True down | pt `inside` b -> case (s, down) of
              (False, True) -> True
              (True, False) -> False
              _ -> s
            MouseMove pt       -> (pt `inside` b) && s
            _ -> s
      processSticky _ s b evt = (s', s', s /= s')
        where 
          s' = case evt of
            Button pt True True | pt `inside` b -> not s
            _ -> s

---------------
-- Check Box --
---------------
-- | Checkbox allows selection or deselection of an item.
--   It has a static label as well as an initial state.
checkbox :: String -> Bool -> UISF () Bool
checkbox l state = proc _ -> do
  rec s  <- delay state -< s'
      e  <- edge <<< toggle state d draw -< s
      let s' = maybe s (const $ not s) e
  returnA -< s'
  where
    (tw, th) = (8 * length l, 16) 
    (minw, minh) = (tw + padding * 2, th + padding * 2)
    d = makeLayout (Stretchy minw) (Fixed minh)
    draw ((x,y), (_w,h)) inFocus down = 
      let x' = x + padding + 16 
          y' = y + (h - th) `div` 2
          b = ((x + padding + 2, y + h `div` 2 - 6), (12, 12))
      in  withColor Black (text (x', y') l) 
          // whenG inFocus (box marked b)
          // whenG down 
             (withColor' gray3 $ polyline 
               [(x + padding + 5, y + h `div` 2),
                (x + padding + 7, y + h `div` 2 + 3),
                (x + padding + 11, y + h `div` 2 - 2)])
          // box pushed b 
          // withColor White (block b)


---------------------
-- Check Box Group --
---------------------
-- | The checkGroup widget creates a group of 'checkbox'es that all send 
--   their outputs to the same output stream. It takes a static list of 
--   labels for the check boxes and assumes they all start unchecked.
--   
--   The output stream is a list of each a value that was paired with a 
--   String value for which the check box is checked.
checkGroup :: [(String, a)] -> UISF () [a]
checkGroup sas = let (s, a) = unzip sas in
  constA (repeat ()) >>> 
  concatA (zipWith checkbox s (repeat False)) >>>
  arr (map fst . filter snd . zip a)

--checkGroup :: [String] -> UISF () [Bool]
--checkGroup ss = constA (repeat ()) >>> 
--                concatA (zipWith checkbox ss (repeat False))


-------------------
-- Radio Buttons --
-------------------
-- | Radio button presents a list of choices and only one of them can be 
-- selected at a time.  It takes a static list of choices (as Strings) 
-- and the index of the initially selected one, and the widget itself 
-- returns the continuous stream representing the index of the selected 
-- choice.
radio :: [String] -> Int -> UISF () Int
radio labels i = proc _ -> do
  rec s   <- delay i -< s''
      s'  <- aux 0 labels -< s
      let s'' = maybe s id s'
  returnA -< s''
  where
    aux :: Int -> [String] -> UISF Int (SEvent Int)
    aux _ [] = arr (const Nothing)
    aux j (l:ls) = proc n -> do
      u <- edge <<< toggle (i == j) d draw -< n == j
      v <- aux (j + 1) ls -< n
      returnA -< maybe v (const $ Just j) u
      where
        (tw, th) = (8 * length l, 16) 
        (minw, minh) = (tw + padding * 2, th + padding * 2)
        d = makeLayout (Stretchy minw) (Fixed minh)
        draw ((x,y), (_w,h)) inFocus down = 
          let x' = x + padding + 16 
              y' = y + (h - th) `div` 2
          in  withColor Black (text (x', y') l) 
              // whenG down (circle gray3 (x,y) (5,6) (9,10))
              // circle gray3 (x,y) (2,3) (12,13) 
              // circle gray0 (x,y) (2,3) (13,14) 
              // whenG inFocus (circle gray2 (x,y) (0,0) (14,15))

--------------
-- List Box --
--------------
-- | The listbox widget creates a box with selectable entries.
-- The input stream is the list of entries as well as which entry is 
-- currently selected, and the output stream is the index of the newly 
-- selected entry.  Note that the index can be greater than the length 
-- of the list (simply indicating no choice selected).
listbox :: (Eq a, Show a) => UISF ([a], Int) Int
listbox = focusable $ mkWidget ([], -1) layout process draw >>> delay (-1)
  where
    layout = makeLayout (Stretchy 80) (Stretchy 16)
    -- takes the rectangle to draw in and a tuple of the list of choices and the index selected
    lineheight = 16
    --draw :: Show a => Rect -> ([a], Int) -> Graphic
    draw rect@(_,(w,_h)) _ (lst, i) = 
        genTextGraphic rect i lst  
        // box pushed rect 
        // withColor White (block rect)
        where
          n = (w - padding * 2) `div` 8
          genTextGraphic _ _ [] = nullGraphic
          genTextGraphic ((x,y),(w,h)) i (v:vs) = (if i == 0
                then withColor White (text (x + padding, y + padding) (take n (show v)))
                     // withColor Blue (block ((x,y),(w,lineheight)))
                else withColor Black (text (x + padding, y + padding) (take n (show v)))) 
                     // genTextGraphic ((x,y+lineheight),(w,h-lineheight)) (i - 1) vs
    process :: Eq a => ([a], Int) -> ([a], Int) -> Rect -> UIEvent -> (Int, ([a], Int), Bool)
    process (lst,i) olds bbx e = (i', (lst, i'), olds /= (lst, i'))
        where
        i' = case e of
          Button pt True True -> boundCheck $ pt2index pt
          SKey DOWN _ True   -> min (i+1) (length lst - 1)
          SKey UP   _ True   -> max (i-1) 0
          SKey HOME _ True   -> 0
          SKey END  _ True   -> length lst - 1
          _ -> boundCheck i
        ((_,y),_) = bbx
        pt2index (_px,py) = (py-y) `div` lineheight
        boundCheck j = if j >= length lst then -1 else j


----------------
-- *** Sliders
----------------

-- $ Sliders are input widgets that allow the user to choose a value within 
-- a given range.  They come in both continous and discrete flavors as well 
-- as in both vertical and horizontal layouts.
-- 
-- Sliders take a boundary argument giving the minimum and maximum possible 
-- values for the output as well as an initial value.  In addition, discrete 
-- (or integral) sliders take a step size as their first argument.

hSlider, vSlider :: RealFrac a => (a, a) -> a -> UISF () a
-- | Horizontal Continuous Slider
hSlider = slider True
-- | Vertical Continuous Slider
vSlider = slider False
hiSlider, viSlider :: Integral a => a -> (a, a) -> a -> UISF () a
-- | Horizontal Discrete Slider
hiSlider = iSlider True
-- | Vertical Discrete Slider
viSlider = iSlider False

slider :: RealFrac a => Bool -> (a, a) -> a -> UISF () a
slider hori (min, max) = mkSlider hori v2p p2v jump
  where
    v2p v w = truncate ((v - min) / (max - min) * fromIntegral w)
    p2v p w =  
      let v = min + (fromIntegral (p - padding) / fromIntegral w * (max - min))
      in if v < min then min else if v > max then max else v
    jump d w v = 
      let v' = v + fromIntegral d * (max - min) * 16 / fromIntegral w
      in if v' < min then min else if v' > max then max else v'

iSlider :: Integral a => Bool -> a -> (a, a) -> a -> UISF () a
iSlider hori step (min, max) = mkSlider hori v2p p2v jump
  where
    v2p v w = w * fromIntegral (v - min) `div` fromIntegral (max - min)
    p2v p w =  
      let v = min + fromIntegral (round (fromIntegral (max - min) * 
              fromIntegral (p - padding) / fromIntegral w))
      in if v < min then min else if v > max then max else v
    jump d _w v = 
      let v' = v + step * fromIntegral d 
      in if v' < min then min else if v' > max then max else v'


---------------------
-- *** Graphs
---------------------

---------------------
-- Real Time Graph --
---------------------
-- | The realtimeGraph widget creates a graph of the data with trailing values.  
-- It takes a dimension parameter, the length of the history of the graph 
-- measured in time, and a color for the graphed line.
-- The signal function then takes an input stream of 
-- (value,time) event pairs, but since there can be zero or more points 
-- at once, we use [] rather than 'SEvent' for the type.
-- The values in the (value,time) event pairs should be between -1 and 1.
realtimeGraph :: RealFrac a => Layout -> Time -> Color -> UISF [(a,Time)] ()
realtimeGraph layout hist color = arr ((),) >>> first getTime >>>
  mkWidget ([(0,0)],0) layout process draw
  where draw _              _ ([],        _) = nullGraphic
        draw ((x,y), (w,h)) _ (lst@(_:_), t) = translateGraphic (x,y) $ 
          withColor color $ polyline (map (adjust t) lst)
          where adjust t (i,t0) = (truncate $ fromIntegral w * (hist + t0 - t) / hist,
                                   buffer + truncate (fromIntegral (h - 2*buffer) * (1 + i)/2))
                buffer = truncate $ fromIntegral h / 10
        removeOld _ [] = []
        removeOld t ((i,t0):is) = if t0+hist>=t then (i,t0):is else removeOld t is
        process (t,is) (lst,_) _ _ = ((), (removeOld t (lst ++ is), t), True) 



---------------
-- Histogram --
---------------
-- | The histogram widget creates a histogram of the input map.  It assumes 
-- that the elements are to be displayed linearly and evenly spaced.
histogram :: RealFrac a => Layout -> UISF (SEvent [a]) ()
histogram layout = 
  mkWidget Nothing layout process draw
  where process Nothing Nothing  _ _ = ((), Nothing, False)
        process Nothing (Just a) _ _ = ((), Just a, False) --TODO check if this should be True
        process (Just a) _       _ _ = ((), Just a, True)
        draw (xy, (w, h)) _ = translateGraphic xy . mymap (polyline . mkPts)
          where mkPts l  = zip (reverse $ xs $ length l) (map adjust . normalize . reverse $ l)
                xs n     = let k = n-1 in 0 : map (\x -> truncate $ fromIntegral (w*x) / fromIntegral k) [1..k]
                adjust i = buffer + truncate (fromIntegral (h - 2*buffer) * (1 - i))
                normalize lst = map (/m) lst where m = maximum lst
                buffer = truncate $ fromIntegral h / 10
                mymap :: ([a] -> Graphic) -> SEvent [a] -> Graphic
                mymap f (Just lst@(_:_)) = f lst
                mymap _ _ = nullGraphic

-- | The histogramWithScale widget creates a histogram and an x coordinate scale.
histogramWithScale :: RealFrac a => Layout -> UISF (SEvent [(a,String)]) ()
histogramWithScale layout = 
  mkWidget Nothing layout process draw
  where process Nothing Nothing  _ _ = ((), Nothing, False)
        process Nothing (Just a) _ _ = ((), Just a, False) --TODO check if this should be True
        process (Just a) _       _ _ = ((), Just a, True)
        draw (xy, (w, h)) _ = mymap (polyline . mkPts) mkScale
          where mkPts l  = zip (reverse $ xs $ length l) (map adjust . normalize . reverse $ l)
                xs n     = let k  = n-1
                               w' = w - sidebuffer * 2
                           in sidebuffer : map (\x -> sidebuffer + (truncate $ fromIntegral (w'*x) / fromIntegral k)) [1..k]
                adjust i = bottombuffer + truncate (fromIntegral (h - topbuffer - bottombuffer) * (1 - i))
                normalize lst = map (/m) lst where m = maximum lst
                topbuffer = truncate $ fromIntegral h / 10
                bottombuffer = max 20 topbuffer
                sidebuffer = 20
                mkScale l = foldl (\pg (x,s) -> translateGraphic xy (withColor Black (text (x-((8*length s) `div` 2), h-12) s)) // pg) 
                                  nullGraphic $ zip (xs $ length l) l
                mymap :: ([a] -> Graphic) -> ([String] -> Graphic) -> SEvent [(a,String)] -> Graphic
                mymap f g (Just lst@(_:_)) = let (fl,gl) = unzip lst in g gl // translateGraphic xy (f fl)
                mymap _ _ _ = nullGraphic


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
          g = scissorGraphic rect $ draw rect (snd f == HasFocus) s'

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
          Button _ True   True -> True
          SKey ENTER _ True -> True
          Key  ' '      _ True -> True
          _ -> False 

-- | The mkSlider widget builder is useful in the creation of all sliders.

mkSlider :: Eq a => Bool              -- ^ True for horizontal, False for vertical
         -> (a -> Int -> Int)         -- ^ A function for converting a value to a position
         -> (Int -> Int -> a)         -- ^ A function for converting a position to a value
         -> (Int -> Int -> a -> a)    -- ^ A function for determining how much to jump when 
                                      -- a click is on the slider but not the target
         -> a                         -- ^ The initial value for the slider
         -> UISF () a
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
      in  box popped (rotR (p, (tw, th)) b) 
          // whenG inFocus (box marked $ rotR (p, (tw-2, th-2)) b) 
          // withColor' bg (block $ rotR ((mx + 2, my + 2), (tw - 4, th - 4)) b) 
          // box pushed (rotR (bar (rotR b b)) b)
    process _ (val, s) b evt = (val', (val', s'), val /= val') 
      where
        (val', s') = case evt of
          Button pt' True down -> let pt = rotP pt' bbx in
            case (pt `inside` target, down) of
              (True, True) -> (val, Just (ptDiff pt val))
              (_, False)   -> (val, Nothing)
              (False, True) | pt `inside` bar' -> clickonbar pt
              _ -> (val, s)
          MouseMove pt' -> let pt = rotP pt' bbx in
            case s of
              Just pd -> (pt2val pd pt, Just pd)
              Nothing -> (val, s)
          SKey LEFT  _ True -> if hori then (jump (-1) bw val, s) else (val, s)
          SKey RIGHT _ True -> if hori then (jump 1    bw val, s) else (val, s)
          SKey UP    _ True -> if hori then (val, s) else (jump (-1) bw val, s)
          SKey DOWN  _ True -> if hori then (val, s) else (jump 1    bw val, s)
          SKey HOME  _ True -> (pos2val 0  (bw - 2 * padding - tw), s)
          SKey END   _ True -> (pos2val bw (bw - 2 * padding - tw), s)
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
              val' = jump (if x' < x then -1 else 1) bw val
          in (val', s)


-- | Canvas displays any graphics. The input is a signal of graphics
-- events because we only want to redraw the screen when the input
-- is there.
canvas :: Dimension -> UISF (SEvent Graphic) ()
canvas (w, h) = mkWidget nullGraphic layout process draw 
  where
    layout = makeLayout (Fixed w) (Fixed h)
    draw ((x,y),(w,h)) _ = translateGraphic (x,y)
    process (Just g) _ _ _ = ((), g, True)
    process Nothing  g _ _ = ((), g, False)

-- | canvas' uses a layout and a graphic generator.  This allows it to 
-- behave similarly to 'canvas', but it can adjust in cases with stretchy layouts.
canvas' :: Layout -> (a -> Dimension -> Graphic) -> UISF (SEvent a) ()
canvas' layout draw = mkWidget Nothing layout process drawit
  where
    drawit (pt, dim) _ = maybe nullGraphic (\a -> translateGraphic pt $ draw a dim)
    process (Just a) _ _ _ = ((), Just a, True)
    process Nothing  a _ _ = ((), a, False)


---------------
-- Cycle Box --
---------------
-- | cyclebox is a clickable widget that cycles through a predefined set 
--   set of appearances/output values.
cyclebox :: Layout -> [(Rect -> Bool -> Graphic, b)] -> Int -> UISF () b
cyclebox d lst start = focusable $ 
  mkWidget start d process draw
  where
    len = length lst
    incr i = (i+1) `mod` len
    draw b inFocus i = (fst (lst!!i)) b inFocus
    process _ i b evt = (snd (lst!!i'), i', i /= i')
      where 
        i' = case evt of
          Button _ True True -> incr i
          SKey ENTER _ True -> incr i
          Key ' ' _ True -> incr i
          _ -> i

-- | cyclebox' is a cyclebox that additionally accepts input events that 
--   can set it to a particular appearance/output.
cyclebox' :: Layout -> [(Rect -> Bool -> Graphic, b)] -> Int -> UISF (SEvent Int) b
cyclebox' d lst start = focusable $ 
  mkWidget start d process draw
  where
    len = length lst
    incr i = (i+1) `mod` len
    draw b inFocus i = (fst (lst!!i)) b inFocus
    process ei i b evt = (snd (lst!!i'), i', i /= i')
      where 
        j = fromMaybe i ei
        i' = case evt of
          Button _ True True -> incr j
          SKey ENTER _ True -> incr j
          Key ' ' _ True -> incr j
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
    lshift <- isKeyPressed LSHIFT
    rshift <- isKeyPressed RSHIFT
    let isShift = lshift || rshift
        (f, hasFocus') = case (focus, hasFocus, inp) of
          (HasFocus, _, _) -> (HasFocus, True)
          (SetFocusTo n, _, _) | n == myid -> (NoFocus, True)
          (DenyFocus, _, _) -> (DenyFocus, False)
          (_, _,    Button pt _ True) -> (NoFocus, pt `inside` bounds ctx)
          (_, True, SKey TAB _ True) -> if isShift then (SetFocusTo (myid-1), False) 
                                                    else (SetFocusTo (myid+1), False)
          (_, _, _) -> (focus, hasFocus)
        focus' = if hasFocus' then HasFocus else DenyFocus
        inp' = if hasFocus' then (case inp of 
              SKey TAB _ _ -> NoUIEvent
              _ -> inp)
               else (case inp of 
              Button _ _ True -> NoUIEvent
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
-- UI colors and drawing routine
------------------------------------------------------------

bg, gray0, gray1, gray2, gray3, blue3 :: RGB
bg = rgb 0xec 0xe9 0xd8
gray0 = rgb 0xff 0xff 0xff
gray1 = rgb 0xf1 0xef 0xe2
gray2 = rgb 0xac 0xa8 0x99
gray3 = rgb 0x71 0x6f 0x64
blue3 = rgb 0x31 0x3c 0x79

box :: [(RGB,RGB)] -> Rect -> Graphic
box [] _ = nullGraphic 
box ((t, b):cs) ((x, y), (w, h)) = 
  box cs ((x + 1, y + 1), (w - 2, h - 2)) 
  // withColor' t (line (x, y) (x, y + h - 1) 
                   // line (x, y) (x + w - 2, y)) 
  // withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) 
                   // line (x + w - 1, y) (x + w - 1, y + h - 1))

circle :: RGB -> Point -> Dimension -> Dimension -> Graphic
circle c (x, y) (w1, h1) (w2, h2) = 
  withColor' c $ arc (x + padding + w1, y + padding + h1) 
                     (x + padding + w2, y + padding + h2) 0 360

block :: Rect -> Graphic
block ((x,y), (w, h)) = polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

pushed, popped, marked :: [(RGB,RGB)]
pushed = [(gray2, gray0),(gray3, gray1)]
popped = [(gray1, gray3),(gray0, gray2)]
marked = [(gray2, gray0),(gray0, gray2)]

inside :: Point -> Rect -> Bool
inside (u, v) ((x, y), (w, h)) = u >= x && v >= y && u < x + w && v < y + h

