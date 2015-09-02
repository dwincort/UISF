{-# LANGUAGE Arrows #-}

import FRP.UISF
import FRP.UISF.Widget.Construction (focusable)
import qualified Data.Map.Strict as Map

{-
The mind map!  More description can go here in time.

-}

-- | The MindMap type is just a mapping of String keys to lists of 
--   String elements.
type MindMap = Map.Map String [String]

-- | This is a compound widget of a textbox and a button.  The 
--   textbox starts empty and the given String is for the text of the 
--   button.  When the button is pressed or when 'enter' is pressed 
--   while the textbox is in focus, textEntryField returns an event 
--   containing the current value of the textbox and then clears the 
--   textbox so that it can be reused.
textEntryField :: String -> UISF () (SEvent String)
textEntryField labl = rightLeft $ focusable $ proc () -> do
  b <- edge <<< button labl -< ()
  t <- textbox "" <<< delay Nothing -< fmap (const "") b
  returnA -< fmap (const t) b

uisf :: MindMap -> UISF () ()
uisf imap = proc () -> do
  l <- textEntryField "Lookup" -< ()
  a <- textEntryField "Add" -< ()
  key <- hold "" -< l
  m <- accum imap -< fmap (\v -> Map.insertWith (++) key [v]) a
  setLayout (makeLayout (Stretchy 1) (Fixed 12)) (arr id) -< ()
  leftRight (label "Key = " >>> displayStr) -< key
  runDynamic displayStr -< Map.findWithDefault [] key m
  returnA -< ()

main = runUI' (uisf Map.empty)


