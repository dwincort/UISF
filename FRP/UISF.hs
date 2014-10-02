module FRP.UISF
  ( -- UI functions
    UISF 
  , runUI'              -- :: String -> UISF () () -> IO ()
  , runUI               -- :: Dimension -> String -> UISF () () -> IO ()
  , convertToUISF       -- :: NFData b => Double -> Double -> SF a b -> UISF a ([b], Bool)
  , asyncUISF           -- :: NFData b => Automaton a b -> UISF (ASyncInput a) (ASyncOutput b)
  , AsyncInput (..)     -- data AsyncInput a = AINoValue | AIClearBuffer | AIValue a
  , AsyncOutput (..)    -- data AsyncOutput b = AONoValue | AOCalculating Int | AOValue b
  , Dimension           -- type Dimension = (Int, Int)
  , topDown, bottomUp, leftRight, rightLeft    -- :: UISF a b -> UISF a b
  , setSize             -- :: Dimension -> UISF a b -> UISF a b
  , setLayout           -- :: Layout -> UISF a b -> UISF a b
  , pad                 -- :: (Int, Int, Int, Int) -> UISF a b -> UISF a b
  , getTime             -- :: UISF () Time
    -- Widgets
  , label               -- :: String -> UISF a a
  , displayStr          -- :: UISF String ()
  , display             -- :: Show a => UISF a ()
  , withDisplay         -- :: Show b => UISF a b -> UISF a b
  , textbox             -- :: UISF String String
  , textboxE            -- :: String -> UISF (Event String) String
  , title               -- :: String -> UISF a b -> UISF a b
  , button              -- :: String -> UISF () Bool
  , stickyButton        -- :: String -> UISF () Bool
  , checkbox            -- :: String -> Bool -> UISF () Bool
  , checkGroup          -- :: [(String, a)] -> UISF () [a]
  , radio               -- :: [String] -> Int -> UISF () Int
  , hSlider, vSlider    -- :: RealFrac a => (a, a) -> a -> UISF () a
  , hiSlider, viSlider  -- :: Integral a => a -> (a, a) -> a -> UISF () a
  , realtimeGraph       -- :: RealFrac a => Layout -> Time -> Color -> UISF (Time, [(a,Time)]) ()
  , histogram           -- :: RealFrac a => Layout -> UISF (Event [a]) ()
  , histogramWithScale  -- :: RealFrac a => Layout -> UISF (SEvent [(a,String)]) ()
  , listbox             -- :: (Eq a, Show a) => UISF ([a], Int) Int
  , canvas              -- :: Dimension -> UISF (Event Graphic) ()
  , canvas'             -- :: Layout -> (a -> Dimension -> Graphic) -> UISF (Event a) ()
  -- Widget Utilities
  , makeLayout          -- :: LayoutType -> LayoutType -> Layout
  , LayoutType (..)     -- data LayoutType = Stretchy { minSize :: Int } | Fixed { fixedSize :: Int }
  , Color (..)          -- data Color = Black | Blue | Green | Cyan | Red | Magenta | Yellow | White
  , module FRP.UISF.AuxFunctions
  , module Control.Arrow
  ) where

import FRP.UISF.UITypes
import FRP.UISF.UISF
import FRP.UISF.Widget
import FRP.UISF.SOE (Color (..))

import FRP.UISF.AuxFunctions
import Control.Arrow
