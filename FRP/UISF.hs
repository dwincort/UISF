module FRP.UISF
  ( -- UI functions
    UISF 
  , runUI'              -- :: UISF () () -> IO ()
  , runUI               -- :: UIParams -> UISF () () -> IO ()
  , UIParams (..)       -- data UIParams = UIParams { ... }
  , defaultUIParams     -- :: UIParams
  , Dimension           -- type Dimension = (Int, Int)
    -- Widgets
  , label               -- :: String -> UISF a a
  , displayStr          -- :: UISF String ()
  , display             -- :: Show a => UISF a ()
  , withDisplay         -- :: Show b => UISF a b -> UISF a b
  , textbox             -- :: String -> UISF (Event String) String
  , title               -- :: String -> UISF a b -> UISF a b
  , spacer              -- :: UISF a a
  , button              -- :: String -> UISF () Bool
  , stickyButton        -- :: String -> UISF () Bool
  , checkbox            -- :: String -> Bool -> UISF () Bool
  , checkGroup          -- :: [(String, a)] -> UISF () [a]
  , radio               -- :: [String] -> Int -> UISF () Int
  , hSlider, vSlider    -- :: RealFrac a => (a, a) -> a -> UISF () a
  , hiSlider, viSlider  -- :: Integral a => a -> (a, a) -> a -> UISF () a
  , realtimeGraph       -- :: RealFrac a => Layout -> Time -> Color -> UISF [(a,Time)] ()
  , Color (..)          -- data Color = Black | Blue | Green | Cyan | Red | Magenta | Yellow | White
  , histogram           -- :: RealFrac a => Layout -> UISF (Event [a]) ()
  , histogramWithScale  -- :: RealFrac a => Layout -> UISF (SEvent [(a,String)]) ()
  , scrollable          -- :: Layout -> Dimension -> UISF a b -> UISF a b
  , listbox             -- :: (Eq a, Show a) => UISF ([a], Int) Int
  , canvas              -- :: Dimension -> UISF (Event Graphic) ()
  , canvas'             -- :: Layout -> (a -> Dimension -> Graphic) -> UISF (Event a) ()
  -- Widget Utilities
  , topDown, bottomUp, leftRight, rightLeft    -- :: UISF a b -> UISF a b
  , pad                 -- :: (Int, Int, Int, Int) -> UISF a b -> UISF a b
  , setSize             -- :: Dimension -> UISF a b -> UISF a b
  , setLayout           -- :: Layout -> UISF a b -> UISF a b
  , makeLayout          -- :: LayoutType -> LayoutType -> Layout
  , LayoutType (..)     -- data LayoutType = Stretchy { minSize :: Int } | Fixed { fixedSize :: Int }
  , Layout              -- data Layout = Layout {..}
  , getTime             -- :: UISF () Time
  , asyncUISFV          -- :: NFData b => Double -> Double -> Automaton a b -> UISF a ([b], Bool)
  , asyncUISFE          -- :: NFData b => Automaton a b -> UISF (SEvent a) (SEvent b)
  , module FRP.UISF.AuxFunctions
  , module Control.Arrow
  ) where

import FRP.UISF.UITypes
import FRP.UISF.UISF
import FRP.UISF.Widget
import FRP.UISF.SOE (Color (..))

import FRP.UISF.AuxFunctions
import Control.Arrow
