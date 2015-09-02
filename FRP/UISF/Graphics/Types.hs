-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Graphics.Types
-- Copyright   :  (c) Daniel Winograd-Cort 2015
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental

{-# LANGUAGE BangPatterns, FlexibleInstances, TypeSynonymInstances #-}
module FRP.UISF.Graphics.Types (
  -- * Useful Types
  Point, Angle, Dimension, Rect
  ) where


------------------------------------------------------------
-- Useful Types
------------------------------------------------------------

-- | Point describes a point on the GUI.
type Point = (Int, Int)

-- | A dimension specifies size.
type Dimension = (Int, Int)

-- | A rectangle has a (bottom left) corner point and a dimension.
type Rect = (Point, Dimension)

-- | Angles are used when making arcs, circles, etc. or when 
--  performing rotations.  Angles are measured in Degrees
type Angle = Double
