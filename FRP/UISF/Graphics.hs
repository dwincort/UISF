-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Graphics
-- Copyright   :  (c) Daniel Winograd-Cort 2015
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental

{-# LANGUAGE BangPatterns, FlexibleInstances, TypeSynonymInstances #-}
module FRP.UISF.Graphics (
  Point, Angle, Dimension, Rect,
  Color(..), RGB, colorToRGB, rgb, rgbE, extractRGB,
  Graphic,
  nullGraphic,
  overGraphic,
  withColor, withColor',
  text, textLines,
  ellipse, shearEllipse, line, polygon, polyline, polybezier, arc,
  circleFilled, circleOutline, rectangleFilled, rectangleOutline,
  translateGraphic, rotateGraphic, scaleGraphic,
  boundGraphic,
  UIText(..), UITexty(..),
  uitextToString, splitUIText, takeUIText, dropUIText, uitextLen,
  pureUIText, appendUIText, coloredUIText, rgbUIText, fontUIText,
  textWidth, textWithinPixels, textHeight,
  WrapSetting(..), prepText,
  textWidth', textWithinPixels', textHeight',
  BitmapFont(..)
  ) where


import FRP.UISF.Graphics.Types
import FRP.UISF.Graphics.Color
import FRP.UISF.Graphics.Text
import FRP.UISF.Graphics.Graphic
