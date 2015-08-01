-- Filename: fft.hs
-- Created by: Daniel Winograd-Cort
-- Created on: unknown
-- Last Modified by: Daniel Winograd-Cort
-- Last Modified on: 12/12/2013

-- -- DESCRIPTION --
-- This code was inspired by Euterpea.  It uses UISF to present a GUI that 
-- shows the sum of two waves (whose frequencies are specified by the user) 
-- as well as the Fast Fourier Transform of that sum.
-- 
-- This module requires the array and pure-fft packages.

{-# LANGUAGE Arrows #-}
module FRP.UISF.Examples.FFT where
import FRP.UISF
import Numeric.FFT (fft)
import Data.Complex
import Data.Map (Map)
import Data.Maybe (listToMaybe, catMaybes)
import qualified Data.Map as Map

import Data.Array.Unboxed



-- | Alternative for working with Math.FFT instead of Numeric.FFT
--import qualified Math.FFT as FFT
--import Data.Array.IArray
--import Data.Array.CArray
--myFFT n lst = elems $ (FFT.dft) (listArray (0, n-1) lst)


--------------------------------------
-- Sine wave oscillators
--------------------------------------

-- Table definition
type Table = UArray Int Double

-- Sine table generator. Takes an integer representing the number of samples to generate
-- and a list of relative intensities for the overtones of the sine wave.

tableSinesN :: Int -> [Double] -> Table
tableSinesN size amps = 
    let wave x   = sum (zipWith (*) [sin (2*pi*x*n) | n <- [1..]] amps)
        delta    = 1 / fromIntegral size
        waveform = take size $ map wave [0,delta..]
        divisor  = (maximum . map abs) waveform
     in listArray (0,size) (map (/divisor) waveform)

-- Two example sine tables.

tab1, tab2 :: Table
tab1 = tableSinesN 4096 [1] -- Basic sine wave
tab2 = tableSinesN 4096 [1.0,0.5,0.33]

-- Table-driven oscillator

osc :: ArrowCircuit a => Table -> Double -> a Double Double
osc table sr = proc freq -> do
    rec 
      let delta = 1 / sr * freq
          phase = if next > 1 then frac next else next
      next <- delay 0 -< frac (phase + delta)
    returnA -< ((table!).(`mod` size).round.(*rate)) phase
  where (_,size)    = bounds table
        rate        = fromIntegral size
        frac x = if x > 1 then x - fromIntegral (truncate x) else x


--------------------------------------
-- Fast Fourier Transform
--------------------------------------

-- | Returns n samples of type b from the input stream at a time, 
--   updating after k samples.  This function is good for chunking 
--   data and is a critical component to fftA
quantize :: ArrowCircuit a => Int -> Int -> a b (SEvent [b])
quantize n k = proc d -> do
    rec (ds,c) <- delay ([],0) -< (take n (d:ds), c+1)
    returnA -< if c >= n && c `mod` k == 0 then Just ds else Nothing

-- | Converts the vector result of a dft into a map from frequency to magnitude.
--   One common use is:
--      fftA >>> arr (fmap $ presentFFT clockRate)
presentFFT :: Double -> [Double] -> Map Double Double
presentFFT clockRate a = Map.fromList $ zipWith (curry mkAssoc) [0..] a where 
    mkAssoc (i,c) = (freq, c) where
        samplesPerPeriod = fromIntegral (length a)
        freq = fromIntegral i * (clockRate / samplesPerPeriod)

-- | Given a quantization frequency (the number of samples between each 
--   successive FFT calculation) and a fundamental period, this will decompose
--   the input signal into its constituent frequencies.
--   NOTE: The fundamental period must be a power of two!
fftA :: ArrowCircuit a => Int -> Int -> a Double (SEvent [Double])
fftA qf fp = proc d -> do
    carray <- quantize fp qf -< d :+ 0
    returnA -< fmap (map magnitude . take (fp `div` 2) . fft) carray


--------------------------------------
-- UISF Example
--------------------------------------

-- This example shows off the histogram and realtimeGraph widgets by 
-- summing two sin waves and displaying them.  Additionally, it makes 
-- use of two horizontal sliders.
-- This example also shows off asyncUISFV and how to take a SigFun, 
-- of the type used to create sound, and convert it to a UISF.
fftEx :: UISF () ()
fftEx = proc _ -> do
    f1 <- hSlider (1, 2000) 440 -< ()
    _ <- leftRight (label "Freq 1: " >>> display) -< f1
    f2 <- hSlider (1, 2000) 440 -< ()
    _ <- leftRight (label "Freq 2: " >>> display) -< f2
    d <- asyncVT sr 0.1 myAutomaton -< (f1, f2)
    let fft = listToMaybe $ catMaybes $ map (snd . fst) d
        s = map (\((s, _), t) -> (s,t)) d
    _ <- histogram (makeLayout (Stretchy 10) (Fixed 150)) -< fft
    _ <- realtimeGraph (makeLayout (Stretchy 10) (Fixed 150)) 2 Black -< s
    returnA -< ()
  where
    sr = 1000 -- signal rate
    myAutomaton = proc (f1, f2) -> do
        s1 <- osc tab1 sr -< f1
        s2 <- osc tab2 sr -< f2
        let s = (s1 + s2)/2
        fftData <- fftA 100 256 -< s
        returnA -< (s, fftData)

-- This test is run separately from the others.
main :: IO ()
main = runUI (defaultUIParams {uiSize=(500, 600), uiTitle="FFT Example"}) fftEx
