{-# LANGUAGE Arrows #-}

import Prelude hiding (init)
import UISF
import Euterpea (osc, tableLinearN, tableSinesN)
import Control.CCA.Types
import Numeric.FFT (fft)
import Data.Complex
import Data.Map (Map)
import Data.Maybe (listToMaybe, catMaybes)
import qualified Data.Map as Map

-- | Alternative for working with Math.FFT instead of Numeric.FFT
--import qualified Math.FFT as FFT
--import Data.Array.IArray
--import Data.Array.CArray
--myFFT n lst = elems $ (FFT.dft) (listArray (0, n-1) lst)


--------------------------------------
-- Methods from Euterpea for making signals
--------------------------------------




--------------------------------------
-- Fast Fourier Transform
--------------------------------------

-- | Returns n samples of type b from the input stream at a time, 
--   updating after k samples.  This function is good for chunking 
--   data and is a critical component to fftA
quantize :: ArrowInit a => Int -> Int -> a b (SEvent [b])
quantize n k = proc d -> do
    rec (ds,c) <- init ([],0) -< (take n (d:ds), c+1)
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
fftA :: ArrowInit a => Int -> Int -> a Double (SEvent [Double])
fftA qf fp = proc d -> do
    carray <- quantize fp qf -< d :+ 0
    returnA -< fmap (map magnitude . take (fp `div` 2) . fft) carray



-- This example shows off the histogram and realtimeGraph widgets by 
-- summing two sin waves and displaying them.  Additionally, it makes 
-- use of two horizontal sliders.
-- This example also shows off convertToUISF and how to take a SigFun, 
-- of the type used to create sound, and convert it to a UISF.
fftEx :: UISF () ()
fftEx = proc _ -> do
    f1 <- hSlider (1, 2000) 440 -< ()
    _ <- leftRight (label "Freq 1: " >>> display) -< f1
    f2 <- hSlider (1, 2000) 440 -< ()
    _ <- leftRight (label "Freq 2: " >>> display) -< f2
    d <- convertToUISF 1000 0.1 myPureSignal -< (f1, f2)
    let fft = listToMaybe $ catMaybes $ map (snd . fst) d
        s = map (\((s, _), t) -> (s,t)) d
    _ <- histogram (makeLayout (Stretchy 10) (Fixed 150)) -< fft
    _ <- realtimeGraph (makeLayout (Stretchy 10) (Fixed 150)) 2 Black -< s
    returnA -< ()
  where
    squareTable = tableLinearN 2048 0 [(1024,0),(1,1),(1023,1)]
    --myPureSignal :: SF (Double, Double) (Double, SEvent [Double])
    myPureSignal = proc (f1, f2) -> do
        s1 <- osc (tableSinesN 4096 [1]) 0 -< f1
        s2 <- osc (tableSinesN 4096 [1]) 0 -< f2
        let s = (s1 + s2)/2
        fftData <- fftA 100 256 -< s
        returnA -< (s, fftData)

-- This test is run separately from the others.
t0 :: IO ()
t0 = runUIEx (500,600) "fft Test" fftEx
