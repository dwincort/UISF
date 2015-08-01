-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.AuxFunctions
-- Copyright   :  (c) Daniel Winograd-Cort 2014
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental
--
-- Auxiliary functions for use with UISF or other arrows.

{-# LANGUAGE Arrows, TupleSections, FlexibleContexts #-}

module FRP.UISF.AuxFunctions (
    -- * Types
    SEvent, Time, DeltaT, 
    getDeltaT, accumTime, 
    -- * Useful SF Utilities (Mediators)
    constA, constSF, 
    edge, 
    accum, unique, 
    hold, now, 
    mergeE, (~++), 
    concatA, runDynamic, foldA, foldSF, 
    maybeA, evMap, 
    -- * Delays and Timers
    ArrowCircuit(..), 
    vdelay, fdelay, 
    vcdelay, fcdelay, 
    timer, genEvents, 
    -- * Event buffer
    Tempo, BufferOperation(..), eventBuffer, eventBuffer', 
    
--    (=>>), (->>), (.|.),
--    snapshot, snapshot_,
) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Arrow.Operations
import Data.Sequence (empty, (<|), (|>), (><), 
                      viewl, ViewL(..), viewr, ViewR(..))
import qualified Data.Sequence as Seq
import Data.Maybe (listToMaybe)



--------------------------------------
-- Types
--------------------------------------

-- | SEvent is short for \"Stream Event\" and is a type synonym for Maybe.
type SEvent = Maybe

-- | Time is simply represented as a Double.
type Time = Double 

-- | DeltaT is a type synonym referring to a change in Time.
type DeltaT = Double

-- | This is a convenience function for any DeltaT ArrowReader
getDeltaT :: ArrowReader DeltaT a => a b DeltaT
getDeltaT = readState

-- | This function returns the accumulated delta times created by 
--  getDeltaT.  Thus, it is the "accumulated" time.
accumTime :: (ArrowCircuit a, ArrowReader DeltaT a) => a b Time
accumTime = getDeltaT >>> arr (Just . (+)) >>> accum 0

--------------------------------------
-- Useful SF Utilities (Mediators)
--------------------------------------

-- | constA is an arrowized version of const.
constA  :: Arrow a => c -> a b c
constA = arr . const

-- | constSF is a convenience composing 'constA' with the given SF.
constSF :: Arrow a => b -> a b d -> a c d
constSF s sf = constA s >>> sf

-- | edge generates an event whenever the Boolean input signal changes
--   from False to True -- in signal processing this is called an ``edge
--   detector,'' and thus the name chosen here.
edge :: ArrowCircuit a => a Bool (SEvent ())
edge = proc b -> do
    prev <- delay False -< b
    returnA -< if not prev && b then Just () else Nothing

-- | The signal function (accum v) starts with the value v, but then 
--   applies the function attached to the first event to that value 
--   to get the next value, and so on.
accum :: ArrowCircuit a => b -> a (SEvent (b -> b)) b
accum x = proc f -> do
    rec b <- delay x -< b'
        let b' = maybe b ($b) f
    returnA -< b'

-- | The signal function unique will produce an event each time its input 
--   signal changes.
unique :: Eq e => ArrowCircuit a => a e (SEvent e)
unique = proc e -> do
    prev <- delay Nothing -< Just e
    returnA -< if prev == Just e then Nothing else Just e

-- | hold is a signal function whose output starts as the value of the 
--   static argument.  This value is held until the first input event 
--   happens, at which point it changes to the value attached to that 
--   event, which it then holds until the next event, and so on.
hold :: ArrowCircuit a => b -> a (SEvent b) b
hold x = arr (fmap const) >>> accum x

-- | Now is a signal function that produces one event and then forever 
--   after produces nothing.  It is essentially an impulse function.
now :: ArrowCircuit a => a () (SEvent ())
now = constA Nothing >>> delay (Just ())

{-# DEPRECATED mergeE "As of UISF-0.4.0.0, mergeE is being removed as it's basically just mappend from Monoid." #-}
-- | mergeE merges two events with the given resolution function.
mergeE :: (a -> a -> a) -> SEvent a -> SEvent a -> SEvent a
mergeE _       Nothing     Nothing     = Nothing
mergeE _       le@(Just _) Nothing     = le
mergeE _       Nothing     re@(Just _) = re
mergeE resolve (Just l)    (Just r)    = Just (resolve l r)

{-# DEPRECATED (~++) "As of UISF-0.4.0.0, (~++) is being removed as it is equivalent to Monoid's mappend." #-}
-- | This is an infix specialization of 'mergeE' to lists.
(~++) :: SEvent [a] -> SEvent [a] -> SEvent [a]
(~++) = mergeE (++)

-- | Returns n samples of type b from the input stream at a time, 
--   updating after k samples.  This function is good for chunking 
--   data and is a critical component to fftA
quantize :: ArrowCircuit a => Int -> Int -> a b (SEvent [b])
quantize n k = proc d -> do
    rec (ds,c) <- delay ([],0) -< (take n (d:ds), c+1)
    returnA -< if c >= n && c `mod` k == 0 then Just ds else Nothing

-- | Combines the input list of arrows into one arrow that takes a 
--   list of inputs and returns a list of outputs.
concatA :: Arrow a => [a b c] -> a [b] [c]
concatA [] = arr $ const []
concatA (sf:sfs) = proc (b:bs) -> do
    c <- sf -< b
    cs <- concatA sfs -< bs
    returnA -< (c:cs)

-- | This essentially allows an arrow that processes b to c to take 
--   [b] and recursively generate cs, combining them all into a 
--   final output d.
foldA :: ArrowChoice a => (c -> d -> d) -> d -> a b c -> a [b] d
foldA merge i sf = h where 
  h = proc inp -> case inp of
    [] -> returnA -< i
    b:bs -> do
        c <- sf -< b
        d <- h  -< bs
        returnA -< merge c d

-- | This is a special case of foldA for lists.
runDynamic :: ArrowChoice a => a b c -> a [b] [c]
runDynamic = foldA (:) []

-- | For folding results of a list of signal functions.
foldSF :: Arrow a => (b -> c -> c) -> c -> [a () b] -> a () c
foldSF f c sfs = let inps = replicate (length sfs) () in
    constA inps >>> concatA sfs >>> arr (foldr f c)
--foldSF f b sfs =
--  foldr g (constA b) sfs where
--    g sfa sfb =
--      proc () -> do
--        s1  <- sfa -< ()
--        s2  <- sfb -< ()
--        returnA -< f s1 s2

-- | This behaves much like the maybe function except lifted to the 
--   ArrowChoice level.  The arrow behaves like its first argument 
--   when the input stream is Nothing and like its second when it is 
--   a Just value.
maybeA :: ArrowChoice a => a () c -> a b c -> a (Maybe b) c
maybeA nothing just = proc eb -> do
  case eb of
    Just b -> just -< b
    Nothing -> nothing -< ()

-- | This lifts the arrow to an event-based arrow that behaves as 
--   a constant stream of Nothing when there is no event.
evMap :: ArrowChoice a => a b c -> a (SEvent b) (SEvent c)
evMap a = maybeA (constA Nothing) (a >>> arr Just)

--------------------------------------
-- Delays and Timers
--------------------------------------

-- | fdelay is a delay function that delays for a fixed amount of time, 
--   given as the static argument.  It returns a signal function that 
--   takes the current time and an event stream and delays the event 
--   stream by the delay amount.
--   fdelay guarantees that the order of events in is the same as the 
--   order of events out and that no event will be skipped.  However, 
--   if events are too densely packed in the signal (compared to the 
--   clock rate of the underlying arrow), then some events may be 
--   over delayed.
fdelay :: (ArrowReader DeltaT a, ArrowCircuit a) => DeltaT -> a (SEvent b) (SEvent b)
fdelay d = proc e -> do
    t <- accumTime -< ()
    rec q <- delay empty -< maybe q' (\e' -> q' |> (t+d,e')) e
        let (ret, q') = case viewl q of
                EmptyL -> (Nothing, q)
                (t0,e0) :< qs -> if t >= t0 then (Just e0, qs) else (Nothing, q)
    returnA -< ret

-- | vdelay is a delay function that delays for a variable amount of time.
--   It takes the current time, an amount of time to delay, and an event 
--   stream and delays the event stream by the delay amount.
--   vdelay, like fdelay, guarantees that the order of events in is the 
--   same as the order of events out and that no event will be skipped.  
--   If the events are too dense or the delay argument drops too quickly, 
--   some events may be over delayed.
vdelay :: (ArrowReader DeltaT a, ArrowCircuit a) => a (DeltaT, SEvent b) (SEvent b)
vdelay = proc (d, e) -> do
    t <- accumTime -< ()
    rec q <- delay empty -< maybe q' (\e' -> q' |> (t,e')) e
        let (ret, q') = case viewl q of 
                EmptyL -> (Nothing, q)
                (t0,e0) :< qs -> if t-d >= t0 then (Just e0, qs) else (Nothing, q)
    returnA -< ret

-- | fcdelay is a continuous version of fdelay.  It takes an initial value 
--   to emit for the first dt seconds.  After that, the delay will always 
--   be accurate, but some data may be ommitted entirely.  As such, it is 
--   not advisable to use fcdelay for event streams where every event must 
--   be processed (that's what fdelay is for).
fcdelay :: (ArrowReader DeltaT a, ArrowCircuit a) => b -> DeltaT -> a b b
fcdelay i dt = proc v -> do
    t <- accumTime -< ()
    rec q <- delay empty -< q' |> (t+dt, v) -- this list has pairs of (emission time, value)
        let (ready, rest) = Seq.spanl ((<= t) . fst) q
            (ret, q') = case viewr ready of
                EmptyR -> (i, rest)
                _ :> (t', v') -> (v', (t',v') <| rest)
    returnA -< ret

-- | vcdelay is a continuous version of vdelay.  It will always emit the 
--   value that was produced dt seconds earlier (erring on the side of an 
--   older value if necessary).  Be warned that this version of delay can 
--   both omit some data entirely and emit the same data multiple times.  
--   As such, it is usually inappropriate for events (use vdelay).
--   vcdelay takes a 'maxDT' argument that stands for the maximum delay 
--   time that it can handle.  This is to prevent a space leak.
--   
--   Implementation note: Rather than keep a single buffer, we keep two 
--   sequences that act to produce a sort of lens for a buffer.  qlow has 
--   all the values that are older than what we currently need, and qhigh 
--   has all of the newer ones.  Obviously, as time moves forward and the 
--   delay amount variably changes, values are moved back and forth between 
--   these two sequences as necessary.
--   This should provide a slight performance boost.
vcdelay :: (ArrowReader DeltaT a, ArrowCircuit a) => DeltaT -> b -> a (DeltaT, b) b
vcdelay maxDT i = proc (dt, v) -> do
    t <- accumTime -< ()
    rec (qlow, qhigh) <- delay (empty,empty) -< 
                (dropMostWhileL ((< t-maxDT) . fst) qlow', qhigh' |> (t, v))
                    -- this is two lists with pairs of (initial time, value)
            -- We construct four subsequences:, a, b, c, and d.  They are ordered by time and we 
            -- have an invariant that a >< b >< c >< d is the entire buffer ordered by time.
        let (b,a) = Seq.spanr ((> t-dt)  . fst) qlow
            (c,d) = Seq.spanl ((<= t-dt) . fst) qhigh
            -- After the spans, the value we are looking for will be in either c or a.
            (ret, qlow', qhigh') = case viewr c of
                _ :> (t', v') -> (v', qlow >< c, d)
                EmptyR -> case viewr a of
                    _ :> (t', v') -> (v', a, b >< qhigh)
                    EmptyR -> (i, a, b >< qhigh)
    returnA -< ret
  where
    -- This function acts like a wrapper for Seq.dropWhileL that will never 
    -- leave the input queue empty (unless it started that way).  At worst, 
    -- it will leave the queue with its rightmost (latest in time) element.
    dropMostWhileL f q = if Seq.null q then empty else case viewl dq of
            EmptyL -> Seq.singleton $ Seq.index q (Seq.length q - 1)
            _ -> dq
        where
            dq = Seq.dropWhileL f q

-- | timer is a variable duration timer.
--   This timer takes the current time as well as the (variable) time between 
--   events and returns an SEvent steam.  When the second argument is non-positive, 
--   the output will be a steady stream of events.  As long as the clock speed is 
--   fast enough compared to the timer frequency, this should give accurate and 
--   predictable output and stay synchronized with any other timer and with 
--   time itself.
timer :: (ArrowReader DeltaT a, ArrowCircuit a) => a DeltaT (SEvent ())
timer = proc dt -> do
    now <- accumTime -< ()
    rec last <- delay 0 -< t'
        let ret = now >= last + dt
            t'  = latestEventTime last dt now
    returnA -< if ret then Just () else Nothing
  where
    latestEventTime last dt now | dt <= 0 = now
    latestEventTime last dt now = 
        if now > last + dt
        then latestEventTime (last+dt) dt now
        else last


-- | genEvents is a timer that instead of returning unit events 
--   returns the next element of the input list.  When the input 
--   list is empty, the output stream becomes all Nothing.
genEvents :: (ArrowReader DeltaT a, ArrowCircuit a) => [b] -> a DeltaT (SEvent b)
genEvents lst = proc dt -> do
    e <- timer -< dt
    rec l <- delay lst -< maybe l (const $ drop 1 l) e
    returnA -< maybe Nothing (const $ listToMaybe l) e


--------------------------------------
-- Event buffer
--------------------------------------

-- | Tempo is just a Double.
type Tempo = Double

-- | The BufferOperation data type wraps up the data and operational commands 
--   to control an 'eventbuffer'.
data BufferOperation b = 
      NoBOp -- ^ No Buffer Operation
    | ClearBuffer -- ^ Erase the buffer
    | SkipAheadInBuffer DeltaT  -- ^ Skip ahead a certain amount of time in the buffer
    | MergeInBuffer  [(DeltaT, b)]    -- ^ Merge data into the buffer
    | AppendToBuffer [(DeltaT, b)]    -- ^ Append data to the end of the buffer
    | SetBufferPlayStatus Bool (BufferOperation b) -- ^ Set a new play status (True = Playing, False = Paused)
    | SetBufferTempo Tempo (BufferOperation b) -- ^ Set the buffer's tempo

-- | eventBuffer allows for a timed series of events to be prepared and 
--   emitted.  The streaming input is a BufferOperation, described above.  
--   Note that the default play status is playing and the default tempo 
--   is 1.  Just as MIDI files have events timed based 
--   on ticks since the last event, the events here are timed based on 
--   seconds since the last event.  If an event is to occur 0.0 seconds 
--   after the last event, then it is assumed to be played at the same 
--   time as the last event, and all simultaneous events are emitted 
--   at the same timestep. In addition to any events emitted, a 
--   streaming Bool is emitted that is True if the buffer is empty and 
--   False if the buffer is full (meaning that events will still come).
eventBuffer :: (ArrowReader DeltaT a, ArrowCircuit a) => a (BufferOperation b) (SEvent [b], Bool)
eventBuffer = arr (,()) >>> second getDeltaT >>> eventBuffer'

-- | eventBuffer' is a version that takes Time explicitly rather than 
--   with ArrowTime.
eventBuffer' :: ArrowCircuit a => a (BufferOperation b, DeltaT) (SEvent [b], Bool)
eventBuffer' = proc (bo', dt) -> do
    let (bo, doPlay', tempo') = collapseBO bo'
    doPlay <- hold True -< doPlay'
    tempo <- hold 1 -< tempo'
    rec buffer <- delay []   -< buffer' --the buffer
        let bufdt = tempo * dt
            (nextMsgs, buffer') = if doPlay 
                -- Subtract delta time, update the buffer, and get any events that are ready
                then getNextEvent (update (subTime buffer bufdt) bo)
                -- Regardless, update the buffer based on the operation
                else (Nothing, update buffer bo)
    returnA -< (nextMsgs, null buffer')
  where 
    subTime :: [(DeltaT, b)] -> DeltaT -> [(DeltaT, b)]
    subTime [] _ = []
    subTime ((bt,b):bs) dt = if bt < dt then (0,b):subTime bs (dt-bt) else (bt-dt,b):bs
    getNextEvent :: [(DeltaT, b)] -> (SEvent [b], [(DeltaT, b)])
    getNextEvent buffer = 
        let (es,rest) = span ((<=0).fst) buffer
            nextEs = map snd es
        in  if null buffer then (Nothing, [])
            else (Just nextEs, rest)
    update :: [(DeltaT, b)] -> BufferOperation b -> [(DeltaT, b)]
    update b NoBOp = b
    update _ ClearBuffer = []
    update b (SkipAheadInBuffer dt) = skipAhead b dt
    update b (MergeInBuffer b') = merge b b'
    update b (AppendToBuffer b') = b ++ b'
    update _ _ = error "The impossible happened in eventBuffer"
    merge :: [(DeltaT, b)] -> [(DeltaT, b)] -> [(DeltaT, b)]
    merge b [] = b
    merge [] b = b
    merge ((bt1,b1):bs1) ((bt2,b2):bs2) = if bt1 < bt2
        then (bt1,b1):merge bs1 ((bt2-bt1,b2):bs2)
        else (bt2,b2):merge ((bt1-bt2,b1):bs1) bs2
    skipAhead :: [(DeltaT, b)] -> DeltaT -> [(DeltaT, b)]
    skipAhead [] _ = []
    skipAhead b dt | dt <= 0 = b
    skipAhead ((bt,b):bs) dt = if bt < dt 
        then skipAhead bs (dt-bt)
        else (bt-dt,b):bs
    collapseBO :: BufferOperation b -> (BufferOperation b, Maybe Bool, Maybe Tempo)
    collapseBO (SetBufferPlayStatus b bo) = let (o, _, t) = collapseBO bo in (o, Just b, t)
    collapseBO (SetBufferTempo t bo) = let (o, b, _) = collapseBO bo in (o, b, Just t)
    collapseBO bo = (bo, Nothing, Nothing)



--------------------------------------
-- Yampa-style utilities
--------------------------------------

(=>>) :: SEvent a -> (a -> b) -> SEvent b
(=>>) = flip fmap
(->>) :: SEvent a -> b -> SEvent b
(->>) = flip $ fmap . const
(.|.) :: SEvent a -> SEvent a -> SEvent a
(.|.) = flip $ flip maybe Just

snapshot :: SEvent a -> b -> SEvent (a,b)
snapshot = flip $ fmap . flip (,)
snapshot_ :: SEvent a -> b -> SEvent b
snapshot_ = flip $ fmap . const -- same as ->>


