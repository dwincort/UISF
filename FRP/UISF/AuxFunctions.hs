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

{-# LANGUAGE Arrows, ScopedTypeVariables, TupleSections #-}

module FRP.UISF.AuxFunctions (
    -- * Types
    SEvent, Time, DeltaT, 
    ArrowTime, time, 
    ArrowIO, liftAIO, initialAIO, 
    -- * Useful SF Utilities (Mediators)
    constA, constSF, 
    edge, 
    accum, unique, 
    hold, now, 
    mergeE, (~++), 
    concatA, runDynamic, foldA, foldSF, 
    maybeA, evMap, 
    -- * Delays and Timers
    delay, 
    -- | delay is a unit delay.  It is exactly the delay from ArrowCircuit.
    vdelay, fdelay, 
    vcdelay, fcdelay, 
    timer, genEvents, 
    -- * Event buffer
    Tempo, BufferOperation(..), eventBuffer, eventBuffer', 
    
--    (=>>), (->>), (.|.),
--    snapshot, snapshot_,

    -- * Signal Function Asynchrony
    -- $asynchrony
    Automaton(..), 
    asyncV, asyncE, asyncC, asyncC'
) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.Automaton
import Data.Sequence (empty, (<|), (|>), (><), 
                      viewl, ViewL(..), viewr, ViewR(..))
import qualified Data.Sequence as Seq
import Data.Maybe (listToMaybe)

import Control.Concurrent
import Data.IORef
import Data.Foldable (toList)
import Control.DeepSeq


--------------------------------------
-- Types
--------------------------------------

-- | SEvent is short for \"Stream Event\" and is a type synonym for Maybe.
type SEvent = Maybe

-- | Time is simply represented as a Double.
type Time = Double 

-- | DeltaT is a type synonym referring to a change in Time.
type DeltaT = Double

-- | Instances of this class have arrowized access to time.  This is 
--   convenient in many cases where time is necessary but we would 
--   prefer not to make it an explicit argument.
class ArrowTime a where
    time :: a () Time

-- | Instances of the ArrowIO class have an arrowized ability to 
--   perform IO actions.
class Arrow a => ArrowIO a where
  -- | The liftAIO function lifts an IO action into an arrow.
  liftAIO :: (b -> IO c) -> a b c
  -- | The initialAIO function performs an IO action once upon the 
  --   initialization of the arrow and then uses the result of that 
  --   action to generate the arrow itself.
  initialAIO :: IO d -> (d -> a b c) -> a b c

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
    rec b <- delay x -< maybe b ($b) f
    returnA -< b

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
hold x = arr (fmap (const $)) >>> accum x

-- | Now is a signal function that produces one event and then forever 
--   after produces nothing.  It is essentially an impulse function.
now :: ArrowCircuit a => a () (SEvent ())
now = arr (const Nothing) >>> delay (Just ())

-- | mergeE merges two events with the given resolution function.
mergeE :: (a -> a -> a) -> SEvent a -> SEvent a -> SEvent a
mergeE _       Nothing     Nothing     = Nothing
mergeE _       le@(Just _) Nothing     = le
mergeE _       Nothing     re@(Just _) = re
mergeE resolve (Just l)    (Just r)    = Just (resolve l r)

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
fdelay :: (ArrowTime a, ArrowCircuit a) => DeltaT -> a (SEvent b) (SEvent b)
fdelay d = proc e -> do
    t <- time -< ()
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
vdelay :: (ArrowTime a, ArrowCircuit a) => a (DeltaT, SEvent b) (SEvent b)
vdelay = proc (d, e) -> do
    t <- time -< ()
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
fcdelay :: (ArrowTime a, ArrowCircuit a) => b -> DeltaT -> a b b
fcdelay i dt = proc v -> do
    t <- time -< ()
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
vcdelay :: (ArrowTime a, ArrowCircuit a) => DeltaT -> b -> a (DeltaT, b) b
vcdelay maxDT i = proc (dt, v) -> do
    t <- time -< ()
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
timer :: (ArrowTime a, ArrowCircuit a) => a DeltaT (SEvent ())
timer = proc dt -> do
    now <- time -< ()
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
genEvents :: (ArrowTime a, ArrowCircuit a) => [b] -> a DeltaT (SEvent b)
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
eventBuffer :: (ArrowTime a, ArrowCircuit a) => a (BufferOperation b) (SEvent [b], Bool)
eventBuffer = arr (,()) >>> second time >>> eventBuffer'

-- | eventBuffer' is a version that takes Time explicitly rather than 
--   with ArrowTime.
eventBuffer' :: ArrowCircuit a => a (BufferOperation b, Time) (SEvent [b], Bool)
eventBuffer' = proc (bo', t) -> do
    let (bo, doPlay', tempo') = collapseBO bo'
    doPlay <- hold True -< doPlay'
    tempo <- hold 1 -< tempo'
    rec tprev  <- delay 0    -< t   --used to calculate dt, the change in time
        buffer <- delay []   -< buffer' --the buffer
        let dt = tempo * (t-tprev) --dt will never be negative
            (nextMsgs, buffer') = if doPlay 
                -- Subtract delta time, update the buffer, and get any events that are ready
                then getNextEvent (update (subTime buffer dt) bo)
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



--------------------------------------
-- Signal Function Asynchrony
--------------------------------------

{- $asynchrony
Due to the ability for ArrowIO arrows to perform IO actions, they are 
obviously not guaranteed to be pure, and thus when we run them, we say 
that they run \"in real time\".  This means that the time between two 
samples can vary and is inherently unpredictable.

However, there are cases when we would like more control over the timing 
of certain arrowized computations.  For instance, sometimes we have a 
pure computation that we would like to run on a simulated clock.  This 
computation will expect to produce values at specific intervals, and 
because it's pure, that expectation can sort of be satisfied.

To achieve this, we allow these sub-computations to be performed 
asynchronously.  The following functions behave subtly differently 
to exhibit different forms of asynchrony for different use cases.
-}

-- | The asyncV functions is for \"Virtual time\" asynchrony.  The 
--   embedded signal function is given along with an expected 
--   clockrate, and the output conforms to that clockrate as well as it 
--   can.
--   
--   The clockrate is the simulated rate of the input signal function.
--   The buffer is the amount of time the given signal function is 
--   allowed to get ahead of real time.  The threadHandler is where the 
--   ThreadId of the forked thread is sent.
--
--   The output signal function takes and returns values in real time.  
--   The input must be paired with time, and the return values are the 
--   list of bs generated in the given time step, each time stamped.  
--   Note that the returned list may be long if the clockrate is much 
--   faster than real time and potentially empty if it's slower.
--   Note also that the caller can check the time stamp on the element 
--   at the end of the list to see if the inner, \"simulated\" signal 
--   function is performing as fast as it should.
asyncV :: (ArrowIO a, NFData c) => 
          Double             -- ^ Clockrate
       -> DeltaT             -- ^ Amount of time to buffer
       -> (ThreadId -> a () ()) -- ^ The thread handler
       -> (Automaton (->) b c)      -- ^ The automaton to convert to realtime
       -> a (b, Time) [(c, Time)]
asyncV clockrate buffer threadHandler sf = initialAIO iod darr where
  iod = do
    inp <- newEmptyMVar
    out <- newIORef empty
    timevar <- newEmptyMVar
    tid <- forkIO $ worker inp out timevar 1 1 sf
    return (tid, inp, out, timevar)
  darr (tid, inp, out, timevar) = proc (b,t) -> do
    _ <- threadHandler tid -< ()
    _ <- liftAIO (\b -> tryTakeMVar inp >> putMVar inp b) -< b -- send the worker the new input
    _ <- liftAIO (tryPutMVar timevar) -< t  -- update the time for the worker
    c <- liftAIO (atomicModifyIORef out) -< Seq.spanl (\(_,t0) -> t >= t0) --collect ready results
    returnA -< toList c
  -- worker processes the inner, "simulated" signal function.
  worker inp out timevar t count (Automaton sf) = do
    b <- readMVar inp     -- get the latest input
    let (c, sf') = sf b   -- do the calculation
    s <- deepseq c $ atomicModifyIORef out (\s -> (s |> (c, fromIntegral count/clockrate), s))
    t' <- if Seq.length s > 0 && snd (seqLastElem s) >= t+buffer then takeMVar timevar else return t
    worker inp out timevar t' (count+1) sf'
  seqLastElem s = Seq.index s (Seq.length s - 1)



-- | The asyncE (E for \"Event\") function takes a signal function (an Automaton) and converts 
--   it into an asynchronous event-based signal function usable in a ArrowIO signal 
--   function context.  The output arrow takes events of type a, feeds them to 
--   the asynchronously running input signal function, and returns events with the output 
--   b whenever they are ready.  The input signal function is expected to run slowly 
--   compared to the output one, but it is capable of running just as fast.
asyncE :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
          (ThreadId -> a () ()) -- ^ The thread handler
       -> (Automaton (->) b c)  -- ^ The automaton to convert to asynchronize
       -> a (SEvent b) (SEvent c)
asyncE threadHandler sf = initialAIO iod darr where
  iod = do
    inp <- newIORef empty
    out <- newIORef empty
    proceed <- newEmptyMVar
    tid <- forkIO $ worker proceed inp out sf
    return (tid, proceed, inp, out)
  -- count should start at 0
  darr (tid, proceed, inp, out) = proc eb -> do
    _ <- threadHandler tid -< ()
    case eb of
      Just b -> 
        liftAIO (\b -> atomicModifyIORef inp (\s -> (s |> b, ())) >> tryPutMVar proceed ()) -< b
      Nothing -> returnA -< False
    c <- liftAIO (const $ atomicModifyIORef out seqRestHead) -< ()
    returnA -< c
  -- worker processes the inner, "simulated" signal function.
  -- worker :: MVar () -> IORef (Seq a) -> IORef (Seq b) -> Automaton a b -> IO ()
  worker proceed inp out (Automaton sf) = do
    eb <- atomicModifyIORef inp seqRestHead
    case eb of
      Nothing -> takeMVar proceed >> worker proceed inp out (Automaton sf)
      Just b -> do
        let (c, sf') = sf b     -- do the calculation
        deepseq c $ atomicModifyIORef out (\s -> (s |> c, ()))
        worker proceed inp out sf'
  seqRestHead s = case viewl s of
      EmptyL  -> (s,  Nothing)
      a :< s' -> (s', Just a)

-- | The asyncC (C for \"Continuous time\") function allows a continuous 
--   signal function to run as fast as it can asynchronously.  There are 
--   no guarantees that all input data make it to the asynchronous signal 
--   function; if this is required, asyncE should be used instead.  
--   Rather, the embedded signal function runs as fast as it can on 
--   whatever value it has most recently seen.  Its results are 
--   bundled together in a list to be returned to the main signal 
--   function.
asyncC :: (ArrowIO a, NFData c) => 
          (ThreadId -> a () ()) -- ^ The thread handler
       -> (Automaton (->) b c)  -- ^ The automaton to convert to realtime
       -> a b [c]
--asyncC th sf = asyncC' th (const . return $ (), return) (first sf)
asyncC threadHandler sf = initialAIO iod darr where
  iod = do
    inp <- newEmptyMVar
    out <- newIORef empty
    tid <- forkIO $ worker inp out sf
    return (tid, inp, out)
  darr (tid, inp, out) = proc b -> do
    _ <- threadHandler tid -< ()
    _ <- liftAIO (\b -> tryTakeMVar inp >> putMVar inp b) -< b -- send the worker the new input
    c <- liftAIO (\_ -> atomicModifyIORef out (\s -> (empty,s))) -< () --collect ready results
    returnA -< toList c
  -- worker processes the inner, "simulated" signal function.
  worker inp out (Automaton sf) = do
    b <- readMVar inp     -- get the latest input
    let (c, sf') = sf b   -- do the calculation
    deepseq c $ atomicModifyIORef out (\s -> (s |> c, ()))
    worker inp out sf'


-- | This is a version of asyncC that does IO actions on either end of 
--   the embedded signal function.
asyncC' :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData b) => 
           (ThreadId -> a () ()) -- ^ The thread handler
        -> (b -> IO d, e -> IO ()) -- ^ Effectful input and output channels for the automaton
        -> (Automaton (->) (b,d) (c,e))  -- ^ The automaton to convert to asynchronize
        -> a b [c]
asyncC' threadHandler (iAction, oAction) sf = initialAIO iod darr where
  iod = do
    inp <- newIORef undefined
    start <- newEmptyMVar
    out <- newIORef empty
    tid <- forkIO $ takeMVar start >> worker inp out sf
    return (tid, inp, out, start)
  darr (tid, inp, out, start) = proc b -> do
    _ <- threadHandler tid -< ()
    _ <- liftAIO $ (\b -> deepseq b $ writeIORef inp b) -< b -- send the worker the new input
    c <- initialAIO (putMVar start ()) (const $ liftAIO (\_ -> atomicModifyIORef' out (\s -> (empty,s)))) -< () --collect ready results
    returnA -< toList c
  -- worker processes the inner, "simulated" signal function.
  worker inp out (Automaton sf) = do
    b <- readIORef inp     -- get the latest input
    d <- iAction b
    let ((c,e), sf') = sf (b,d)   -- do the calculation
    oAction e
    atomicModifyIORef' out (\s -> (s |> c, ()))
    worker inp out sf'

