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

{-# LANGUAGE Arrows, ScopedTypeVariables #-}

module FRP.UISF.AuxFunctions (
    -- * Types
    SEvent, Time, DeltaT, 
    ArrowTime, time, 
    -- * Useful SF Utilities (Mediators)
    constA, constSF, 
    edge, 
    accum, unique, 
    hold, now, 
    mergeE, (~++), 
    concatA, foldA, foldSF, 
    -- * Delays and Timers
    delay, 
    -- | delay is a unit delay.  It is exactly the delay from ArrowCircuit.
    vdelay, fdelay, 
    vdelayC, fdelayC, 
    timer, genEvents, 
    -- * Event buffer
    BufferEvent (..), Tempo, BufferControl, eventBuffer, 
    
--    (=>>), (->>), (.|.),
--    snapshot, snapshot_,

    -- * Signal Function Conversions
    -- $conversions
    -- ** Types
    Automaton(..), toAutomaton, msfiToAutomaton, 
    -- *** Conversions
    -- $conversions2
    toMSF, toRealTimeMSF, 
    async, AsyncInput(..), AsyncOutput(..)
) where

import Prelude
import Control.Arrow
import Control.Arrow.Operations
import Data.Sequence (Seq, empty, (<|), (|>), (><), 
                      viewl, ViewL(..), viewr, ViewR(..))
import qualified Data.Sequence as Seq
import Data.Maybe (listToMaybe)

-- For use with MSF Conversions
import Control.Monad.Fix
import FRP.UISF.Types.MSF
import Data.Functor.Identity

import Control.Concurrent.MonadIO
import Data.IORef.MonadIO
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

-- | Instances of this class have arrowized access to the time
class ArrowTime a where
    time :: a () Time

--------------------------------------
-- Useful SF Utilities (Mediators)
--------------------------------------

-- | constA is an arrowized version of const
constA  :: Arrow a => c -> a b c
constA = arr . const

-- | constSF is a convenience
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

-- | A nice infix operator for merging event lists
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

-- | For folding results of a list of signal functions
foldSF ::  Arrow a => (b -> c -> c) -> c -> [a () b] -> a () c
foldSF f b sfs =
  foldr g (constA b) sfs where
    g sfa sfb =
      proc () -> do
        s1  <- sfa -< ()
        s2  <- sfb -< ()
        returnA -< f s1 s2


--------------------------------------
-- Delays and Timers
--------------------------------------

-- | delay is a unit delay.  It is exactly the delay from ArrowCircuit.


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

-- | fdelayC is a continuous version of fdelay.  It takes an initial value 
--   to emit for the first dt seconds.  After that, the delay will always 
--   be accurate, but some data may be ommitted entirely.  As such, it is 
--   not advisable to use fdelayC for event streams where every event must 
--   be processed (that's what fdelay is for).
fdelayC :: (ArrowTime a, ArrowCircuit a) => b -> DeltaT -> a b b
fdelayC i dt = proc v -> do
    t <- time -< ()
    rec q <- delay empty -< q' |> (t+dt, v) -- this list has pairs of (emission time, value)
        let (ready, rest) = Seq.spanl ((<= t) . fst) q
            (ret, q') = case viewr ready of
                EmptyR -> (i, rest)
                _ :> (t', v') -> (v', (t',v') <| rest)
    returnA -< ret

-- | vdelayC is a continuous version of vdelay.  It will always emit the 
--   value that was produced dt seconds earlier (erring on the side of an 
--   older value if necessary).  Be warned that this version of delay can 
--   both omit some data entirely and emit the same data multiple times.  
--   As such, it is usually inappropriate for events (use vdelay).
--   vdelayC takes a 'maxDT' argument that stands for the maximum delay 
--   time that it can handle.  This is to prevent a space leak.
--   
--   Implementation note: Rather than keep a single buffer, we keep two 
--   sequences that act to produce a sort of lens for a buffer.  qlow has 
--   all the values that are older than what we currently need, and qhigh 
--   has all of the newer ones.  Obviously, as time moves forward and the 
--   delay amount variably changes, values are moved back and forth between 
--   these two sequences as necessary.
--   This should provide a slight performance boost.
vdelayC :: (ArrowTime a, ArrowCircuit a) => DeltaT -> b -> a (DeltaT, b) b
vdelayC maxDT i = proc (dt, v) -> do
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

-- | The BufferEvent data type is used in tandem with 'BufferControl' 
--   to provide the right control information to 'eventBuffer'.
data BufferEvent b = 
      Clear -- ^ Erase the buffer
    | SkipAhead DeltaT  -- ^ Skip ahead a certain amount of time in the buffer
    | AddData      [(DeltaT, b)]    -- ^ Merge data into the buffer
    | AddDataToEnd [(DeltaT, b)]    -- ^ Add data to the end of the buffer

-- | Tempo is just a Double.
type Tempo = Double

-- | BufferControl has a Buffer event, a bool saying whether to Play (true) or 
--   Pause (false), and a tempo multiplier.
type BufferControl b = (SEvent (BufferEvent b), Bool, Tempo)

-- | eventBuffer allows for a timed series of events to be prepared and 
--   emitted.  The streaming input is a BufferControl, described above.  
--   Just as MIDI files have events timed based 
--   on ticks since the last event, the events here are timed based on 
--   seconds since the last event.  If an event is to occur 0.0 seconds 
--   after the last event, then it is assumed to be played at the same 
--   time as the last event, and all simultaneous events are emitted 
--   at the same timestep. In addition to any events emitted, a 
--   streaming Bool is emitted that is True if the buffer is empty and 
--   False if the buffer is full (meaning that events will still come).
eventBuffer :: (ArrowTime a, ArrowCircuit a) => a (BufferControl b) (SEvent [b], Bool)
eventBuffer = proc (bc, doPlay, tempo) -> do
    t <- time -< ()
    rec tprev  <- delay 0    -< t   --used to calculate dt, the change in time
        buffer <- delay []   -< buffer''' --the buffer
        let dt = tempo * (t-tprev) --dt will never be negative
            buffer' = if doPlay then subTime buffer dt else buffer
            buffer'' = maybe buffer' (update buffer') bc  --update the buffer based on the control
            (nextMsgs, buffer''') = if doPlay then getNextEvent buffer'' --get any events that are ready
                                    else (Nothing, buffer'')
    returnA -< (nextMsgs, null buffer''')
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
    update :: [(DeltaT, b)] -> BufferEvent b -> [(DeltaT, b)]
    update _ Clear = []
    update b (SkipAhead dt) = skipAhead b dt
    update b (AddData b') = merge b b'
    update b (AddDataToEnd b') = b ++ b'
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
-- Signal Function Conversions
--------------------------------------

-- $conversions
-- Due to the internal monad (specifically, because it could be IO), MSFs are 
-- not necessarily pure.  Thus, when we run them, we say that they run \"in 
-- real time\".  This means that the time between two samples can vary and is 
-- inherently unpredictable.
-- 
-- However, sometimes we have a pure computation that we would like to run 
-- on a simulated clock.  This computation will expect to produce values at 
-- specific intervals, and because it's pure, that expectation can sort of be 
-- satisfied.
-- 
-- The three functions in this section are three different ways to handle 
-- this case.  toMSF simply lifts the pure computation and \"hopes\" 
-- that the timing works the way you want.  As expected, this is not 
-- recommended.  async lets the pure computation compute in its own thread, 
-- but it puts no restrictions on speed.  toRealTimeMSF takes a signal rate 
-- argument and attempts to mediate between real and virtual time.
-- 
-- Rather than use MSF Identity as our default pure function, we present 
-- the Automaton type:
newtype Automaton a b = Automaton (a -> (b, Automaton a b))

-- | toAutomaton lifts a pure function to an Automaton.
toAutomaton :: (a -> b) -> Automaton a b
toAutomaton f = g where g = Automaton $ \a -> (f a, g)

-- | msfiToAutomaton lifts a pure MSF (i.e. one in the Identity monad) to 
--   an Automaton.
msfiToAutomaton :: MSF Identity a b -> Automaton a b
msfiToAutomaton (MSF msf) = Automaton $ second msfiToAutomaton . runIdentity . msf


-- $conversions2
-- The following two functions are for lifting Automatons to MSFs.  The first 
-- one is a quick and dirty solution, and the second one appropriately 
-- converts a simulated time Automaton into a real time one.

-- | This function should be avoided, as it directly converts the automaton 
--   with no real regard for time.
toMSF :: Monad m => Automaton a b -> MSF m a b
toMSF (Automaton f) = MSF $ return . second toMSF . f

-- | The clockrate is the simulated rate of the input signal function.
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
toRealTimeMSF :: forall m a b . (Monad m, MonadIO m, MonadFix m, NFData b) => 
                 Double             -- ^ Clockrate
              -> DeltaT             -- ^ Amount of time to buffer
              -> (ThreadId -> m ()) -- ^ The thread handler
              -> Automaton a b      -- ^ The automaton to convert to realtime
              -> MSF m (a, Time) [(b, Time)]
toRealTimeMSF clockrate buffer threadHandler sf = MSF initFun
  where
    -- initFun creates some refs and threads and is never used again.
    -- All future processing is done in sfFun and the spawned worker thread.
    initFun :: (a, Double) -> m ([(b, Double)], MSF m (a, Double) [(b, Double)])
    initFun (a, t) = do
        inp <- newIORef a
        out <- newIORef empty
        timevar <- newEmptyMVar
        tid <- liftIO $ forkIO $ worker inp out timevar 1 1 sf
        threadHandler tid
        sfFun inp out timevar (a, t)
    -- sfFun communicates with the worker thread, sending it the input values 
    -- and collecting from it the output values.
    sfFun :: IORef a -> IORef (Seq (b, Double)) -> MVar Double 
          -> (a, Double) -> m ([(b, Double)], MSF m (a, Double) [(b, Double)])
    sfFun inp out timevar (a, t) = do
        writeIORef inp a        -- send the worker the new input
        tryPutMVar timevar t    -- update the time for the worker
        b <- atomicModifyIORef out $ Seq.spanl (\(_,t0) -> t >= t0) --collect ready results
        return (toList b, MSF (sfFun inp out timevar))
    -- worker processes the inner, "simulated" signal function.
    worker :: IORef a -> IORef (Seq (b, Double)) -> MVar Double 
           -> DeltaT -> Integer -> Automaton a b -> IO ()
    worker inp out timevar t count (Automaton sf) = do
        a <- readIORef inp      -- get the latest input
        let (b, sf') = sf a     -- do the calculation
        s <- deepseq b $ atomicModifyIORef out (\s -> (s |> (b, fromIntegral count/clockrate), s))
        t' <- if Seq.length s > 0 && snd (seqLastElem s) >= t+buffer then takeMVar timevar else return t
        worker inp out timevar t' (count+1) sf'
    seqLastElem s = Seq.index s (Seq.length s - 1)


data AsyncInput a = AINoValue | AIClearBuffer | AIValue a
data AsyncOutput b = AONoValue | AOCalculating Int | AOValue b

-- | The async function takes a pure signal function (an Automaton) and converts 
--   it into an asynchronous signal function usable in a MonadIO signal 
--   function context.  The output MSF takes events of type a, feeds them to 
--   the asynchronously running input SF, and returns events with the output 
--   b whenever they are ready.  The input SF is expected to run slowly 
--   compared to the output MSF, but it is capable of running just as fast.
--   The input stream is a value, an option to clear any buffered values, or 
--   nothing, and the output stream is either a result value, a AOCalculating 
--   indicating that the asynchronous function is calculating and giving the 
--   buffer size, or nothing.
async :: forall m a b. (Monad m, MonadIO m, MonadFix m, NFData b) => 
                 (ThreadId -> m ()) -- ^ The thread handler
              -> Automaton a b      -- ^ The automaton to convert to asynchronize
              -> MSF m (AsyncInput a) (AsyncOutput b)
async threadHandler sf = delay AINoValue >>> MSF initFun
  where
    -- initFun creates some refs and threads and is never used again.
    -- All future processing is done in sfFun and the spawned worker thread.
    initFun :: (AsyncInput a) -> m ((AsyncOutput b), MSF m (AsyncInput a) (AsyncOutput b))
    initFun ea = do
        inp <- newIORef empty
        out <- newIORef empty
        proceed <- newEmptyMVar
        tid <- liftIO $ forkIO $ worker proceed inp out sf
        threadHandler tid
        sfFun 0 proceed inp out ea
    -- sfFun communicates with the worker thread, sending it the input values 
    -- and collecting from it the output values.
    sfFun :: Int -> MVar () -> IORef (Seq a) -> IORef (Seq b) 
          -> (AsyncInput a) -> m ((AsyncOutput b), MSF m (AsyncInput a) (AsyncOutput b))
    sfFun count proceed inp out ea = do
        count' <- case ea of
          AIValue a -> atomicModifyIORef inp (\is -> (is |> a, ())) >> tryPutMVar proceed () >> return (count+1)
          AIClearBuffer -> atomicModifyIORef inp (\_ -> (empty, ())) >> tryTakeMVar proceed >> return 0
          AINoValue -> return count
        b <- atomicModifyIORef out seqRestHead  -- collect any ready results
        let (b', count'') = maybe (Nothing, count') (\x -> (Just x, count'-1)) b
            b'' = maybe (if count'' <= 0 then AONoValue else AOCalculating count'') AOValue b'
        return (b'', MSF (sfFun count'' proceed inp out))
    -- worker processes the inner, "simulated" signal function.
    worker :: MVar () -> IORef (Seq a) -> IORef (Seq b) -> Automaton a b -> IO ()
    worker proceed inp out (Automaton sf) = do
        ea <- atomicModifyIORef inp seqRestHead
        case ea of
          Nothing -> takeMVar proceed >> worker proceed inp out (Automaton sf)
          Just a -> do
            let (b, sf') = sf a     -- do the calculation
            deepseq b $ atomicModifyIORef out (\s -> (s |> b, ()))
            worker proceed inp out sf'
    seqRestHead s = case viewl s of
        EmptyL  -> (s,  Nothing)
        a :< s' -> (s', Just a)

