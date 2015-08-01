-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Asynchrony
-- Copyright   :  (c) Daniel Winograd-Cort 2015
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental
--
-- This module provides functionality to allow UISF to perform 
-- asynchronous computations.

{-# LANGUAGE Arrows, TupleSections, FlexibleContexts #-}

module FRP.UISF.Asynchrony (
    -- * ArrowIO
    -- $arrowio
    ArrowIO(..),
    -- * Signal Function Asynchrony
    -- $automaton
    Automaton(..), 
    statefulFunctionToAutomaton,
    actionToIOAuto,
    statefulActionToIOAuto,
    pureAutoToIOAuto,
    -- $asynchrony
    asyncV, asyncVOn,
    asyncVT, asyncVTOn,
    asyncE, asyncEOn,
    asyncEIO, asyncEIOOn,
    asyncC, asyncCOn,
    asyncCIO, asyncCIOOn
) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.Automaton

import Data.Sequence (empty, (<|), (|>), (><), 
                      viewl, ViewL(..), viewr, ViewR(..))
import qualified Data.Sequence as Seq

import Control.Concurrent
import Control.Exception (bracket)
import Data.IORef
import Data.Foldable (toList)
import Control.DeepSeq

import FRP.UISF.AuxFunctions (DeltaT, Time, SEvent, accumTime)

{- $arrowio
Programming with UISF is not pure functional reactive programming.  
Indeed, the GUI nature demands a certain amount of effectful computation 
to really allow for an interface at all.  That said, widgets allow only 
very specific effects, and they are handled in such a way that the 
programming feels functional and reactive.

To allow for more generic needs, we introduce the ArrowIO class below, 
which UISF instantiates.  Note that using the ArrowIO functions should 
be done very carefully; if a lifted IO action blocks within the UISF, 
it will very likely block the entire GUI.  Thus, when possible, one is 
advised to use 'initialAIO' or 'terminalAIO', which are guaranteed to 
perform their 'IO' action only once.
-}

-- | Instances of the ArrowIO class have an arrowized ability to 
--   perform IO actions.
class Arrow a => ArrowIO a where
  -- | The liftAIO function lifts an IO action into an arrow.
  liftAIO :: (b -> IO c) -> a b c
  -- | The initialAIO function performs an IO action once when the 
  --   arrow is first initialized and then uses the result of that 
  --   action to generate the arrow itself.
  initialAIO :: IO d -> (d -> a b c) -> a b c
  -- | The terminalAIO function stores an IO action to be performed 
  --   once when the arrow terminates.  This will typically be some 
  --   sort of clean up behavior.
  terminalAIO :: IO () -> a b b


{- $automaton
The functions we want to perform asynchronously may be pure, but it 
is quite possible that they are somewhat more complex.  Thus, in 
keeping with the general arrowized nature of UISF, we allow the 
asynchronized functions to be Automatons.

Use the 'Arrow' 'arr' function to convert a pure function to an 
Automaton of any type.
-}

-- | Pure automatons from b to c are functions that take a b value 
--  and return a c value along with a new pure automaton from b to c.
type PureAuto = Automaton (->)

-- | IO automatons from b to c are functions that take a b value and 
--  perform an IO action that returns a c value along with a new IO 
--  automaton from b to c.
type IOAuto = Automaton (Kleisli IO)

-- | A convenience function for converting stateful functions to 
--  'Automaton's.
statefulFunctionToAutomaton :: ArrowLoop a => s -> ((b,s) -> (c,s)) -> Automaton a b c
statefulFunctionToAutomaton s f = loop $ second (delay s) >>> arr f

-- | A convenience function for converting IO actions to 'IOAuto's.
actionToIOAuto :: (b -> IO c) -> IOAuto b c
actionToIOAuto f = Automaton (Kleisli g) where
  g b = f b >>= (\c -> return (c, Automaton (Kleisli g)))

-- | A convenience function for converting stateful IO actions to 
--  'IOAuto's.
statefulActionToIOAuto :: s -> ((b,s) -> IO (c,s)) -> IOAuto b c
statefulActionToIOAuto s f = loop $ second (delay s) >>> actionToIOAuto f

-- | A convenience function for lifting a 'PureAuto' into an 'IOAuto'.
pureAutoToIOAuto :: PureAuto b c -> IOAuto b c
pureAutoToIOAuto (Automaton f) = Automaton (Kleisli g) where
  g b = let (c,f') = f b in return (c, pureAutoToIOAuto f')


{- $asynchrony
There are times when we want to perform some behavior during the 
course of running an arrow, but that behavior doesn't temporally 
align nicely with the main GUI.  For example:

- We have an FRP program that needs to be run at a fixed time rate, 
  with each tick through time well specified.

- We have a costly computation or potentially blocking action whose 
  result is not immediately 
  relevant, and we do not want to sacrifice the GUI's response time 
  by making it part of the main loop.  That is, we are okay with it 
  taking multiple ticks to finish, but we don't want to slow down 
  any current ticks.

- We want to perform an action in a private, tight loop.  For instance, 
  we need to poll a device, and we do not want the GUI's framerate tied 
  to the polling frequency in any way.

The following functions implement these different forms of asynchrony.
-}



-- | The asyncV functions is for \"Virtual time\" asynchrony.  The 
--   embedded signal function is given along with an expected 
--   clockrate, and the output conforms to that clockrate as well as it 
--   can.
--   
--   The clockrate is the simulated rate of the input signal function.
--   The buffer is the amount of time the given signal function is 
--   allowed to get ahead of real time.
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
          Double            -- ^ Clockrate
       -> DeltaT            -- ^ Amount of time to buffer
       -> PureAuto b c      -- ^ The automaton to run virtually
       -> a (b, Time) [(c, Time)]
asyncV = asyncVHelper forkIO

-- | A variant of 'asyncV' that uses 'forkOn' internally and thus takes 
--  a core ID to fork on.
asyncVOn :: (ArrowIO a, NFData c) => 
            Int             -- ^ Core to fork on
         -> Double          -- ^ Clockrate
         -> DeltaT          -- ^ Amount of time to buffer
         -> PureAuto b c    -- ^ The automaton to run virtually
         -> a (b, Time) [(c, Time)]
asyncVOn = asyncVHelper . forkOn

asyncVHelper :: (ArrowIO a, NFData c) => 
            (IO () -> IO ThreadId) -- ^ fork
         -> Double          -- ^ Clockrate
         -> DeltaT          -- ^ Amount of time to buffer
         -> PureAuto b c    -- ^ The automaton to convert to realtime
         -> a (b, Time) [(c, Time)]
asyncVHelper frk clockrate buffer sf = initialAIO iod darr where
  iod = do
    inp <- newIORef undefined
    start <- newEmptyMVar
    out <- newIORef empty
    timevar <- newEmptyMVar
    tid <- frk $ takeMVar start >> worker inp out timevar 1 1 sf
    return (tid, inp, out, timevar, start)
  darr (tid, inp, out, timevar, start) = proc (b,t) -> do
    _ <- terminalAIO (killThread tid) -< ()
    _ <- liftAIO (writeIORef inp) -< b -- send the worker the new input
    _ <- initialAIO (putMVar start ()) (const $ liftAIO $ tryPutMVar timevar) -< t  -- update the time for the worker
    c <- liftAIO (atomicModifyIORef out) -< Seq.spanl (\(_,t0) -> t >= t0) --collect ready results
    returnA -< toList c
  -- worker processes the inner, "simulated" signal function.
  worker inp out timevar t count (Automaton sf) = do
    b <- readIORef inp     -- get the latest input
    let (c, sf') = sf b   -- do the calculation
    s <- deepseq c $ atomicModifyIORef out (\s -> (s |> (c, fromIntegral count/clockrate), s))
    t' <- if Seq.length s > 0 && snd (seqLastElem s) >= t+buffer then takeMVar timevar else return t
    worker inp out timevar t' (count+1) sf'
  seqLastElem s = Seq.index s (Seq.length s - 1)


-- | A variant of 'asyncV' that uses a built-in time step of the arrow 
--  to accumulate and use the current time.
asyncVT :: (ArrowIO a, ArrowCircuit a, ArrowReader DeltaT a, NFData c) => 
           Double            -- ^ Clockrate
        -> DeltaT            -- ^ Amount of time to buffer
        -> PureAuto b c      -- ^ The automaton to run virtually
        -> a b [(c, Time)]
asyncVT clockrate buffer sf = (id &&& accumTime) >>> asyncV clockrate buffer sf

-- | A variant of 'asyncVT' that uses 'forkOn' internally and thus takes 
--  a core ID to fork on.
asyncVTOn :: (ArrowIO a, ArrowCircuit a, ArrowReader DeltaT a, NFData c) => 
             Int             -- ^ Core to fork on
          -> Double          -- ^ Clockrate
          -> DeltaT            -- ^ Amount of time to buffer
          -> PureAuto b c      -- ^ The automaton to run virtually
          -> a b [(c, Time)]
asyncVTOn i clockrate buffer sf = (id &&& accumTime) >>> asyncVOn i clockrate buffer sf



-- | The asyncE (E for \"Event\") function takes a signal function (an Automaton) and converts 
--   it into an asynchronous event-based signal function usable in a ArrowIO signal 
--   function context.  The output arrow takes events of type a, feeds them to 
--   the asynchronously running input signal function, and returns events with the output 
--   b whenever they are ready.  The input signal function is expected to run slowly 
--   compared to the output one, but it is capable of running just as fast.
asyncE :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
          PureAuto b c  -- ^ The automaton to convert to asynchronize
       -> a (SEvent b) (SEvent c)
asyncE = asyncEHelper forkIO

-- | A variant of 'asyncE' that uses 'forkOn' internally and thus takes 
--  a core ID to fork on.
asyncEOn :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
            Int             -- ^ Core to fork on
         -> PureAuto b c    -- ^ The automaton to asynchronize
         -> a (SEvent b) (SEvent c)
asyncEOn = asyncEHelper . forkOn

asyncEHelper :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
          (IO () -> IO ThreadId)    -- ^ fork
       -> PureAuto b c              -- ^ The automaton to asynchronize
       -> a (SEvent b) (SEvent c)
asyncEHelper frk sf = initialAIO iod darr where
  iod = do
    inp <- newIORef empty
    out <- newIORef empty
    proceed <- newEmptyMVar
    tid <- frk $ worker proceed inp out sf
    return (tid, proceed, inp, out)
  -- count should start at 0
  darr (tid, proceed, inp, out) = proc eb -> do
    _ <- terminalAIO (killThread tid) -< ()
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


-- | A variant of 'asyncE' that takes an 'IOAuto' and can thus perform 
--  'IO' actions.
asyncEIO :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
          (IO d, d -> IO ())    -- ^ Initialization and termination procedures
       -> (d -> IOAuto b c)     -- ^ The automaton to asynchronize
       -> a (SEvent b) (SEvent c)
asyncEIO = asyncEIOHelper forkIO

-- | A variant of 'asyncEIO' that uses 'forkOn' internally and thus takes 
--  a core ID to fork on.
asyncEIOOn :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
              Int                   -- ^ Core to fork on
           -> (IO d, d -> IO ())    -- ^ Initialization and termination procedures
           -> (d -> IOAuto b c)     -- ^ The automaton to asynchronize
           -> a (SEvent b) (SEvent c)
asyncEIOOn = asyncEIOHelper . forkOn

asyncEIOHelper :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
          (IO () -> IO ThreadId)    -- ^ fork
       -> (IO d, d -> IO ())        -- ^ Initialization and termination procedures
       -> (d -> IOAuto b c)         -- ^ The automaton to asynchronize
       -> a (SEvent b) (SEvent c)
asyncEIOHelper frk (ini, term) sf = initialAIO iod darr where
  iod = do
    inp <- newIORef empty
    out <- newIORef empty
    proceed <- newEmptyMVar
    tid <- frk $ bracket ini term $ worker proceed inp out . sf
    return (tid, proceed, inp, out)
  -- count should start at 0
  darr (tid, proceed, inp, out) = proc eb -> do
    _ <- terminalAIO (killThread tid) -< ()
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
        (c, sf') <- runKleisli sf $ b     -- do the action
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
asyncC :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
          PureAuto b c  -- ^ The automaton to convert to realtime
       -> a b [c]
asyncC = asyncCHelper forkIO

-- | A variant of 'asyncC' that uses 'forkOn' internally and thus takes 
--  a core ID to fork on.
asyncCOn :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
            Int           -- ^ Core to fork on
         -> PureAuto b c  -- ^ The automaton to convert to realtime
         -> a b [c]
asyncCOn = asyncCHelper . forkOn

asyncCHelper :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
            (IO () -> IO ThreadId)  -- ^ fork
         -> PureAuto b c            -- ^ The automaton to convert to realtime
         -> a b [c]
asyncCHelper frk sf = initialAIO iod darr where
  iod = do
    inp <- newIORef undefined
    start <- newEmptyMVar
    out <- newIORef empty
    tid <- frk $ takeMVar start >> worker inp out sf
    return (tid, inp, out, start)
  darr (tid, inp, out, start) = proc b -> do
    _ <- terminalAIO (killThread tid) -< ()
    _ <- liftAIO (writeIORef inp) -< b -- send the worker the new input
    c <- initialAIO (putMVar start ()) (const $ liftAIO (\_ -> atomicModifyIORef out (\s -> (empty,s)))) -< () --collect ready results
    returnA -< toList c
  -- worker processes the inner, "simulated" signal function.
  worker inp out (Automaton sf) = do
    b <- readIORef inp    -- get the latest input
    let (c, sf') = sf b   -- do the calculation
    deepseq c $ atomicModifyIORef out (\s -> (s |> c, ()))
    worker inp out sf'






-- | A variant of 'asyncC' that takes an 'IOAuto' and can thus perform 
--  'IO' actions.
asyncCIO :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
          (IO d, d -> IO ())    -- ^ Initialization and termination procedures
       -> (d -> IOAuto b c)     -- ^ The automaton to convert to realtime
       -> a b [c]
asyncCIO = asyncCIOHelper forkIO

-- | A variant of 'asyncCIO' that uses 'forkOn' internally and thus takes 
--  a core ID to fork on.
asyncCIOOn :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
              Int                   -- ^ Core to fork on
           -> (IO d, d -> IO ())    -- ^ Initialization and termination procedures
           -> (d -> IOAuto b c)     -- ^ The automaton to convert to realtime
           -> a b [c]
asyncCIOOn = asyncCIOHelper . forkOn

asyncCIOHelper :: (ArrowIO a, ArrowLoop a, ArrowCircuit a, ArrowChoice a, NFData c) => 
          (IO () -> IO ThreadId)    -- ^ fork
       -> (IO d, d -> IO ())        -- ^ Initialization and termination procedures
       -> (d -> IOAuto b c)         -- ^ The automaton to convert to realtime
       -> a b [c]
asyncCIOHelper frk (ini, term) sf = initialAIO iod darr where
  iod = do
    inp <- newIORef undefined
    start <- newEmptyMVar
    out <- newIORef empty
    tid <- frk $ takeMVar start >> bracket ini term (worker inp out . sf)
    return (tid, inp, out, start)
  darr (tid, inp, out, start) = proc b -> do
    _ <- terminalAIO (killThread tid) -< ()
    _ <- liftAIO (writeIORef inp) -< b -- send the worker the new input
    c <- initialAIO (putMVar start ()) (const $ liftAIO (\_ -> atomicModifyIORef out (\s -> (empty,s)))) -< () --collect ready results
    returnA -< toList c
  -- worker processes the inner, "simulated" signal function.
  worker inp out (Automaton sf) = do
    b <- readIORef inp    -- get the latest input
    (c, sf') <- runKleisli sf $ b   -- do the action
    deepseq c $ atomicModifyIORef out (\s -> (s |> c, ()))
    worker inp out sf'


