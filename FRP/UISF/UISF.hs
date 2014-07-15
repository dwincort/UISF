-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.UISF
-- Copyright   :  (c) Daniel Winograd-Cort 2014
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental
--
-- A simple Graphical User Interface with concepts borrowed from Phooey
-- by Conal Elliot.

{-# LANGUAGE ScopedTypeVariables, Arrows, DoRec, CPP, OverlappingInstances, FlexibleInstances, TypeSynonymInstances #-}

module FRP.UISF.UISF (
    UISF,
    -- * UISF Getters
    getTime, getCTX, getEvents, getFocusData, getMousePosition, 
    -- * UISF constructors, transformers, and converters
    -- $ctc
    mkUISF, mkUISF', expandUISF, compressUISF, transformUISF, 
    initialIOAction, 
    uisfSourceE, uisfSinkE, uisfPipeE, 
    -- * UISF Lifting
    -- $lifting
    toUISF, convertToUISF, asyncUISF, 
    -- * Layout Transformers
    -- $lt
    leftRight, rightLeft, topDown, bottomUp, 
    conjoin, unconjoin, 
    setLayout, setSize, pad, 
    -- * Execute UI Program
    runUI, runUI'

) where

#if __GLASGOW_HASKELL__ >= 610
import Control.Category
import Prelude hiding ((.))
#endif
import Control.Arrow
import Control.Arrow.Operations

import FRP.UISF.SOE
import FRP.UISF.UIMonad

import FRP.UISF.Types.MSF
import FRP.UISF.AuxFunctions (Automaton, Time, toMSF, toRealTimeMSF, 
                              SEvent, ArrowTime (..),
                              async, AsyncInput (..), AsyncOutput (..))

import Control.Monad (when)
import qualified Graphics.UI.GLFW as GLFW (sleep, SpecialKey (..))
import Control.Concurrent.MonadIO
import Control.DeepSeq


-- | The main UI signal function, built from the UI monad and MSF.
type UISF = MSF UI

instance ArrowCircuit UISF where
  delay i = MSF (h i) where h i x = seq i $ return (i, MSF (h x))
        -- We probably want this to be a deepseq, but changing the types is a pain.

instance ArrowTime UISF where
  time = getTime


------------------------------------------------------------
-- * UISF Getters
------------------------------------------------------------

-- | Get the time signal from a UISF
getTime      :: UISF () Time
getTime      = mkUISF (\_ (_,f,t,_) -> (nullLayout, False, f, nullAction, nullCD, t))

-- | Get the context signal from a UISF
getCTX       :: UISF () CTX
getCTX       = mkUISF (\_ (c,f,_,_) -> (nullLayout, False, f, nullAction, nullCD, c))

-- | Get the UIEvent signal from a UISF
getEvents    :: UISF () UIEvent
getEvents    = mkUISF (\_ (_,f,_,e) -> (nullLayout, False, f, nullAction, nullCD, e))

-- | Get the focus data from a UISF
getFocusData :: UISF () Focus
getFocusData = mkUISF (\_ (_,f,_,_) -> (nullLayout, False, f, nullAction, nullCD, f))

-- | Get the mouse position from a UISF
getMousePosition :: UISF () Point
getMousePosition = proc _ -> do
  e <- getEvents -< ()
  rec p' <- delay (0,0) -< p
      let p = case e of
                  MouseMove pt -> pt
                  _            -> p'
  returnA -< p


------------------------------------------------------------
-- * UISF constructors, transformers, and converters
------------------------------------------------------------

-- $ctc These fuctions are various shortcuts for creating UISFs.
-- The types pretty much say it all for how they work.

mkUISF :: (a -> (CTX, Focus, Time, UIEvent) -> (Layout, DirtyBit, Focus, Action, ControlData, b)) -> UISF a b
mkUISF f = pipe (\a -> UI (return . f a))

mkUISF' :: (a -> (CTX, Focus, Time, UIEvent) -> IO (Layout, DirtyBit, Focus, Action, ControlData, b)) -> UISF a b
mkUISF' = pipe . (UI .)

expandUISF :: UISF a b -> a -> (CTX, Focus, Time, UIEvent) -> IO (Layout, DirtyBit, Focus, Action, ControlData, (b, UISF a b))
{-# INLINE expandUISF #-}
expandUISF (MSF f) = unUI . f

compressUISF :: (a -> (CTX, Focus, Time, UIEvent) -> IO (Layout, DirtyBit, Focus, Action, ControlData, (b, UISF a b))) -> UISF a b
{-# INLINE compressUISF #-}
compressUISF f = MSF (UI . f)

transformUISF :: (UI (c, UISF b c) -> UI (c, UISF b c)) -> UISF b c -> UISF b c
transformUISF f (MSF sf) = MSF $ \a -> do
  (c, nextSF) <- f (sf a)
  return (c, transformUISF f nextSF)

-- | Apply the given IO action when this UISF is first run and use its 
--   result to produce the UISF to run
initialIOAction :: IO x -> (x -> UISF a b) -> UISF a b
initialIOAction = initialAction . liftIO

-- source, sink, and pipe functions
-- DWC Note: I don't feel comfortable with how generic these are.
-- Also, the continuous ones can't work.
-- 
-- uisfSource :: IO c ->         UISF () c
-- uisfSink   :: (b -> IO ()) -> UISF b  ()
-- uisfPipe   :: (b -> IO c) ->  UISF b  c
-- uisfSource = source . liftIO
-- uisfSink   = sink . (liftIO .)
-- uisfPipe   = pipe . (liftIO .)

-- | Generate a source UISF from the IO action.
uisfSourceE :: IO c ->         UISF (SEvent ()) (SEvent c)
uisfSourceE = (delay Nothing >>>) . sourceE . liftIO

-- | Generate a sink UISF from the IO action.
uisfSinkE   :: (b -> IO ()) -> UISF (SEvent b)  (SEvent ())
uisfSinkE   = (delay Nothing >>>) . sinkE . (liftIO .)

-- | Generate a pipe UISF from the IO action.
uisfPipeE   :: (b -> IO c) ->  UISF (SEvent b)  (SEvent c)
uisfPipeE   = (delay Nothing >>>) . pipeE . (liftIO .)



------------------------------------------------------------
-- * UISF Lifting
------------------------------------------------------------

-- $lifting The following two functions are for lifting SFs to UISFs.  

-- | This is a quick and dirty solution that ignores timing issues.
toUISF :: Automaton a b -> UISF a b
toUISF = toMSF

-- | This is the standard one that appropriately keeps track of 
--   simulated time vs real time.  
--
-- The clockrate is the simulated rate of the input signal function.
-- The buffer is the number of time steps the given signal function is allowed 
-- to get ahead of real time.  The real amount of time that it can get ahead is
-- the buffer divided by the clockrate seconds.
-- The output signal function takes and returns values in real time.  The return 
-- values are the list of bs generated in the given time step, each time stamped.
-- 
-- Note that the returned list may be long if the clockrate is much 
-- faster than real time and potentially empty if it's slower.
-- Note also that the caller can check the time stamp on the element 
-- at the end of the list to see if the inner, "simulated" signal 
-- function is performing as fast as it should.
convertToUISF :: NFData b => Double -> Double -> Automaton a b -> UISF a [(b, Time)]
convertToUISF clockrate buffer sf = proc a -> do
  t <- time -< ()
  toRealTimeMSF clockrate buffer addThreadID sf -< (a, t)


-- | We can also lift a signal function to a UISF asynchronously.
asyncUISF :: NFData b => Automaton a b -> UISF (AsyncInput a) (AsyncOutput b)
asyncUISF = async addThreadID


------------------------------------------------------------
-- * Layout Transformers
------------------------------------------------------------

-- $lt These functions are UISF transformers that modify the context.

topDown, bottomUp, leftRight, rightLeft, conjoin, unconjoin :: UISF a b -> UISF a b
topDown   = modifyFlow (\ctx -> ctx {flow = TopDown})
bottomUp  = modifyFlow (\ctx -> ctx {flow = BottomUp})
leftRight = modifyFlow (\ctx -> ctx {flow = LeftRight})
rightLeft = modifyFlow (\ctx -> ctx {flow = RightLeft})
conjoin   = modifyFlow (\ctx -> ctx {isConjoined = True})
unconjoin = modifyFlow (\ctx -> ctx {isConjoined = False})


modifyFlow  :: (CTX -> CTX) -> UISF a b -> UISF a b
modifyFlow h = transformUISF (modifyFlow' h)
  where modifyFlow' :: (CTX -> CTX) -> UI a -> UI a
        modifyFlow' h (UI f) = UI g where g (c,s,t,i) = f (h c,s,t,i)


-- | Set a new layout for this widget.
setLayout  :: Layout -> UISF a b -> UISF a b
setLayout l = transformUISF (setLayout' l)
  where setLayout' :: Layout -> UI a -> UI a
        setLayout' d (UI f) = UI aux
          where
            aux inps = do
              (_, db, foc, a, ts, v) <- f inps
              return (d, db, foc, a, ts, v)

-- | A convenience function for setLayout, setSize sets the layout to a 
-- fixed size (in pixels).
setSize  :: Dimension -> UISF a b -> UISF a b
setSize (w,h) = setLayout $ makeLayout (Fixed w) (Fixed h)

-- | Add space padding around a widget.
pad  :: (Int, Int, Int, Int) -> UISF a b -> UISF a b
pad args = transformUISF (pad' args)
  where pad' :: (Int, Int, Int, Int) -> UI a -> UI a
        pad' (w,n,e,s) (UI f) = UI aux
          where
            aux (ctx@(CTX i _ c), foc, t, inp) = do
              rec (l, db, foc', a, ts, v) <- f (CTX i ((x + w, y + n),(bw,bh)) c, foc, t, inp)
                  let d = l { hFixed = hFixed l + w + e, vFixed = vFixed l + n + s }
                      ((x,y),(bw,bh)) = bounds ctx
              return (d, db, foc', a, ts, v)


------------------------------------------------------------
-- * Execute UI Program
------------------------------------------------------------

defaultSize :: Dimension
defaultSize = (300, 300)
defaultCTX :: Dimension -> CTX
defaultCTX size = CTX TopDown ((0,0), size) False
defaultFocus :: Focus
defaultFocus = (0, SetFocusTo 0)
resetFocus :: (WidgetID, FocusInfo) -> (WidgetID, FocusInfo)
resetFocus (n,SetFocusTo i) = (0, SetFocusTo $ (i+n) `rem` n)
resetFocus (_,_) = (0,NoFocus)

-- | Run the UISF with the default size (300 x 300).
runUI' ::              String -> UISF () () -> IO ()
runUI' = runUI defaultSize

-- | Run the UISF
runUI  :: Dimension -> String -> UISF () () -> IO ()
runUI windowSize title sf = runGraphics $ do
  w <- openWindowEx title (Just (0,0)) (Just windowSize) drawBufferedGraphic
  (events, addEv) <- makeStream
  let pollEvents = windowUser w addEv
  -- poll events before we start to make sure event queue isn't empty
  t0 <- timeGetTime
  pollEvents
  let render :: Bool -> [UIEvent] -> Focus -> Stream UI () -> [ThreadId] -> IO [ThreadId]
      render drawit' (inp:inps) lastFocus uistream tids = do
        wSize <- getMainWindowSize
        t <- timeGetTime
        let rt = t - t0
        let ctx = defaultCTX wSize
        (_, dirty, foc, (graphic, sound), tids', (_, uistream')) <- (unUI $ stream uistream) (ctx, lastFocus, rt, inp)
        -- always output sound
        sound
        -- and delay graphical output when event queue is not empty
        setGraphic' w graphic
        let drawit = dirty || drawit'
            newtids = tids'++tids
            foc' = resetFocus foc
        foc' `seq` newtids `seq` case inp of
          -- Timer only comes in when we are done processing user events
          NoUIEvent -> do 
            -- output graphics 
            when drawit $ setDirty w
            quit <- pollEvents
            if quit then return newtids
                    else render False inps foc' uistream' newtids
          _ -> render drawit inps foc' uistream' newtids
      render _ [] _ _ tids = return tids
  tids <- render True events defaultFocus (streamMSF sf (repeat ())) []
  -- wait a little while before all Midi messages are flushed
  GLFW.sleep 0.5
  mapM_ killThread tids

windowUser :: Window -> (UIEvent -> IO ()) -> IO Bool
windowUser w addEv = do 
  quit <- getEvents
  addEv NoUIEvent
  return quit
 where 
  getEvents :: IO Bool
  getEvents = do
    mev <- maybeGetWindowEvent 0.001 w
    case mev of
      Nothing -> return False
      Just e  -> case e of
-- There's a bug somewhere with GLFW that makes pressing ESC freeze up 
-- GHCi, so I've removed this.
--        SKey GLFW.ESC True -> closeWindow w >> return True
--        Key '\00'  True -> return True
        Closed          -> return True
        _               -> addEv e >> getEvents

makeStream :: IO ([a], a -> IO ())
makeStream = do
  ch <- newChan
  contents <- getChanContents ch
  return (contents, writeChan ch)

