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

{-# LANGUAGE ScopedTypeVariables, Arrows, RecursiveDo, CPP, OverlappingInstances, FlexibleInstances, TypeSynonymInstances #-}

module FRP.UISF.UISF (
    UISF(..),
    -- * UISF Getters
    getTime, getCTX, getEvents, getFocusData, addThreadId, getMousePosition, 
    -- * UISF constructors, transformers, and converters
    -- $ctc
    mkUISF, 
    -- * UISF Lifting
    -- $lifting
    convertToUISF, asyncUISF, 
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
import Prelude hiding ((.), id)
#endif
import Control.Arrow
import Control.Arrow.Operations

import FRP.UISF.SOE
import FRP.UISF.UITypes

import FRP.UISF.AuxFunctions (Automaton, Time, toRealTimeArrow, 
                              SEvent, ArrowTime (..), ArrowIO (..),
                              async, AsyncInput (..), AsyncOutput (..))

import Control.Monad (when)
import qualified Graphics.UI.GLFW as GLFW (sleep, SpecialKey (..))
import Control.Concurrent
import Control.DeepSeq


------------------------------------------------------------
-- UISF Declaration and Instances
------------------------------------------------------------

data UISF b c = UISF 
  { uisfLayout :: Flow -> Layout,
    uisfFun    :: (CTX, Focus, Time, UIEvent, b) -> 
                  IO (DirtyBit, Focus, Graphic, ControlData, c, UISF b c) }

instance Category UISF where
  id = UISF (const nullLayout) fun where fun (_,foc,_,_,b) = return (False, foc, nullGraphic, nullCD, b, id)
  UISF gl g . UISF fl f = UISF layout fun where
    layout flow = mergeLayout flow (fl flow) (gl flow)
    fun (ctx, foc, t, e, b) = 
      let (fctx, gctx) = divideCTX ctx (fl $ flow ctx) (layout $ flow ctx)
          -- TODO: maybe divideCTX should take fl and gl; also, it should take the Flow -> Layout functions
      in do (fdb, foc',  fg, fcd, c, uisff') <- f (fctx, foc,  t, e, b)
            (gdb, foc'', gg, gcd, d, uisfg') <- g (gctx, foc', t, e, c)
            let graphic    = mergeGraphics ctx (fg, (fl $ flow ctx) ) (gg, (gl $ flow ctx) )
                cd         = mergeCD fcd gcd
                dirtybit   = ((||) $! fdb) $! gdb
            return (dirtybit, foc'', graphic, cd, d, uisfg' . uisff')

instance Arrow UISF where
  arr f = UISF (const nullLayout) fun where fun (_,foc,_,_,b) = return (False, foc, nullGraphic, nullCD, f b, arr f)
  first (UISF fl f) = UISF fl fun where
    fun (ctx, foc, t, e, (b, d)) = do
      (db, foc', g, cd, c, uisff') <- f (ctx, foc, t, e, b)
      return (db, foc', g, cd, (c,d), first uisff')
  -- TODO: custom defs for &&& and *** may improve performance, but they'll end up 
  -- looking like the ugly compose definition above.  Maybe I can find a way to 
  -- abstract the behavior out so that it's all in one place.

instance ArrowLoop UISF where
  loop (UISF fl f) = UISF fl fun where
    fun (ctx, foc, t, e, b) = do
      rec (db, foc', g, cd, (c,d), uisff') <- f (ctx, foc, t, e, (b,d))
      return (db, foc', g, cd, c, loop uisff')

instance ArrowChoice UISF where
  left uisf = left' True uisf where
    left' lastLeft ~(UISF fl f) = UISF fl fun where
      fun (ctx, foc, t, e, x) = case x of
            Left b  -> do (db, foc', g, cd, c, uisff') <- f (ctx, foc, t, e, b)
                          return (db || lastLeft, foc', g, cd, Left c, left' True uisff')
            Right d -> return (lastLeft, foc, nullGraphic, nullCD, Right d, left' False $ UISF (const nullLayout) f)
  uisff ||| uisfg = choice' True (uisfLayout uisff) uisff uisfg where
    choice' lastLeft layout uisff uisfg = UISF layout fun where
      fun (ctx, foc, t, e, x) = case x of
            Left b  -> do (db, foc', g, cd, d, uisff') <- uisfFun uisff (ctx, foc, t, e, b)
                          return (db || lastLeft, foc', g, cd, d, choice' True (uisfLayout uisff') uisff' uisfg)
            Right c -> do (db, foc', g, cd, d, uisfg') <- uisfFun uisfg (ctx, foc, t, e, c)
                          return (db || not lastLeft, foc', g, cd, d, choice' False (uisfLayout uisfg') uisff uisfg')


instance ArrowCircuit UISF where
    delay i = UISF (const nullLayout) (fun i) where 
      fun i (_,foc,_,_,b) = seq i $ return (False, foc, nullGraphic, nullCD, i, UISF (const nullLayout) (fun b))

instance ArrowIO UISF where
  liftAIO f = UISF (const nullLayout) fun where 
    fun (_,foc,_,_,b) = f b >>= (\c -> return (False, foc, nullGraphic, nullCD, c, liftAIO f))
  initialAIO iod f = UISF (const nullLayout) fun where
    fun inps = do
      d <- iod
      (db, foc', g, cd, c, uisff') <- uisfFun (f d) inps
      return (db, foc', g, cd, c, uisff')

instance ArrowTime UISF where
  time = getTime


------------------------------------------------------------
-- * UISF Getters and Convenience Constructor
------------------------------------------------------------

-- | Get the time signal from a UISF
getTime      :: UISF () Time
getTime      = mkUISF nullLayout (\(_,f,t,_,_) -> (False, f, nullGraphic, nullCD, t))

-- | Get the context signal from a UISF
getCTX       :: UISF () CTX
getCTX       = mkUISF nullLayout (\(c,f,_,_,_) -> (False, f, nullGraphic, nullCD, c))

-- | Get the UIEvent signal from a UISF
getEvents    :: UISF () UIEvent
getEvents    = mkUISF nullLayout (\(_,f,_,e,_) -> (False, f, nullGraphic, nullCD, e))

-- | Get the focus data from a UISF
getFocusData :: UISF () Focus
getFocusData = mkUISF nullLayout (\(_,f,_,_,_) -> (False, f, nullGraphic, nullCD, f))

-- | A thread handler for UISF.
addThreadId :: ThreadId -> UISF a a
addThreadId t = mkUISF nullLayout (\(_,f,_,_,b) -> (False, f, nullGraphic, [t], b))

-- | Get the mouse position from a UISF
getMousePosition :: UISF () Point
getMousePosition = proc _ -> do
  e <- getEvents -< ()
  rec p' <- delay (0,0) -< p
      let p = case e of
                  MouseMove pt -> pt
                  _            -> p'
  returnA -< p

-- | This function creates a UISF with the given parameters.
mkUISF :: Layout -> ((CTX, Focus, Time, UIEvent, a) -> (DirtyBit, Focus, Graphic, ControlData, b)) -> UISF a b
mkUISF l f = UISF (const l) fun where
  fun inps = let (db, foc, g, cd, b) = f inps in return (db, foc, g, cd, b, mkUISF l f)


------------------------------------------------------------
-- * UISF Lifting
------------------------------------------------------------
-- $lifting The following two functions are for lifting SFs to UISFs.  

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
convertToUISF :: NFData b => Double -> Double -> Automaton (->) a b -> UISF a [(b, Time)]
convertToUISF clockrate buffer sf = proc a -> do
  t <- time -< ()
  toRealTimeArrow clockrate buffer addThreadId sf -< (a, t)


-- | We can also lift a signal function to a UISF asynchronously.
asyncUISF :: NFData b => Automaton (->) a b -> UISF (AsyncInput a) (AsyncOutput b)
asyncUISF = async addThreadId


------------------------------------------------------------
-- * Layout Transformers
------------------------------------------------------------

-- $lt These functions are UISF transformers that modify the context.

topDown, bottomUp, leftRight, rightLeft, conjoin, unconjoin :: UISF a b -> UISF a b
topDown   = modifyFlow TopDown
bottomUp  = modifyFlow BottomUp
leftRight = modifyFlow LeftRight
rightLeft = modifyFlow RightLeft
conjoin   = modifyCTX (\ctx -> ctx {isConjoined = True})
unconjoin = modifyCTX (\ctx -> ctx {isConjoined = False})


modifyFlow :: Flow -> UISF a b -> UISF a b
modifyFlow newFlow (UISF l f) = UISF (const $ l newFlow) h where
  h (ctx, foc, t, e, b) = do
    (db, foc', g, cd, c, uisf) <- f (ctx {flow = newFlow}, foc, t, e, b)
    return (db, foc', g, cd, c, modifyFlow newFlow uisf)
  

modifyCTX  :: (CTX -> CTX) -> UISF a b -> UISF a b
modifyCTX mod (UISF l f) = UISF l h where
  h (ctx, foc, t, e, b) = do
    (db, foc', g, cd, c, uisf) <- f (mod ctx, foc, t, e, b)
    return (db, foc', g, cd, c, modifyCTX mod uisf)


-- | Set a new layout for this widget.
setLayout  :: Layout -> UISF a b -> UISF a b
setLayout l (UISF _ f) = UISF (const l) f

-- | A convenience function for setLayout, setSize sets the layout to a 
-- fixed size (in pixels).
setSize  :: Dimension -> UISF a b -> UISF a b
setSize (w,h) = setLayout $ makeLayout (Fixed w) (Fixed h)

-- | Add space padding around a widget.
pad  :: (Int, Int, Int, Int) -> UISF a b -> UISF a b
pad args@(w,n,e,s) (UISF fl f) = UISF layout h where
  layout ctx = let l = fl ctx in l { hFixed = hFixed l + w + e, vFixed = vFixed l + n + s }
  h (ctx, foc, t, e, b) = let ((x,y),(bw,bh)) = bounds ctx in do
    (db, foc', g, cd, c, uisf) <- f (ctx {bounds = ((x + w, y + n),(bw,bh))}, foc, t, e, b)
    return (db, foc', g, cd, c, pad args uisf)


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
runUI' :: String -> UISF () () -> IO ()
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
  let render :: Bool -> [UIEvent] -> Focus -> UISF () () -> [ThreadId] -> IO [ThreadId]
      render drawit' (inp:inps) lastFocus uisf tids = do
        wSize <- getMainWindowSize
        t <- timeGetTime
        let rt = t - t0
        let ctx = defaultCTX wSize
        (dirty, foc, graphic, tids', _, uisf') <- uisfFun uisf (ctx, lastFocus, rt, inp, ())
        -- delay graphical output when event queue is not empty
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
                    else render False inps foc' uisf' newtids
          _ -> render drawit inps foc' uisf' newtids
      render _ [] _ _ tids = return tids
  tids <- render True events defaultFocus sf []
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
-- GHCi (specifically when calling GLFW.closeWindow), so I've removed this.
--        SKey GLFW.ESC True -> closeWindow w >> return True
        Closed          -> return True
        _               -> addEv e >> getEvents

makeStream :: IO ([a], a -> IO ())
makeStream = do
  ch <- newChan
  contents <- getChanContents ch
  return (contents, writeChan ch)

