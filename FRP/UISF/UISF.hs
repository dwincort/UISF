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

{-# LANGUAGE Arrows, RecursiveDo, CPP, OverlappingInstances, FlexibleInstances, TypeSynonymInstances #-}

module FRP.UISF.UISF (
    UISF(..),
    uisfSource, uisfSink, uisfPipe,
    uisfSourceE, uisfSinkE, uisfPipeE,
    -- * UISF Getters
    getTime, getCTX, withCTX, getEvents, getFocusData, addTerminationProc, getMousePosition, 
    -- * UISF constructors, transformers, and converters
    mkUISF, 
    -- * UISF Lifting
    -- $lifting
    asyncUISFE, asyncUISFEOn, asyncUISFV, --asyncUISFC, 
    -- * Layout Transformers
    -- $lt
    leftRight, rightLeft, topDown, bottomUp, 
    conjoin, unconjoin, 
    setLayout, setSize, pad, 
    -- * Execute UI Program
    UIParams (..), defaultUIParams,
    runUI, runUI'

) where

#if __GLASGOW_HASKELL__ >= 610
import Control.Category
import Prelude hiding ((.), id, mapM_)
#else
import Prelude hiding (mapM_)
#endif
import Control.Arrow
import Control.Arrow.Operations

import FRP.UISF.Graphics
import FRP.UISF.Keys
import FRP.UISF.Glut
import FRP.UISF.UITypes

import FRP.UISF.AuxFunctions (Automaton, Time, evMap, 
                              SEvent, ArrowTime (..), ArrowIO (..),
                              asyncE, asyncEOn, asyncV)

import Control.Monad (when, unless)
import Data.Foldable (mapM_)
import Control.Concurrent
import Control.DeepSeq
import Data.IORef
import Control.Exception


------------------------------------------------------------
-- UISF Declaration and Instances
------------------------------------------------------------

data UISF b c = UISF 
  { uisfLayout :: Flow -> Layout,
    uisfFun    :: (CTX, Focus, Time, UIEvent, b) -> 
                  IO (DirtyBit, Focus, Graphic, TerminationProc, c, UISF b c) }

instance Category UISF where
  id = UISF (const nullLayout) fun where fun (_,foc,_,_,b) = return (False, foc, nullGraphic, nullTP, b, id)
  UISF gl g . UISF fl f = UISF layout fun where
    layout flow = mergeLayout flow (fl flow) (gl flow)
    fun (ctx, foc, t, e, b) = 
      let (fctx, gctx) = divideCTX ctx (fl $ flow ctx) (gl $ flow ctx)
      in do (fdb, foc',  fg, ftp, c, uisff') <- f (fctx, foc,  t, e, b)
            (gdb, foc'', gg, gtp, d, uisfg') <- g (gctx, foc', t, e, c)
            let graphic    = mergeGraphics ctx (fg, (fl $ flow ctx) ) (gg, (gl $ flow ctx) )
                tp         = mergeTP ftp gtp
                dirtybit   = ((||) $! fdb) $! gdb
            return (dirtybit, foc'', graphic, tp, d, uisfg' . uisff')

instance Arrow UISF where
  arr f = UISF (const nullLayout) fun where fun (_,foc,_,_,b) = return (False, foc, nullGraphic, nullTP, f b, arr f)
  first (UISF fl f) = UISF fl fun where
    fun (ctx, foc, t, e, (b, d)) = do
      (db, foc', g, tp, c, uisff') <- f (ctx, foc, t, e, b)
      return (db, foc', g, tp, (c,d), first uisff')
  -- TODO: custom defs for &&& and *** may improve performance, but they'll end up 
  -- looking like the ugly compose definition above.  Maybe I can find a way to 
  -- abstract the behavior out so that it's all in one place.

instance ArrowLoop UISF where
  loop (UISF fl f) = UISF fl fun where
    fun (ctx, foc, t, e, b) = do
      rec (db, foc', g, tp, (c,d), uisff') <- f (ctx, foc, t, e, (b,d))
      return (db, foc', g, tp, c, loop uisff')

instance ArrowChoice UISF where
  left uisf = left' True uisf where
    left' lastLeft ~(UISF fl f) = UISF fl fun where
      fun (ctx, foc, t, e, x) = case x of
            Left b  -> do (db, foc', g, tp, c, uisff') <- f (ctx, foc, t, e, b)
                          return (db || lastLeft, foc', g, tp, Left c, left' True uisff')
            Right d -> return (lastLeft, foc, nullGraphic, nullTP, Right d, left' False $ UISF (const nullLayout) f)
  uisff ||| uisfg = choice' True (uisfLayout uisff) uisff uisfg where
    choice' lastLeft layout uisff uisfg = UISF layout fun where
      fun (ctx, foc, t, e, x) = case x of
            Left b  -> do (db, foc', g, tp, d, uisff') <- uisfFun uisff (ctx, foc, t, e, b)
                          return (db || lastLeft, foc', g, tp, d, choice' True (uisfLayout uisff') uisff' uisfg)
            Right c -> do (db, foc', g, tp, d, uisfg') <- uisfFun uisfg (ctx, foc, t, e, c)
                          return (db || not lastLeft, foc', g, tp, d, choice' False (uisfLayout uisfg') uisff uisfg')


instance ArrowCircuit UISF where
    delay i = UISF (const nullLayout) (fun i) where 
      fun i (_,foc,_,_,b) = seq i $ return (False, foc, nullGraphic, nullTP, i, UISF (const nullLayout) (fun b))

instance ArrowIO UISF where
  liftAIO f = UISF (const nullLayout) fun where 
    fun (_,foc,_,_,b) = f b >>= (\c -> return (False, foc, nullGraphic, nullTP, c, liftAIO f))
  initialAIO iod f = UISF (const nullLayout) fun where
    fun inps = do
      d <- iod
      (db, foc', g, tp, c, uisff') <- uisfFun (f d) inps
      return (db, foc', g, tp, c, setDirty uisff')
    setDirty (UISF l f) = UISF l h where
      h inp = do
        (_, foc', g, tp, c, uisf) <- f inp
        return (True, foc', g, tp, c, uisf)

instance ArrowTime UISF where
  time = getTime


------------------------------------------------------------
-- * UISF IO Lifters
------------------------------------------------------------

-- | Lift an IO source to UISF.
uisfSource :: IO b -> UISF () b
uisfSource = liftAIO . const

-- | Lift an IO sink to UISF.
uisfSink :: (a -> IO ()) -> UISF a ()
uisfSink = liftAIO

-- | Lift an IO pipe to UISF.
uisfPipe :: (a -> IO b) -> UISF a b
uisfPipe = liftAIO

-- | Lift an IO source to an event-based UISF.
uisfSourceE :: IO b -> UISF (SEvent ()) (SEvent b)
uisfSourceE = evMap . uisfSource

-- | Lift an IO sink to an event-based UISF.
uisfSinkE :: (a -> IO ()) -> UISF (SEvent a) (SEvent ())
uisfSinkE = evMap . uisfSink

-- | Lift an IO pipe to an event-based UISF.
uisfPipeE :: (a -> IO b) -> UISF (SEvent a) (SEvent b)
uisfPipeE = evMap . uisfPipe


------------------------------------------------------------
-- * UISF Getters and Convenience Constructor
------------------------------------------------------------

-- | Get the time signal from a UISF.
getTime      :: UISF () Time
getTime      = mkUISF nullLayout (\(_,f,t,_,_) -> (False, f, nullGraphic, nullTP, t))

{-# DEPRECATED getCTX "Use withCTX instead" #-}
-- | Get the context signal from a UISF.
--   This has been deprecated in favor of withCTX as it can provide 
--   misleading information.
getCTX       :: UISF () CTX
getCTX       = mkUISF nullLayout (\(c,f,_,_,_) -> (False, f, nullGraphic, nullTP, c))

-- | Provide the context signal to the UISF.
withCTX :: UISF (CTX,a) b -> UISF a b
withCTX (UISF l f) = UISF l h where
  h (ctx, foc, t, e, b) = do
    (db, foc', g, tp, c, uisf) <- f (ctx, foc, t, e, (ctx,b))
    return (db, foc', g, tp, c, withCTX uisf)

-- | Get the UIEvent signal from a UISF.
getEvents    :: UISF () UIEvent
getEvents    = mkUISF nullLayout (\(_,f,_,e,_) -> (False, f, nullGraphic, nullTP, e))

-- | Get the focus data from a UISF.
getFocusData :: UISF () Focus
getFocusData = mkUISF nullLayout (\(_,f,_,_,_) -> (False, f, nullGraphic, nullTP, f))

-- | Add a termination procedure to a UISF.
addTerminationProc :: IO () -> UISF a a
addTerminationProc p = UISF (const nullLayout) fun where
  fun  (_,f,_,_,b) = return (False, f, nullGraphic, Just p,  b, UISF (const nullLayout) fun2)
  fun2 (_,f,_,_,b) = return (False, f, nullGraphic, Nothing, b, UISF (const nullLayout) fun2)

-- | Get the mouse position from a UISF.
getMousePosition :: UISF () Point
getMousePosition = proc _ -> do
  e <- getEvents -< ()
  rec p' <- delay (0,0) -< p
      let p = case e of
                  MouseMove pt -> pt
                  _            -> p'
  returnA -< p

-- | This function creates a UISF with the given parameters.
mkUISF :: Layout -> ((CTX, Focus, Time, UIEvent, a) -> (DirtyBit, Focus, Graphic, TerminationProc, b)) -> UISF a b
mkUISF l f = UISF (const l) fun where
  fun inps = let (db, foc, g, tp, b) = f inps in return (db, foc, g, tp, b, mkUISF l f)


------------------------------------------------------------
-- * UISF Lifting
------------------------------------------------------------
-- $lifting The following two functions are for lifting Automatons to UISFs.  

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
asyncUISFV :: NFData b => Double -> Double -> Automaton (->) a b -> UISF a [(b, Time)]
asyncUISFV clockrate buffer sf = proc a -> do
  t <- time -< ()
  asyncV clockrate buffer (addTerminationProc . killThread) sf -< (a, t)


-- | We can also lift a signal function to a UISF asynchronously.
asyncUISFE :: NFData b => Automaton (->) a b -> UISF (SEvent a) (SEvent b)
asyncUISFE = asyncE (addTerminationProc . killThread)

asyncUISFEOn :: NFData b => Int -> Automaton (->) a b -> UISF (SEvent a) (SEvent b)
asyncUISFEOn n = asyncEOn n (addTerminationProc . killThread)


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
    (db, foc', g, tp, c, uisf) <- f (ctx {flow = newFlow}, foc, t, e, b)
    return (db, foc', g, tp, c, modifyFlow newFlow uisf)
  

modifyCTX  :: (CTX -> CTX) -> UISF a b -> UISF a b
modifyCTX mod (UISF l f) = UISF l h where
  h (ctx, foc, t, e, b) = do
    (db, foc', g, tp, c, uisf) <- f (mod ctx, foc, t, e, b)
    return (db, foc', g, tp, c, modifyCTX mod uisf)


-- | Set a new layout for this widget.
setLayout  :: Layout -> UISF a b -> UISF a b
setLayout l (UISF _ f) = UISF (const l) h where
  h (ctx, foc, t, e, b) = do
    (db, foc', g, tp, c, uisf) <- f (ctx, foc, t, e, b)
    return (db, foc', g, tp, c, setLayout l uisf)

-- | A convenience function for setLayout, setSize sets the layout to a 
-- fixed size (in pixels).
setSize  :: Dimension -> UISF a b -> UISF a b
setSize (w,h) = setLayout $ makeLayout (Fixed w) (Fixed h)

-- | Add space padding around a widget.
pad  :: (Int, Int, Int, Int) -> UISF a b -> UISF a b
pad args@(w,n,e,s) (UISF fl f) = UISF layout h where
  layout ctx = let l = fl ctx in l { wFixed = wFixed l + w + e, hFixed = hFixed l + n + s }
  h (ctx, foc, t, evt, b) = let ((x,y),(bw,bh)) = bounds ctx in do
    (db, foc', g, tp, c, uisf) <- f (ctx {bounds = ((x + w, y + n),(bw-w-e,bh-n-s))}, foc, t, evt, b)
    return (db, foc', g, tp, c, pad args uisf)


------------------------------------------------------------
-- * Execute UI Program
------------------------------------------------------------

-- | The UIParams data type provides an interface for modifying some 
--   of the settings for runUI without forcing runUI to take a zillion 
--   arguments.  Typical usage will be to modify the below defaultUIParams 
--   using record syntax.
data UIParams = UIParams {
    uiInitialize :: IO ()   -- ^ An initialization action.
  , uiClose :: IO ()        -- ^ A termination action.
  , uiTitle :: String       -- ^ The UI window's title.
  , uiSize :: Dimension     -- ^ The size of the UI window.
  , uiInitFlow :: Flow      -- ^ The initial Flow setting.
  , uiTickDelay :: Double   -- ^ How long the UI will sleep between clock 
                            --   ticks if no events are detected.  This 
                            --   should be probably be set to O(milliseconds), 
                            --   but it can be set to 0 for better performance 
                            --   (but also higher CPU usage)
  , uiCloseOnEsc :: Bool    -- ^ Should the UI window close when the user 
                            --   presses the escape key?
  , uiBackground :: RGB     -- ^ The default color of the UI window background.
}

-- | This is the default UIParams value and what is used in runUI'.
defaultUIParams :: UIParams
defaultUIParams = UIParams {
    uiInitialize = return (),
    uiClose = return (),
    uiTitle = "User Interface",
    uiSize = (300, 300),
    uiInitFlow = TopDown,
    uiTickDelay = 0.001,
    uiCloseOnEsc = False,
    uiBackground = colorToRGB LightBeige
}

defaultCTX :: Flow -> Dimension -> CTX
defaultCTX flow size = CTX flow ((0,0), size) False
defaultFocus :: Focus
defaultFocus = (0, SetFocusTo 0)
resetFocus :: (WidgetID, FocusInfo) -> (WidgetID, FocusInfo)
resetFocus (n,SetFocusTo i) = (0, SetFocusTo $ (i+n) `rem` n)
resetFocus (_,_) = (0,NoFocus)

-- | Run the UISF with the default settings.
runUI' :: UISF () () -> IO ()
runUI' = runUI defaultUIParams

-- | Run the UISF with the given parameters.
runUI  :: UIParams -> UISF () () -> IO ()
runUI p sf = do
    tref <- newIORef Nothing
    uiInitialize p
    w <- openWindow (uiBackground p) (uiTitle p) (uiSize p)
    finally (go tref w defaultFocus sf) (terminate tref w)
  where
    terminate tref w = do
      setGraphics w (nullGraphic, False)
      mwindow <- getWindow w
      mapM_ closeWindow mwindow
      tproc <- readIORef tref
      --sequence_ tproc
      case tproc of
        Nothing -> return ()
        Just t -> t
      uiClose p
    go tref w lastFocus uisf = do
      mwindow <- getWindow w
      -- If the window is not there, GL has closed it.  Time to stop.
      case mwindow of
        Nothing -> return ()
        Just _ -> do
          ev <- getNextEvent' w
          -- If the event is the Escape key, that may be a signal to stop.
          let die = case ev of
                (SKey KeyEsc _ True) -> True
                _ -> False
          unless (uiCloseOnEsc p && die) $ do
            -- If there's no event (NoUIEvent), then sleep for tickdelay before processing.
            when (ev == NoUIEvent) (threadDelay $ truncate $ uiTickDelay p * 1000000)
            -- For any other event, immediately process it.
            wSize <- getWindowDim w
            t <- getElapsedGUITime w
            let ctx = defaultCTX (uiInitFlow p) wSize
            (dirty, foc, graphic, tproc', _, uisf') <- uisfFun uisf (ctx, lastFocus, t, ev, ())
            let foc' = resetFocus foc
                -- When we're in the middle of setting focus, don't set 
                -- the graphic yet.  Wait until it's done, and then set it.
                dirty' = case (snd lastFocus, snd foc') of
                    (_, SetFocusTo _) -> False
                    (SetFocusTo _, NoFocus) -> True
                    _ -> dirty
            case dirty' of 
              -- Is this deepseq even helping?
              True -> deepseq graphic $ setGraphics w (graphic, True)
              False -> setGraphics w (graphic, False)
            atomicModifyIORef' tref (\tproc -> (mergeTP tproc' tproc, ()))
            go tref w foc' uisf'
    -- this getNextEvent' function is implementing a possible performance boost.
    -- TODO: Does this actually help at all?
    getNextEvent' w = do
      e <- getNextEvent w
      case e of
        MouseMove _ -> do
          e' <- peekNextEvent w
          case e' of
            MouseMove _ -> getNextEvent' w
            _ -> return e
        _ -> return e
