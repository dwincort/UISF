-----------------------------------------------------------------------------
-- |
-- Module      :  FRP.UISF.Types.MSF
-- Copyright   :  (c) Daniel Winograd-Cort 2014
-- License     :  see the LICENSE file in the distribution
--
-- Maintainer  :  dwc@cs.yale.edu
-- Stability   :  experimental
--
-- MSF is a monadic signal function.

{-# LANGUAGE CPP, RecursiveDo, FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}

module FRP.UISF.Types.MSF where

#if __GLASGOW_HASKELL__ >= 610
import Control.Category
import Prelude hiding ((.))
#endif
import Control.Arrow
import Control.Arrow.Operations
import Control.Monad.Fix

-- | The MSF data type describes a monadic signal function.  
-- Essentially, it is a Kleisli automaton, but we define it 
-- explicitly here.
data MSF m a b = MSF { msfFun :: (a -> m (b, MSF m a b)) }


#if __GLASGOW_HASKELL__ >= 610
instance Monad m => Category (MSF m) where
    id = MSF h where h x = return (x, MSF h)
    MSF g . MSF f = MSF (h f g)
      where h f g x    = do (y, MSF f') <- f x
                            (z, MSF g') <- g y
                            return (z, MSF (h f' g'))

instance Monad m => Arrow (MSF m) where
    arr f = MSF h 
      where h x = return (f x, MSF h)
    first (MSF f) = MSF (h f)
      where h f (x, z) = do (y, MSF f') <- f x
                            return ((y, z), MSF (h f'))
    f &&& g = MSF (h f g)
      where
        h f g x = do
          (y, f') <- msfFun f x
          (z, g') <- msfFun g x 
          return ((y, z), MSF (h f' g'))
    f *** g = MSF (h f g)
      where
        h f g x = do
          (y, f') <- msfFun f (fst x)
          (z, g') <- msfFun g (snd x) 
          return ((y, z), MSF (h f' g'))
#else
instance Monad m => Arrow (MSF m) where
    arr f = MSF h 
      where h x = return (f x, MSF h)
    MSF f >>> MSF g = MSF (h f g)
      where h f g x    = do (y, MSF f') <- f x
                            (z, MSF g') <- g y
                            return (z, MSF (h f' g'))
    first (MSF f) = MSF (h f)
      where h f (x, z) = do (y, MSF f') <- f x
                            return ((y, z), MSF (h f'))
    f &&& g = MSF (h f g)
      where
        h f g x = do
          (y, f') <- msfFun f x
          (z, g') <- msfFun g x 
          return ((y, z), MSF (h f' g'))
    f *** g = MSF (h f g)
      where
        h f g x = do
          (y, f') <- msfFun f (fst x)
          (z, g') <- msfFun g (snd x) 
          return ((y, z), MSF (h f' g'))
#endif

instance MonadFix m => ArrowLoop (MSF m) where
    loop (MSF f) = MSF (h f)
      where h f x = do rec ((y, z), MSF f') <- f (x, z)
                       return (y, MSF (h f'))

instance Monad m => ArrowChoice (MSF m) where
    left msf = MSF (h msf)
      where h msf x = case x of
                        Left x' -> do (y, msf') <- msfFun msf x'
                                      return (Left y, MSF (h msf'))
                        Right y -> return (Right y, MSF (h msf))
    f ||| g = MSF (h f g)
      where h f g x = case x of
                        Left  b -> do (d, f') <- msfFun f b
                                      return (d, MSF (h f' g))
                        Right c -> do (d, g') <- msfFun g c
                                      return (d, MSF (h f g'))


instance MonadFix m => ArrowCircuit (MSF m) where
    delay i = MSF (h i) where h i x = return (i, MSF (h x))

-- * MSF Constructors

-- $ The source, sink, and pipe functions allow one to lift a monadic 
-- action to the MSF data type.
source :: Monad m => m c ->         MSF m () c
sink   :: Monad m => (b -> m ()) -> MSF m b  ()
pipe   :: Monad m => (b -> m c) ->  MSF m b  c
source f = MSF h where h _ = f   >>= return . (\x -> (x, MSF h))
sink   f = MSF h where h x = f x >> return ((), MSF h)
pipe   f = MSF h where h x = f x >>= return . (\x -> (x, MSF h))

-- $ The sourceE, sinkE, and pipeE functions allow one to lift a monadic 
-- action to the MSF data type in event form.
sourceE :: Monad m => m c ->         MSF m (Maybe ()) (Maybe c)
sinkE   :: Monad m => (b -> m ()) -> MSF m (Maybe b)  (Maybe ())
pipeE   :: Monad m => (b -> m c) ->  MSF m (Maybe b)  (Maybe c)
sourceE f = MSF h where h = maybe (return (Nothing, MSF h)) (\_ -> f   >>= return . (\c -> (Just c, MSF h)))
sinkE   f = MSF h where h = maybe (return (Nothing, MSF h)) (\b -> f b >>  return (Just (), MSF h))
pipeE   f = MSF h where h = maybe (return (Nothing, MSF h)) (\b -> f b >>= return . (\c -> (Just c, MSF h)))

-- | This function first performs a monadic action and then uses the 
-- result of that action to complete the MSF.
initialAction :: Monad m => m x -> (x -> MSF m a b) -> MSF m a b
initialAction mx f = MSF $ \a -> do
    x <- mx
    msfFun (f x) a

-- | This function creates a MSF source based on an infinite list.
listSource :: Monad m => [c] -> MSF m () c
listSource cs = MSF (h cs) where h (c:cs) _ = return (c, MSF (h cs))

-- * Running MSF

-- | This steps through the given MSF using the [a] as inputs.  
-- The result is [b] in the monad.
stepMSF :: Monad m => MSF m a b -> [a] -> m [b]
stepMSF _ [] = return []
stepMSF (MSF f) (x:xs) = do 
    (y, f') <- f x
    ys <- stepMSF f' xs
    return (y:ys)

-- | This is the same as 'stepMSF' but additionally returns the 
-- next computation.
stepMSF' :: Monad m => MSF m a b -> [a] -> m ([b], MSF m a b)
stepMSF' g [] = return ([], g)
stepMSF' (MSF f) (x:xs) = do 
    (y, f') <- f x
    (ys, g) <- stepMSF' f' xs
    return (y:ys, g)

-- | The stream data type is used to \"stream\" the results of 
-- running an MSF.
data Stream m b = Stream { stream :: m (b, Stream m b) }
-- | Given an input list of values, this produces a stream of 
-- results that can be unwound as necessary.
streamMSF :: Monad m => MSF m a b -> [a] -> Stream m b
streamMSF (MSF f) (x:xs) = Stream $ do 
    (y, f') <- f x
    return (y, streamMSF f' xs)

-- | This function runs the MSF on a single value.
runMSF :: Monad m => a -> MSF m a b -> m b
runMSF a f = run f where run (MSF f) = do f a >>= run . snd

-- | This function runs an MSF that takes unit input for a single value.
runMSF' :: Monad m => MSF m () b -> m b
runMSF' = runMSF ()
