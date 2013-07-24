{-# LANGUAGE MultiParamTypeClasses #-}
module Reactive.Banana.Extra (
  -- * Combinators
    zipE
  , joinE  
  , scanE
  , scanE2
    -- * Behavior
  , Bhv(..)
  , mixEvents
    -- * Event handlers
  , actimateWith
  , actimateWith_
  , actimateBhvWith
  ) where

import Control.Applicative
import Reactive.Banana
import Reactive.Banana.Frameworks



----------------------------------------------------------------
-- Various scans
----------------------------------------------------------------

-- | Zip behavior and event
zipE :: (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
zipE f b e = fmap f b <@> e

-- | Join two different events using sum type
joinE :: Event t a -> Event t b -> Event t (Either a b)
joinE ea eb = (Left <$> ea) `union` (Right <$> eb)

-- | 'scanl' like accumulation function
scanE :: (a -> b -> a) -> a -> Event t b -> Event t a
scanE fold x0 = accumE x0 . fmap (flip fold)

-- | Scan function which can draw events from two separate sources
scanE2 :: (a -> b -> a)
       -> (a -> c -> a)
       ->  a
       -> Event t b
       -> Event t c
       -> Event t a
scanE2 fb fc a0 eb ec = scanE go a0 $ joinE ec eb
  where
    go a (Right b) = fb a b
    go a (Left  c) = fc a c



----------------------------------------------------------------
-- Behavior
----------------------------------------------------------------

-- | Alternative implementation of behavior. As long as behavior is
--   discrete it could be modelled as initial value and stream of
--   updates to that value.
data Bhv t a = Bhv { initialValue :: a
                   , toEvent      :: Event t a
                   }

mixEvents :: Event t a -> Bhv t a -> Bhv t a
mixEvents e' (Bhv x0 e) = Bhv x0 (union e e')

instance Functor (Bhv t) where
  fmap f (Bhv x e) = Bhv (f x) (fmap f e)

instance Applicative (Bhv t) where
  pure x = Bhv x never
  (<*>) = zipBehavior ($)

instance Apply (Bhv t) (Event t) where
  bhv <@> e = filterJust
            $ toEvent
            $ zipBehavior fmap bhv (Bhv Nothing $ fmap Just e)


zipBehavior :: (a -> b -> c) -> Bhv t a -> Bhv t b -> Bhv t c
zipBehavior f (Bhv a0 evtA) (Bhv b0 evtB)
  = Bhv (f a0 b0) evts
  where
    evts = fmap (uncurry f)
         $ scanE go (a0,b0) (joinE evtA evtB)
    go (_,b) (Left  a) = (a,b)
    go (a,_) (Right b) = (a,b)



----------------------------------------------------------------
-- Actimate variants
----------------------------------------------------------------

actimateWith :: Frameworks t => (a -> IO ()) -> Event t a -> Moment t ()
actimateWith f = reactimate . fmap f

actimateWith_ :: Frameworks t => IO () -> Event t a -> Moment t ()
actimateWith_ f = reactimate . fmap (const f)

actimateBhvWith :: Frameworks t => (a -> IO ()) -> Behavior t a -> Moment t ()
actimateBhvWith f bhv
  = actimateWith f =<< changes bhv
