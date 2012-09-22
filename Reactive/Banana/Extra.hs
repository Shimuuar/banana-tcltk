{-# LANGUAGE MultiParamTypeClasses #-}
module Reactive.Banana.Extra where

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
-- EB
----------------------------------------------------------------

-- | Event and its corresponding behaviour.
data EB t a = EB (Event t a) (Behavior t a)

instance Functor (EB t) where
  fmap f (EB e b) = EB (fmap f e) (fmap f b)

instance Apply (EB t) (EB t) where
  EB e1 b1 <@> EB e2 b2 =
    EB (union (b1 <@> e2)
              (flip ($) <$> b2 <@> e1))
       (b1 <*> b2)

instance Apply (EB t) (Event t) where
  EB e b <@> evt =
    union
      (b <@> evt)
      (filterJust $ flip fmap <$> bhv <@> e)
    where
      bhv = stepper Nothing (Just <$> evt)



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
