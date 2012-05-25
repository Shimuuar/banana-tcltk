module Reactive.Banana.Extra where

import Control.Applicative
import Reactive.Banana

-- | 'scanl' like accumulation function
scanE :: (a -> b -> a) -> a -> Event t b -> Event t a
scanE fold x0 = accumE x0 . fmap (flip fold)

actimateWith :: (a -> IO ()) -> Event t a -> NetworkDescription t ()
actimateWith f = reactimate . fmap f

actimateWith_ :: IO () -> Event t a -> NetworkDescription t ()
actimateWith_ f = reactimate . fmap (const f)

actimateBhvWith :: (a -> IO ()) -> Behavior t a -> NetworkDescription t ()
actimateBhvWith f bhv
  = actimateWith f =<< changes bhv

zipE :: (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
zipE f b e = fmap f b <@> e


pairWith :: (a -> b -> c) -> Event t a -> Event t b -> Event t c
pairWith f ea eb
  = filterJust
  $ fmap fini
  $ scanE acc (Nothing,Nothing)
  $ fmap Left ea `union` fmap Right eb
  where
    acc (_,b) (Left  a) = (Just a, b)
    acc (a,_) (Right b) = (a, Just b)
    fini (a,b) = f <$> a <*> b

pairE :: Event t a -> Event t b -> Event t (a, b)
pairE = pairWith (,)
