module Reactive.Banana.Extra where

import Reactive.Banana

-- | 'scanl' like accumulation function
scanE :: (a -> b -> a) -> a -> Event t b -> Event t a
scanE fold x0 = accumE x0 . fmap (flip fold)

actimateWith :: (a -> IO ()) -> Event t a -> NetworkDescription t ()
actimateWith f = reactimate . fmap f

actimateWith_ :: IO () -> Event t a -> NetworkDescription t ()
actimateWith_ f = reactimate . fmap (const f)

zipE :: (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
zipE f b e = fmap f b <@> e
