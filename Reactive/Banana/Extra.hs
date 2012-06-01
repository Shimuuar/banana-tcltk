module Reactive.Banana.Extra where

import Control.Applicative
import Reactive.Banana


----------------------------------------------------------------
-- Various scans
----------------------------------------------------------------

-- | 'scanl' like accumulation function
scanE :: (a -> b -> a) -> a -> Event t b -> Event t a
scanE fold x0 = accumE x0 . fmap (flip fold)

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


injectModify :: Event t a -> Event t (a -> a) -> Event t a
injectModify ea ef
  = filterJust
  $ scanE acc Nothing
  $ joinE ea ef
  where
    acc _ (Left  a) = Just a
    acc a (Right f) = fmap f a

addTicks :: Event t a -> Event t b -> Event t a
addTicks ea eb
  = filterJust
  $ scanE2 (const Just) (\x _ -> x) Nothing ea eb

----------------------------------------------------------------
-- Zips
----------------------------------------------------------------

joinE :: Event t a -> Event t b -> Event t (Either a b)
joinE ea eb = (Left <$> ea) `union` (Right <$> eb)

zipE :: (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
zipE f b e = fmap f b <@> e


pairWith :: (a -> b -> c) -> Event t a -> Event t b -> Event t c
pairWith f ea eb
  = filterJust
  $ fmap fini
  $ scanE acc (Nothing,Nothing)
  $ joinE ea eb
  where
    acc (_,b) (Left  a) = (Just a, b)
    acc (a,_) (Right b) = (a, Just b)
    fini (a,b) = f <$> a <*> b

pairE :: Event t a -> Event t b -> Event t (a, b)
pairE = pairWith (,)


maybeEvent :: (Event t a -> Event t b) -> Event t (Maybe a) -> Event t (Maybe b)
maybeEvent f e
  = unions [ fmap Just $ f $ filterJust e     -- Just case
           , filterJust $ fmap nothingCase e  -- Nothing case
           ]
  where
    nothingCase Nothing = Just Nothing
    nothingCase _       = Nothing



----------------------------------------------------------------
-- Actimate variants
----------------------------------------------------------------

actimateWith :: (a -> IO ()) -> Event t a -> NetworkDescription t ()
actimateWith f = reactimate . fmap f

actimateWith_ :: IO () -> Event t a -> NetworkDescription t ()
actimateWith_ f = reactimate . fmap (const f)

actimateBhvWith :: (a -> IO ()) -> Behavior t a -> NetworkDescription t ()
actimateBhvWith f bhv
  = actimateWith f =<< changes bhv
