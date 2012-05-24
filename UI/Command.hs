module UI.Command (
    Command(..)
  , Cmd(..)
  ) where

-- | Type class for command which could be serialized and deserialized
--   strings
class Command a where
  encode :: a -> [String]
  decode :: [String] -> Maybe a

data Cmd a = Cmd
  { cmdPrexif :: String
  , cmdValue  :: a
  }


instance Command () where
  encode _      = ["()"]
  decode ["()"] = Just ()
  decode _      = Nothing

instance Command Integer where
  encode i   = [show i]
  decode [s] = case reads s of [(i,"")] -> Just i
                               _        -> Nothing
  decode _   = Nothing

instance Command Int where
  encode i   = [show i]
  decode [s] = case reads s of [(i,"")] -> Just i
                               _        -> Nothing
  decode _   = Nothing
