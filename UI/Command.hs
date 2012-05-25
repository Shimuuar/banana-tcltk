module UI.Command (
    Command(..)
  , Cmd(..)
  ) where

import Data.Char


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

instance Command Bool where
  encode True  = ["true" ]
  encode False = ["false"]
  decode [c] =
    case map toLower c of
      "true"  -> Just True
      "1"     -> Just True
      "false" -> Just False
      "0"     -> Just False
      _       -> Nothing
  decode _ = Nothing
