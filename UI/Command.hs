{-# LANGUAGE FlexibleInstances #-}
module UI.Command (
    Command(..)
  ) where

import Control.Applicative
import Data.Char


-- | Since communication with Tcl goes through pipe commands which are
--   sent and recieved must be serialized and deserialized. This type
--   class provides exactrly this functionality.
--
--   FIXME: lexing of message is not implemented correctly.
class Command a where
  encode :: a -> [String]
  decode :: [String] -> Maybe a



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance Command () where
  encode _      = ["()"]
  decode ["()"] = Just ()
  decode _      = Nothing

instance Command Integer where
  encode = encodeRead
  decode = decodeRead

instance Command Int where
  encode = encodeRead
  decode = decodeRead

instance Command Float where
  encode = encodeRead
  decode = decodeRead

instance Command Double where
  encode = encodeRead
  decode = decodeRead

instance Command [Char] where
  encode     = (:[])
  decode [s] = Just s
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



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

encodeRead :: (Show a) => a -> [String]
encodeRead = pure . show

decodeRead :: Read a => [String] -> Maybe a
decodeRead [s] =
  case reads s of [(i,"")] -> Just i
                  _        -> Nothing
decodeRead _ = Nothing
