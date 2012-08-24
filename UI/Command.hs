{-# LANGUAGE FlexibleInstances #-}
module UI.Command (
    Command(..)
  , lex
  , unlex
  ) where

import Control.Arrow       (first)
import Control.Applicative
import Data.Char
import Data.List           (intercalate)
import Prelude hiding (lex)



-- | Lex string read from Tcl to chunks. Spaces are used as
--   separators. Literal spaces and backslashes are backslash
--   escaped. Tabs are not separators.
lex :: String -> [String]
lex s =
  case dropWhile (== ' ') s of
    "" -> []
    w  -> case walk w of
            (tok,rest) -> tok : lex rest
  where
    -- Break & unescaping
    walk (     ' ' :ss) = ("",ss)
    walk ('\\':'\\':ss) = first ('\\':) $ walk ss
    walk ('\\':' ' :ss) = first (' ' :) $ walk ss
    walk (       c :ss) = first ( c  :) $ walk ss
    walk []             = ("","")

-- | Convert string for printing by Tcl
unlex :: [String] -> String
unlex ss
  = intercalate " "
  $ map (concatMap escape) ss
  where
    escape ' '  = "\\ "
    escape '\\' = "\\\\"
    escape  c   = [c]



-- | Since communication with Tcl goes through pipe commands which are
--   sent and recieved must be serialized and deserialized. This type
--   class provides exactrly this functionality.
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

instance (Command a, Command b) => Command (a,b) where
  encode (a,b) = map ('_':) $ encode a ++ [""] ++ encode b
  decode strs  = do
    let strip ('_':s) = Just s
        strip _       = Nothing
    ss <- mapM strip strs
    case break null ss of
      (a, "":b) -> (,) <$> decode a <*> decode b
      _         -> Nothing



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
