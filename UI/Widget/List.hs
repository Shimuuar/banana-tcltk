module UI.Widget.List (
  listWidget
  ) where

import Reactive.Banana
import Reactive.Banana.Extra
import Reactive.Banana.Frameworks

import UI.TclTk
import UI.TclTk.AST
import UI.TclTk.Builder
import UI.Widget
import UI.Command


listWidget :: Frameworks t => EB t [a] -> GUI t p (TkName, Event t (Int,a))
listWidget (EB evtXs bhvXs) = do
  -- Events
  (pref,cmdEvt) <- addTclEvent
  -- Function to transform event
  let getEvents ixE = listEvents evtXs $ cmdEvt `union` (JumpTo <$> ixE)
  ----------------------------------------
  -- Build UI
  frame [Fill FillX] $
    withPack PackLeft $ do
      let cmd = Cmd pref
      spacer
      button [Text "<|" ] [] $ cmd  ToBegin
      button [Text "<<<"] [] $ cmd (MoveBack 10)
      button [Text "<"  ] [] $ cmd (MoveBack 1)
      --
      spacer
      (_,evt,_) <- finiWidget . modifyWidgetM fst getEvents =<< entryInt [] [] 0
      _       <- label [ Text  " / " ] []
      labN    <- label []              []
      spacer
      --
      button [Text ">"  ] [] $ cmd (MoveFwd  1)
      button [Text ">>>"] [] $ cmd (MoveFwd  10)
      button [Text "|>" ] [] $ cmd  ToEnd
      spacer
      -- Actions
      actimateTclB (length <$> bhvXs) $ do
        configure labN $ LamOpt $ Text . show
      return evt



-- | Commands for a list
data ListCmd
  = MoveFwd  Int
  | MoveBack Int
  | JumpTo   Int
  | ToBegin
  | ToEnd
  deriving (Show)

data Cursor a
  = Cursor Int [a] Int
  | Invalid

instance Command ListCmd where
  encode (MoveFwd  n) = ["fwd",  show n]
  encode (MoveBack n) = ["back", show n]
  encode (JumpTo   n) = ["jump", show n]
  encode  ToBegin     = ["begin"]
  encode  ToEnd       = ["end"]
  decode ("fwd"  : n) = MoveFwd  <$> decode n
  decode ("back" : n) = MoveBack <$> decode n
  decode ("jump" : n) = JumpTo   <$> decode n
  decode ["begin"]    = Just   ToBegin
  decode ["end"]      = Just   ToEnd
  decode _            = Nothing


listEvents :: Event t [a] -> Event t ListCmd -> Event t (Maybe (Int,a))
listEvents listEvt command
  = fmap fini
  $ scanE acc Invalid
  $ listEvt `joinE` command
  where
    --
    fini (Cursor _ xs i) = Just (i, xs !! i)
    fini Invalid         = Nothing
    -- Accumulate data
    acc Invalid (Left []) = Invalid
    acc Invalid (Left xs) = Cursor (length xs) xs 0
    acc Invalid _         = Invalid
    acc (Cursor _   _  n) (Left xs) =
      Cursor len xs $ clip len n where len = length xs
    acc (Cursor len xs n) (Right c) =
      case c of
        MoveFwd  d -> go $ n + d
        MoveBack d -> go $ n - d
        JumpTo   d -> go   d
        ToBegin    -> go   0
        ToEnd      -> go $ len - 1
      where
        go = Cursor len xs . clip len
    -- Clip out of range indices
    clip len i | i < 0     = 0
               | i >= len  = len -1
               | otherwise = i
