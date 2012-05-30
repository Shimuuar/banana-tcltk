module UI.Widget.List (
  listWidget
  ) where

import Reactive.Banana
import Reactive.Banana.Extra

import UI.TclTk
import UI.TclTk.AST
import UI.TclTk.Builder
import UI.Widget
import UI.Command


listWidget :: Show a => Behavior t [a] -> GUI t p (TkName, Event t (Int,a))
listWidget bhvXs = do
  -- Events
  (cmd,cmdEvt) <- addTclEvent
  xsEvt        <- eventChanges bhvXs
  let
    -- Length of list
    lenEvt = length <$> xsEvt
    -- function to transform event
    getEvents ixE = listEvents xsEvt $ cmdEvt `union` (JumpTo <$> ixE)

  ----------------------------------------
  -- Build UI
  frame [Fill FillX] $
    withPack PackLeft $ do
      spacer
      button [Text "<|" ] [] $ cmd  ToBegin
      button [Text "<<<"] [] $ cmd (MoveBack 10)
      button [Text "<"  ] [] $ cmd (MoveBack 1)
      --
      spacer
      (_,evt) <- finiWidget . modifyWidgetM fst getEvents =<< entryInt [] [] 0
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
      return  evt



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
  $ fmap Left listEvt `union` fmap Right command
  where
    --
    fini (Cursor _ xs i) = Just (i, xs !! i)
    fini Invalid         = Nothing
    -- Accumulate data
    acc Invalid (Left []) = Invalid
    acc Invalid (Left xs) = Cursor (length xs) xs 0
    acc Invalid _         = Invalid
    acc (Cursor _   _  n) (Left xs) =
      Cursor len xs $ clip len n
      where
        len = length xs
    acc (Cursor len xs n) (Right c) =
      case c of
        MoveFwd  d -> Cursor len xs $ clip len $ n + d
        MoveBack d -> Cursor len xs $ clip len $ n - d
        JumpTo   d -> Cursor len xs $ clip len d
        ToBegin    -> Cursor len xs 0
        ToEnd      -> Cursor len xs $ len - 1
    -- Clip out of range indices
    clip len i | i < 0     = 0
               | i >= len  = len -1
               | otherwise = i
