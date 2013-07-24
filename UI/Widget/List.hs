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


-- | List widget
listWidget :: Frameworks t => Event t [a] -> GUI t p (TkName, Event t (Int,a))
listWidget eList = do
  frame [Fill FillX] $
    withPack PackLeft $ do
      let toEvt x (_,e) = x <$ e
      -- Buttons
      spacer
      e1 <- toEvt ToBegin       <$> button [Text "<|" ] []
      e2 <- toEvt (MoveBack 10) <$> button [Text "<<<"] []
      e3 <- toEvt (MoveBack 1 ) <$> button [Text "<"  ] []
      -- Central area
      spacer
      Widget _ eN finiN <- entryInt [] []
      _       <- label [ Text  " / " ] []
      labN    <- label []              []
      actimateTcl (length <$> eList) $ do
        configure labN $ LamOpt $ Text . show
      spacer
      -- More buttons
      e4 <- toEvt (MoveFwd 1 ) <$> button [Text ">"  ] []
      e5 <- toEvt (MoveFwd 10) <$> button [Text ">>>"] []
      e6 <- toEvt ToEnd        <$> button [Text "|>" ] []
      spacer
      -- OOPS
      let events = listEvents eList (unions [JumpTo <$> eN, e1, e2, e3, e4, e5, e6])
      finiN $ Bhv 0 $ fst <$> events
      return events




-- Commands for a list
data ListCmd
  = MoveFwd  Int
  | MoveBack Int
  | JumpTo   Int
  | ToBegin
  | ToEnd
  deriving (Show)

-- Cursor for list of values
data Cursor a
  = Cursor Int [a] Int          -- Length × list × position
  | Invalid


listEvents :: Event t [a] -> Event t ListCmd -> Event t (Int,a)
listEvents listEvt command
  = filterJust
  $ fmap fini
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
