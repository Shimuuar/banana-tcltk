module UI.Widget.List (
  listWidget
  ) where

import Control.Arrow
import Control.Monad.Trans.Class

import Reactive.Banana
import Reactive.Banana.Extra

import UI.TclTk
import UI.TclTk.AST
import UI.TclTk.Builder
import UI.Reactive
import UI.Widget
import Debug.Trace



listWidget :: Show a => Source -> Behavior t [a] -> Gui t p (Widget t (Int,a))
listWidget src bhvXs = do
  -- Events
  cmdEvt  <- lift $ addEventSource src
  initEvt <- getParameter
  xsEvt   <- lift $ union (bhvXs <@ initEvt) <$> changes bhvXs
  let lenEvt = length <$> xsEvt
      evt    = listEvents xsEvt $ tclEvent cmdEvt
      go     = cmd cmdEvt
  ----------------------------------------
  -- Build UI
  lift $ actimateWith print lenEvt
  name <- frame [] $
    withPack PackLeft $ do
      --
      button [Text "<|" ] [] $ go  ToBegin
      button [Text "<<<"] [] $ go (MoveBack 10)
      button [Text "<"  ] [] $ go (MoveBack 1)
      -- labels
      nm   <- label [ Width 10    ] []
      _    <- label [ Text  " / " ] []
      labN <- label []              []
      actimateTcl src lenEvt $ do
        configure labN $ LamOpt $ Text . show
      actimateTcl src evt $ do
        configure nm $ LamOpt $ \e ->
          case e of
            Just (i,_) -> Text $ show i
            Nothing    -> Text "-"
      --
      button [Text ">"  ] [] $ go (MoveFwd  1)
      button [Text ">>>"] [] $ go (MoveFwd  10)
      button [Text "|>" ] [] $ go  ToEnd
      -- Network
      return ()
  -- Return data
  return $ Widget name $ filterJust evt



-- | Commands for a list
data ListCmd
  = MoveFwd  Int
  | MoveBack Int
  | ToBegin
  | ToEnd
  deriving (Show)

data Cursor a
  = Cursor Int [a] Int
  | Invalid

instance Command ListCmd where
  encode (MoveFwd  n) = ["fwd",  show n]
  encode (MoveBack n) = ["back", show n]
  encode  ToBegin     = ["begin"]
  encode  ToEnd       = ["end"]
  decode ["fwd" , n]  = Just $ MoveFwd (read n)
  decode ["back", n]  = Just $ MoveBack (read n)
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
    acc Invalid _
      | trace "ACC Invalid" False = undefined
    acc (Cursor len _ i) _
      | traceShow ("ACC",len,i) False = undefined
    acc _ (Left xs)
      | traceShow ("LEFT: ",length xs) False = undefined
    acc _ (Right c)
      | traceShow ("RIGHT", c) False = undefined


    acc Invalid (Left []) = Invalid
    acc _       (Left xs) = trace "NEWCURSOR" Cursor (length xs) xs 0
    acc Invalid _         = Invalid
    acc (Cursor len xs n) (Right c) =
      case c of
        MoveFwd  d -> Cursor len xs $ clip len $ n + d
        MoveBack d -> Cursor len xs $ clip len $ n - d
        ToBegin    -> Cursor len xs 0
        ToEnd      -> Cursor len xs $ len - 1
    -- Clip out of range indices
    clip len i | i < 0     = 0
               | i >= len  = len -1
               | otherwise = i
