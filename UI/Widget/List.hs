module UI.Widget.List (
  startList
  ) where

import Control.Monad.Trans.Class

import Reactive.Banana
import Reactive.Banana.Extra

import UI.TclTk
import UI.TclTk.AST
import UI.TclTk.Builder
import UI.Reactive


-- | Commands for a list
data ListCmd
  = MoveFwd  Int
  | MoveBack Int
  | ToBegin
  | ToEnd
  deriving (Show)

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



-- | Behavior of a list
listEvents :: [a] -> Event t ListCmd -> Event t (Int,a)
listEvents [] _   = error "Empty list"
listEvents xs evt 
  = fmap (\i -> (i, xs !! i)) $ scanE go 0 evt
  where
    go n (MoveFwd  d) = clip $ n + d
    go n (MoveBack d) = clip $ n - d
    go _  ToBegin     = 0
    go _  ToEnd       = len - 1
    --
    clip i | i < 0     = 0
           | i >= len  = len -1
           | otherwise = i
    --
    len = length xs



startList :: Show a => Source -> [a] -> Gui t p (Event t (Int,a))
startList _   [] = return never
startList src xs = do
  -- Events
  e       <- lift $ addEventSource src
  initEvt <- getParameter
  let evt = unions [ fmap (const (0, head xs)) initEvt
                   , listEvents xs $ tclEvent e
                   ]
  -- UI
  frame [] $
    withPack PackLeft $ do
      -- GUI
      button [Text "<|" ] [] $ cmd e  ToBegin
      button [Text "<<<"] [] $ cmd e (MoveBack 10)
      button [Text "<"  ] [] $ cmd e (MoveBack 1)
      nm <- label [ Text "0" , Width 10 ] []
      label [ Text $ " / " ++ show (length xs) ] []
      button [Text ">"  ] [] $ cmd e (MoveFwd  1)
      button [Text ">>>"] [] $ cmd e (MoveFwd  10)
      button [Text "|>" ] [] $ cmd e  ToEnd
      -- Network
      actimateTcl src (fmap fst evt) $
        configure nm (LamOpt $ \i -> Text (show i))
  -- Return data
  return evt
