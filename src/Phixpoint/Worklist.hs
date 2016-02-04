module Phixpoint.Worklist (
  Worklist,
  empty,
  addWork,
  takeWork
  ) where

import qualified Data.Sequence as Seq
import qualified Data.Set as S

data Worklist a = Worklist { wlSeq :: !(Seq.Seq a)
                           , wlContained :: !(S.Set a)
                           }

empty :: Worklist a
empty = Worklist { wlSeq = Seq.empty
                 , wlContained = S.empty
                 }

addWork :: (Ord a) => Worklist a -> a -> Worklist a
addWork wl a
  | S.member a (wlContained wl) = wl
  | otherwise = wl { wlSeq = wlSeq wl Seq.|> a
                   , wlContained = S.insert a (wlContained wl)
                   }

takeWork :: (Ord a) => Worklist a -> (Maybe a, Worklist a)
takeWork wl =
  case Seq.viewl (wlSeq wl) of
    Seq.EmptyL -> (Nothing, wl)
    work Seq.:< rest -> (Just work, wl { wlSeq = rest
                                       , wlContained = S.delete work (wlContained wl)
                                       })
