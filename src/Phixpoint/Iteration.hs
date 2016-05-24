{-# LANGUAGE BangPatterns #-}
module Phixpoint.Iteration (
  Interpretation(..),
  fixpoint,
  Result,
  lookupAbstractValue
  ) where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M

import Phixpoint.Domain
import Phixpoint.FlowGraph
import qualified Phixpoint.Worklist as W

data Result n' d = Result !(M.Map n' d)

lookupAbstractValue :: (Ord n') => Result n' d -> n' -> Maybe d
lookupAbstractValue (Result r) n = M.lookup n r

-- | An interpretation of a flow graph with nodes of type @n@ in
-- domain @d@.  The @n'@ parameter is a projection from @n@ that is
-- used to index the result map.
--
-- This is a little baroque, but it allows the CFG nodes to be a
-- different type that is efficient for traversal, but also allow @n'@
-- to be more efficient for result lookup later.
data Interpretation n n' d =
  Interpretation { iTransfer :: n' -> d -> d
                 , iProject :: n -> n'
                 }

-- | Find the fixpoint of an abstract 'Interpretation' of a 'FlowGraph'
fixpoint :: (Ord n, Ord n', Eq d) => Domain d -> Interpretation n n' d -> FlowGraph n -> Result n' d
fixpoint d i g = Result (go wl0 abst0)
  where
    wl0 = W.addWork W.empty (fgEntry g)
    abst0 = M.empty

    go !wl !abst
      | (Just n, wl') <- W.takeWork wl =
        case computeAbstraction d i g n abst of
          -- In this case, the abstraction did not change
          Nothing -> go wl' abst
          -- Otherwise, we have new work items and an updated
          -- abstraction
          Just abst' ->
            let wl'' = F.foldl' W.addWork wl' (fgSuccessors g n)
            in go wl'' abst'
      | otherwise = abst

computeAbstraction :: (Ord n', Eq d)
                   => Domain d
                   -> Interpretation n n' d
                   -> FlowGraph n
                   -> n
                   -> M.Map n' d
                   -> Maybe (M.Map n' d)
computeAbstraction d i g n abst
  | curAbs == newAbs = Nothing
  | otherwise = Just (M.insert (iProject i n) newAbs abst)
  where
    curAbs = lookupCurrentAbstraction d abst (iProject i n)
    predAs = map (lookupCurrentAbstraction d abst . iProject i) (fgPredecessors g n)
    inputA = F.foldl' (domLub d) (domTop d) predAs
    newAbs = iTransfer i (iProject i n) inputA

lookupCurrentAbstraction :: (Ord n') => Domain d -> M.Map n' d -> n' -> d
lookupCurrentAbstraction d m n = M.findWithDefault (domTop d) n m
