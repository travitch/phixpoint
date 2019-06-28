-- | An IR-independent representation of flow graphs
--
-- These types are intended to provide a uniform interface for the
-- fixpoint engine to traverse flow graphs.
--
-- For now, it only supports "local" flow graphs that have no explicit
-- representations of call semantics.
module Phixpoint.FlowGraph (
  FlowGraph(..)
  ) where

-- | A flow graph where 'n' is the type of nodes.
--
-- Note, efficient comparison of 'n' is very desirable.
data FlowGraph n =
  FlowGraph { fgEntry :: n
            , fgSuccessors :: n -> [n]
            , fgPredecessors :: n -> [n]
            , fgExits :: [n]
            }







