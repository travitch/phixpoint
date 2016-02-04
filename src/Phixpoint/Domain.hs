module Phixpoint.Domain (
  Domain(..),
  Lifted(..)
  ) where

-- | An abstract domain of interest.
data Domain d =
  Domain { domTop :: d
         , domLeq :: d -> d -> Bool
         , domLub :: d -> d -> d
         , domJoin :: d -> d -> Maybe d
         }

-- Also add a 'lifted' constructor to wrap a domain 'd' in a lifted type
data Lifted d = Top | Lifted d
  deriving (Eq, Ord, Show)
