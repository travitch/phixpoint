-- | Implements a context-sensitive fixpoint computation
--
-- This implementation is based on the algorithm described by Rohan
-- Padhye and Uday Khedker:
--
-- > http://dl.acm.org/citation.cfm?doid=2487568.2487569
--
-- At a high level, this approach is similar to algorithms that
-- distinguish contexts by call strings.  Instead of using call
-- strings directly, though, it is based on _values_.  If multiple
-- contexts have the same input abstract value, they are considered to
-- be equivalent.  This observation leads to a method for
-- automatically handling recursive cycles in the call graph.
--
-- There are a few type parameters used consistently in this module.
--
-- * @n@ is the type of nodes (i.e., program instructions)
--
-- * @c@ is the type of callees (i.e., functions called by call instructions and then returned from)
--
-- * @a@ is the type of abstract values tracked for a program
module Phixpoint.ValueContext (
  Domain(..),
  Analysis(..),
  CalleeDescription(..),
  runSolver
  ) where

import Control.Applicative
import Control.Monad ( ap )
import Control.Monad.ST ( ST, runST )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.STRef as R
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Resizable as RV

-- | An abstract domain of interest.
data Domain a =
  Domain { dTop :: a
         , dMeet :: a -> a -> a
         , dEq :: a -> a -> Bool
         , dLeq :: a -> a -> Bool
         }

data Analysis n c a =
  Analysis { aTransfer      :: Context c a -> n -> a -> a
             -- ^ The standard intraprocedural transfer function
           , aExitTransfer  :: Context c a -> c -> n -> a -> a
             -- ^ Transfer function for exit nodes.  This can be used
             -- to remove undesired information about local variables.
           , aLocalTransfer :: Context c a -> n -> a -> a
             -- ^ Transfer function to maintain local information when
             -- processing call-return edges.
           , aCallTransfer  :: Context c a -> c -> n -> a -> a
             -- ^ Transfer function for call edges (usually mapping
             -- actual arguments to formal parameters).
           , aTargetCallees :: Context c a -> n -> a -> [c]
             -- ^ Return the list of callees for a given call node.
             -- If one of the callees is new, it will be added with
             -- help from 'aAddCallee'.  This can facilitate dynamic
             -- call graph construction, as well as determining
             -- callees in a context-sensitive way.
             --
             -- Adding callees will work well.  Removing them seems
             -- problematic...  That might break monotonicity.
           , aAddCallee     :: c -> CalleeDescription n c
             -- ^ Get the nodes and edges for a newly-discovered
             -- callee.  These will be interned as appropriate.
           }

data CalleeDescription n c =
  CalleeDescription { calleeNodes :: [n]
                    , calleeEntry :: n
                    , calleeEdges :: (n, n)
                    , calleeLocalEdges :: (n, n)
                    , calleeExits :: [n]
                    }

-- We could do something interesting here and possibly create
-- context-sensitive call graphs.
--
-- We could use the same context mechanism as is used for abstract
-- values.  Furthermore, we could even lazily "split" the call graph
-- at those spots.  Even if the context is different, we don't need to
-- split the call graph if the targets are the same.
--
-- I'm not sure how often the context-sensitive callgraph would be
-- useful, but I suspect it would be critical for java and dynamic
-- languages.  Perhaps less important for C.

data Result = Result

runSolver :: [CalleeDescription n c] -> c -> a -> Result
runSolver g0 entry val0 = runST $ do
  callees <- RV.new 4096
  nodes <- RV.new 4096
  inputs <- RV.new 4096
  outputs <- RV.new 4096
  exitValues <- RV.new 4096
  nodeIds <- R.newSTRef M.empty
  calleeIds <- R.newSTRef M.empty
  contextIds <- R.newSTRef M.empty
  abstractIndex <- R.newSTRef M.empty
  calleeContexts <- R.newSTRef M.empty
  let env = SolverEnv { sCallees = callees
                      , sNodes = nodes
                      , sInputs = inputs
                      , sOutputs = outputs
                      , sExitValues = exitValues
                      , sNodeIds = nodeIds
                      , sCalleeIds = calleeIds
                      , sContextIds = contextIds
                      , sAbstractIndex = abstractIndex
                      , sCalleeContexts = calleeContexts
                      }
  runS (bootstrapSystem g0 entry val0 >> solveSystem entry val0) env

bootstrapSystem :: [CalleeDescription n c] -> c -> a -> Solver s n c a ()
bootstrapSystem = undefined

solveSystem :: c -> a -> Solver s n c a Result
solveSystem entry val0 = return Result

-- | A node represents an individual instruction.  These are indices
-- into 'sNodes', 'sInputs', and 'sOutputs'.  They are allocated
-- sequentially.
newtype Node = Node Int
             deriving (Eq, Ord, Show)

-- | A 'Callee' uniquely names a callable unit of code.  These are
-- indices into 'sCallees'.
newtype Callee = Callee Int
               deriving (Eq, Ord, Show)

-- | A context partitions different dataflow facts for the same callee
-- based on the calling context in which it has been invoked.  For
-- this analysis, contexts are tracked based on 1) the calling method
-- and 2) the abstract value at the entry point of the callee.
data Context c a = Context c a

newtype Ctx = Ctx Int
            deriving (Eq, Ord, Show)

-- addNode :: n -> Solver ()
-- addNode = undefined

-- addIntraproceduralEdge :: (Ord n) => n -> n -> Solver ()
-- addIntraproceduralEdge = undefined

-- addCallEdge :: (Ord n, Ord c) => n -> c -> n -> Solver ()
-- addCallEdge = undefined

-- addReturnEdge :: (Ord n) => n -> Solver ()
-- addReturnEdge = undefined

newtype Ref = Ref Int
            deriving (Eq, Ord, Show)

data SolverEnv s n c a =
  SolverEnv { sCallees :: RV.RVector (MV.STVector s) s c
              -- ^ Indexed by Callee
            , sNodes :: RV.RVector (MV.STVector s) s n
              -- ^ Indexed by Node
            , sInputs :: RV.RVector (MV.STVector s) s a
              -- ^ Indexed by Ref
            , sOutputs :: RV.RVector (MV.STVector s) s a
              -- ^ Indexed by Ref
            , sExitValues :: RV.RVector (MV.STVector s) s a
              -- ^ Indexed by 'Ctx'
            , sNodeIds :: R.STRef s (Map n Node)
            , sCalleeIds :: R.STRef s (Map c Callee)
            , sContextIds :: R.STRef s (Map (Context c a) Ctx)
            , sAbstractIndex :: R.STRef s (Map (Context c a, n) Ref)
              -- ^ A mapping of (X, n) -> Ref, which is an index into
              -- 'sInputs' and 'sOutputs'
            , sCalleeContexts :: R.STRef s (Map c (Set (Context c a)))
              -- ^ Track all of the contexts for each callee.  This
              -- doesn't need to be very efficient because it is never
              -- used in the solver itself - just when examining
              -- results.
            }

newtype Solver s n c a b = Solver { runS :: SolverEnv s n c a -> ST s b }

instance Monad (Solver s n c a) where
  {-# INLINE return #-}
  {-# INLINE (>>) #-}
  {-# INLINE (>>=) #-}
  return x = Solver $ \_ -> return x
  (>>=) = bindSolver

{-# INLINE bindSolver #-}
bindSolver :: Solver s n c a x -> (x -> Solver s n c a y) -> Solver s n c a y
bindSolver m k = Solver $ \r -> do
  a <- runS m r
  runS (k a) r

instance Functor (Solver s n c a) where
  fmap f m = Solver $ \r -> do
    a <- runS m r
    return (f a)

instance Applicative (Solver s n c a) where
  pure = return
  (<*>) = ap

{-# INLINE ask #-}
ask :: Solver s n c a (SolverEnv s n c a)
ask = Solver $ \r -> return r

{-# INLINE asks #-}
asks :: (SolverEnv s n c a -> b) -> Solver s n c a b
asks f = Solver $ \r -> return (f r)
