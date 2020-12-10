{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}


module NLP.Partage.AStar.ExtWeight
  ( Trav (..)

  -- * Weight ops
  , zeroWeight
  , addWeight
  , sumWeight
  , minWeight
  , disjointUnion
  , safeInsert
  , safeDelete

  -- * Extended Weight
  , ExtWeight (..)
  , totalWeight
  , extWeight0
  , extWeight
  , joinExtWeight
  , joinExtWeight'

  -- * Double Weight
  , DuoWeight (..)
  , duoWeight
  )
where


import           Data.Function          (on)
import qualified Data.Set               as S
import qualified Data.Map.Strict        as M

import           NLP.Partage.AStar.Base
import           NLP.Partage.AStar.Item
import           NLP.Partage.DAG        (Weight)


--------------------------------------------------
-- Traversal
--------------------------------------------------


-- | Traversal represents an action of inducing a new item on the
-- basis of one or two other chart items.  It can be seen as an
-- application of one of the inference rules specifying the parsing
-- algorithm.
--
-- TODO: Sometimes there is no need to store all the arguments of the
-- inference rules, it seems.  From one of the arguments the other
-- one could be derived.
--
-- TODO: Weight component can be extracted outside the Trav datatype.
data Trav n t
    = Scan
        { scanFrom  :: Active n
        -- ^ The input active state
        , _scanTerm :: Tok t
        -- ^ The scanned terminal
        , _weight   :: Weight
        -- ^ The traversal weight
        }
    | Empty
        { scanFrom  :: Active n
        -- ^ The input active state
        , _weight   :: Weight
        -- ^ The traversal weight
        }
    | Subst
        { passArg :: Passive n t
        -- ^ The passive argument of the action
        , actArg  :: Active n
        -- ^ The active argument of the action
        , _weight :: Weight
        -- ^ The traversal weight
        }
    -- ^ Pseudo substitution
--     | Foot
--         { actArg  :: Active n
--         -- ^ The active argument of the action
--         , theFoot :: n
--         -- ^ The foot non-terminal
--         , _weight :: Weight
--         -- ^ The traversal weight
--         }
--     -- ^ Foot adjoin
--     | Adjoin
--         { passAdj :: Passive n t
--         -- ^ The adjoined item
--         , passMod :: Passive n t
--         -- ^ The modified item
--         , _weight   :: Weight
--         -- ^ The traversal weight
--         }
--     -- ^ Adjoin terminate with two passive arguments
    | SisterAdjoin
        { passArg  :: Passive n t
        -- ^ The passive argument of the action
        , actArg   :: Active n
        -- ^ The active argument of the action
        , _weight   :: Weight
        -- ^ The traversal weight
        }
    -- TODO: we don't really need the passive argument, since it performs a
    -- prediction-related function.  See also `Foot`.
    | PredictWrapping
        { actArg   :: Active n
        -- ^ The active argument of the action
        , theDNode :: n
        -- ^ The dnode non-terminal
        , _weight   :: Weight
        -- ^ The traversal weight
        }
    | CompleteWrapping
        { passWrp  :: Passive n t
        -- ^ The wrapping item
        , passMod  :: Passive n t
        -- ^ The modified item
        , slotNum  :: Int
          -- ^ Number of the filled slot (starts with 0)
        , _weight   :: Weight
        -- ^ The traversal weight
        }
    | CompleteWrappingPrim
        { passWrp  :: Passive n t
        -- ^ The wrapping item
        , passMod  :: Passive n t
        -- ^ The modified item
        , slotNum  :: Int
          -- ^ Number of the filled slot (starts with 0)
        , _weight  :: Weight
        -- ^ The traversal weight
        }
    | Deactivate
        { actArg   :: Active n
        -- ^ The active argument of the action
        , _weight  :: Weight
        -- ^ The traversal weight
        }
    | DeactivatePrim
        { actArg   :: Active n
        -- ^ The active argument of the action
        , _weight  :: Weight
        -- ^ The traversal weight
        }
    deriving (Show, Eq, Ord)


--------------------------------------------------
-- Weight (priority)
--------------------------------------------------


-- | Neutral element of the weight/priority.  Corresponds to the
-- logarithm of probability 1.
zeroWeight :: Weight
zeroWeight = 0


-- | Add two weights.
addWeight :: Weight -> Weight -> Weight
addWeight = (+)
{-# INLINE addWeight #-}


-- | Sum weights.
sumWeight :: [Weight] -> Weight
sumWeight = sum
{-# INLINE sumWeight #-}


-- | Minimum of two weights.
minWeight :: Weight -> Weight -> Weight
minWeight = min
{-# INLINE minWeight #-}


-- | Minimum of two weight maps.
-- TODO: that's one of possible definitions!
minWeightMap :: (Ord a) => M.Map a Weight -> M.Map a Weight -> M.Map a Weight
minWeightMap = M.unionWith min
{-# INLINE minWeightMap #-}


-- | Disjoint union of two maps.
disjointUnion :: (Ord a) => M.Map a Weight -> M.Map a Weight -> M.Map a Weight
disjointUnion m1 m2
  | M.null (M.intersection m1 m2) = M.union m1 m2
  | otherwise = error "AStar.ExtWeight.disjointUnion: maps not disjoint"


-- | Insert *new* element in the map.
safeInsert :: (Ord k) => k -> Weight -> M.Map k Weight -> M.Map k Weight
safeInsert k x m
  | M.member k m =
      error "AStar.ExtWeight.safeInsert: key already present"
  | otherwise = M.insert k x m


-- | Delete *existing* element from the map.
safeDelete :: (Ord k) => k -> M.Map k Weight -> M.Map k Weight
safeDelete k m
  | M.member k m = M.delete k m
  | otherwise =
      error "AStar.ExtWeight.safeDelete: key not present"


--------------------------------------------------
-- Extended Weight
--------------------------------------------------


-- | Extended priority which preserves information about the
-- traversal leading to the underlying chart item.  The best weight
-- (priority) of reaching the underlying item is preserved as well.
-- Note that traversals themselves do not introduce any weights.
data ExtWeight n t = ExtWeight
    { priWeight :: Weight
    -- ^ The actual priority.  In case of initial elements
    -- corresponds to weights (probabilities?) assigned to
    -- individual "elementary rules".
    , estWeight :: Weight
    -- ^ Estimated weight to the "end"
    , gapWeight:: M.Map Pos Weight
    -- ^ Estimated weight of the gap
    , prioTrav  :: S.Set (Trav n t)
    -- ^ Traversal leading to the underlying chart item
    } deriving (Show)


-- | Total weight of an item.
totalWeight :: ExtWeight n t -> Weight
#ifdef NewHeuristic
totalWeight ExtWeight{..} =
  priWeight `addWeight`
  estWeight `addWeight`
  sum (M.elems gapWeight)
#else
-- totalWeight ExtWeight{..} = priWeight `addWeight` estWeight
totalWeight ExtWeight{..} = error "Old heuristic not supported anymore"
#endif


instance (Eq n, Eq t) => Eq (ExtWeight n t) where
    (==) = (==) `on` totalWeight
instance (Ord n, Ord t) => Ord (ExtWeight n t) where
    compare = compare `on` totalWeight


-- | Construct an initial `ExtWeight`.  With an empty set of input
-- traversals, it corresponds to a start node in the underlying
-- hyper-graph.
extWeight0 :: DuoWeight -> Weight -> ExtWeight n t
extWeight0 p initEst =
  ExtWeight
  { priWeight = duoBeta p
  , estWeight = initEst
  , gapWeight = duoGap p
  , prioTrav = S.empty }


-- | Construct an `ExtWeight` with one incoming traversal
-- (traversal=hyperarc).
extWeight :: DuoWeight -> Weight -> Trav n t -> ExtWeight n t
extWeight p est trav =
  ExtWeight
  { priWeight = duoBeta p
  , estWeight = est
  , gapWeight = duoGap p
  , prioTrav = S.singleton trav }


-- | Join two extended priorities.
--
-- * The actual priority (cost) preserved is the lower of the two; we are simply
-- keeping the lowest cost of reaching the underlying chart item.
--
-- * The traversals are unioned.
--
-- NOTE: An `ExtWeight` is always assigned to some particular item and, if
-- two `ExtWeight`s are joined, they correspond to the same item.  Therefore,
-- the individual `gapWeight`s corresponds to the same gap.  It follows (I hope)
-- that `gapWeight` is independent from `priWeight` and thus we can minimize
-- them independently.  Estimated weight is not minimized because it should be
-- equal in both items -- otherwise the function throws an error.
--
joinExtWeight
    :: (Ord n, Ord t)
    => Bool -- ^ Save all arcs
    -> ExtWeight n t
    -> ExtWeight n t
    -> ExtWeight n t
joinExtWeight True x y = if estWeight x /= estWeight y
  then error "joinExtWeight: estimation costs differ!"
  else ExtWeight
       { priWeight = minWeight (priWeight x) (priWeight y)
       , estWeight = estWeight x
       , gapWeight = minWeightMap (gapWeight x) (gapWeight y)
       , prioTrav  = S.union (prioTrav x) (prioTrav y)
       }
joinExtWeight False x y
  | estWeight x /= estWeight y =
      error "joinExtWeight: estimation costs differ!"
  | priWeight x < priWeight y = x
      { gapWeight = minWeightMap (gapWeight x) (gapWeight y) }
  | priWeight x > priWeight y = y
      { gapWeight = minWeightMap (gapWeight x) (gapWeight y) }
  | otherwise = x
      { gapWeight = minWeightMap (gapWeight x) (gapWeight y)
      , prioTrav  = S.union (prioTrav x) (prioTrav y) }
--    | otherwise = ExtWeight
--        { priWeight = minWeight (priWeight x) (priWeight y)
--        , estWeight = estWeight x
--        , gapWeight = minWeightMap (gapWeight x) (gapWeight y)
--        , prioTrav  = S.fromList $
--            case (S.toList (prioTrav x), S.toList (prioTrav y)) of
--              ([t1], [t2]) ->
--                if _weight t1 <= _weight t2
--                then [t1]
--                else [t2]
--              ([t1], []) -> [t1]
--              ([], [t2]) -> [t2]
--              ([], []) -> []
--              _ -> error "joinExtWeight: no traversals!"
--        }



joinExtWeight'
    :: (Ord n, Ord t)
    => Bool -- ^ Save all arcs
    -> ExtWeight n t
    -> ExtWeight n t
    -> ExtWeight n t
joinExtWeight' = joinExtWeight


-- joinExtWeight'
--     :: (Ord n, Ord t)
--     => ExtWeight n t
--     -> ExtWeight n t
--     -> ExtWeight n t
-- joinExtWeight' new old =
--   if estWeight new /= estWeight old
--   then error "joinExtWeight[save active/passive]: estimation costs differ!"
--   else
--     if priWeight new < priWeight old
--     then error "joinExtWeight[save active/passive]: new beta value lower than the old!"
--     else
--       ExtWeight
--       (minWeight (priWeight new) (priWeight old))
--       (estWeight new)
--       (S.union (prioTrav new) (prioTrav old))


--------------------------------------------------
-- Double Weights
--------------------------------------------------


-- | The `DuoWeight` type represents a combined priority weight (beta value --
-- weight of the optimal derivation) with gap weight.
data DuoWeight = DuoWeight
  { duoBeta :: Weight
  -- , duoGap  :: Weight
  , duoGap :: M.Map Pos Weight
  }
  deriving (Show, Eq, Ord)


-- | Extract `DuoWeight` from the `ExtWeight` corresponding to some chart item.
duoWeight :: ExtWeight n t -> DuoWeight
duoWeight ExtWeight{..} =
  DuoWeight
  { duoBeta = priWeight
  , duoGap = gapWeight }
