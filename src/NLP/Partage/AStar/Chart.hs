{-# LANGUAGE RecordWildCards #-}


-- | A representation of the parsing chart -- the closed set of the
-- A* parsing hypergraph.


module NLP.Partage.AStar.Chart
  ( Chart
  , empty

  -- * Stats
  , listPassive
  , listActive
  , doneNodesNum
  , doneEdgesNum

  -- * Active
  , activeTrav
  , isProcessedA
  , saveActive
  , hasActiveTrav

  -- * Passive
  , passiveTrav
  , isProcessedP
  , savePassive
  , hasPassiveTrav

  -- * Extraction
  , isFinal
  , finalFrom
  , expectEnd
  , rootSpan
  , rootEnd
  , provideBeg
  , provideBeg'
  , provideBegIni
  , provideBegIni'
  , withGap
  , provideBegAux
  , auxModifyGap
)
where


import           Control.Monad               ((>=>), guard)
import           Data.Lens.Light
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromJust, maybeToList)
import qualified Data.Set                    as S

import           Control.Monad.Trans.Class   (lift)
import qualified Control.Monad.State.Class   as MS
import qualified Control.Arrow as Arr

import qualified Pipes                       as P
-- import           Pipes                      ((>->))
-- import qualified Pipes.Prelude               as P

import           Data.DAWG.Ord               (ID)

import           NLP.Partage.SOrd
import           NLP.Partage.DAG             (Weight)
import qualified NLP.Partage.DAG             as DAG
import qualified NLP.Partage.Tree.Other      as O

import           NLP.Partage.AStar.Auto      (Auto (..), NotFoot(..))
import           NLP.Partage.AStar.Base
import           NLP.Partage.AStar.ExtWeight
import           NLP.Partage.AStar.Item


-- | A hypergraph dynamically constructed during parsing.
data Chart n t = Chart
    { doneActive  :: M.Map (Active n) (ExtWeight n t)
    -- ^ Processed active items.

    , donePassive :: M.Map (Passive n t) (ExtWeight n t)
    -- ^ Processed passive items.
    }


-- | Create an empty chart.
empty :: Chart n t
empty = Chart
  { doneActive = M.empty
  , donePassive = M.empty
  }



--------------------------------------------------
-- Chart Stats
--------------------------------------------------


-- | List all passive done items together with the corresponding
-- traversals.
listPassive :: Chart n t -> [(Passive n t, ExtWeight n t)]
listPassive = M.toList . donePassive


-- | List all active done items together with the corresponding
-- traversals.
listActive :: Chart n t -> [(Active n, ExtWeight n t)]
listActive = M.toList . doneActive


-- | Number of chart nodes.
doneNodesNum :: Chart n t -> Int
doneNodesNum e
    = length (listPassive e)
    + length (listActive e)


-- | Number of edges outgoing from the nodes in the underlying chart.
doneEdgesNum :: Chart n t -> Int
doneEdgesNum e
    = sumTrav (listPassive e)
    + sumTrav (listActive e)


--------------------
-- Active items
--------------------


-- | Return the corresponding set of traversals for an active item.
activeTrav
    :: (Ord n, Ord t)
    => Active n -> Chart n t
    -> Maybe (ExtWeight n t)
activeTrav p = M.lookup p . doneActive


-- | Check if the active item is not already processed.
isProcessedA :: (Ord n, Ord t) => Active n -> Chart n t -> Bool
isProcessedA p =
    check . activeTrav p
  where
    check (Just _) = True
    check _        = False


-- | Mark the active item as processed (`done').
saveActive
    :: (Ord t, Ord n)
    => M.Map ID (NotFoot n) -- ^ See `lhsNonTerm` from `Auto`
    -> Active n
    -> ExtWeight n t
    -> Chart n t
    -> Chart n t
saveActive lhsMap p ts chart =
  chart
  { doneActive = newDone
  }
  where
    newDone = M.insertWith joinExtWeight' p ts (doneActive chart)


-- | Check if, for the given active item, the given transitions are already
-- present in the hypergraph.
hasActiveTrav
    :: (Ord t, Ord n)
    => Active n
    -> S.Set (Trav n t)
    -> Chart n t
    -> Bool
hasActiveTrav p travSet chart =
  case activeTrav p chart of
    Just ExtWeight{..} -> travSet `S.isSubsetOf` prioTrav
    Nothing -> False


--------------------
-- Passive items
--------------------


-- | Return the corresponding set of traversals for a passive item.
passiveTrav
    :: (Ord n, Ord t)
    => Passive n t
    -> Auto n t
    -> Chart n t
    -> Maybe (ExtWeight n t)
passiveTrav p _ = M.lookup p . donePassive


-- | Check if the state is not already processed.
isProcessedP :: (Ord n, Ord t) => Passive n t -> Auto n t -> Chart n t -> Bool
isProcessedP x auto =
    check . passiveTrav x auto
  where
    check (Just _) = True
    check _        = False


-- | Mark the passive item as processed (`done').
savePassive
  :: (Ord t, Ord n)
  => Passive n t
  -> ExtWeight n t
  -> Auto n t
  -> Chart n t
  -> Chart n t
savePassive p ts _auto chart =
  chart
  { donePassive = newDone
  }
  where
    newDone = M.insertWith joinExtWeight' p ts (donePassive chart)


-- | Check if, for the given active item, the given transitions are already
-- present in the hypergraph.
hasPassiveTrav
  :: (Ord t, Ord n)
  => Passive n t
  -> S.Set (Trav n t)
  -> Auto n t
  -> Chart n t
  -> Bool
hasPassiveTrav p travSet auto chart =
  case passiveTrav p auto chart of
    Just ExtWeight{..} -> travSet `S.isSubsetOf` prioTrav
    Nothing -> False


---------------------------------
-- Extraction of Processed Items
---------------------------------


-- | Check whether the given passive item is final or not.
isFinal
  :: (Ord n)
  => S.Set n       -- ^ Accepted start symbols
  -> Int           -- ^ The length of the input sentence
  -> Auto n t      -- ^ The underlying Earley automaton
  -> Passive n t   -- ^ The item to check
  -> Bool
isFinal startSet n auto p =
  p ^. spanP ^. beg == 0 &&
  p ^. spanP ^. end == n &&
  S.null (p ^. spanP ^. gaps) &&
  not (isSister' did dag) &&
  DAG.isRoot did dag && checkStart
    (S.fromList . maybeToList $ getLabel did)
  where
    -- dag = Auto.gramDAG $ A.automat hype
    dag = gramDAG auto
    did = p ^. dagID
    getLabel did = labNonTerm =<< DAG.label did dag
    checkStart labelSet
      | S.null startSet = True
      | otherwise = (not . S.null) (labelSet `S.intersection` startSet)


-- | Return the list of final, initial, passive chart items.
--
-- TODO: relate to `isFinal`?
--
finalFrom
    :: (Ord n, Eq t)
    => S.Set n      -- ^ Accepted start symbols (if empty, no constraint)
    -> Int          -- ^ The length of the input sentence
    -> Auto n t     -- ^ The underlying Earley yautomaton
    -> Chart n t    -- ^ Result of the earley computation
    -> [Passive n t]
finalFrom startSet n auto Chart{..} = do
  (p, _) <- M.toList donePassive
  guard $ isFinal startSet n auto p
  return p
--   guard $ DAG.isRoot (p ^. dagID) dag
--   nt <- maybeToList $ getLabel (p ^. dagID)
--   guard $ nt `S.member` startSet
--   guard $ noGaps (p ^. spanP)
--   guard . not $ isSister' (p ^. dagID) dag
--   return p
--   where
--     dag = gramDAG auto
--     getLabel did = labNonTerm =<< DAG.label did dag


-- | Return all active processed items which:
-- * expect the given DAG node,
-- * end on the given position.
-- Return the weights (`priWeight`s) of reaching them as well.
expectEnd
    :: (HOrd n, HOrd t, MS.MonadState s m)
    => (s -> Auto n t)
    -> (s -> Chart n t)
    -> DAG.DID
    -> Pos
    -> P.ListT m (Active n, DuoWeight)
expectEnd getAuto getChart did i = do
  compState <- lift MS.get
  let Chart{..} = getChart compState
      automat = getAuto compState
  -- loop over all active items
  (p, e) <- each $ M.toList doneActive
  -- verify the item ends where it should
  guard $ p ^. spanA ^. end == i
  -- determine automaton states from which the given label
  -- leaves as a body transition
  stateSet <- some $ M.lookup did (withBody automat)
  -- verify the state of the active item
  guard $ p ^. state `S.member` stateSet
  -- return the item
  return (p, duoWeight e)


-- | Return all passive items with:
-- * the given non-terminal value
-- * the given span
-- TODO: It should not be called `rootSpan`, it returns also non-root items!
rootSpan
    :: (Ord n, MS.MonadState s m)
    => (s -> Auto n t)
    -> (s -> Chart n t)
    -> n -> (Pos, Pos)
    -> P.ListT m (Passive n t, DuoWeight)
rootSpan getAuto getChart x (i, j) = do
  compState <- lift MS.get
  let Chart{..} = getChart compState
      auto = getAuto compState
      dag = gramDAG auto
  -- loop over all passive items
  (p, e) <- each $ M.toList donePassive
  -- check the necessary constraints
  guard $ p ^. spanP ^. beg == i
  guard $ p ^. spanP ^. end == j
  -- NOTE: verifying `DAG.isRoot` would make this work incorrectly w.r.t. how
  -- the function is used in the AStar module.
  -- guard $ DAG.isRoot (p ^. dagID) dag
  guard $ nonTerm (p ^. dagID) auto == x
  -- return the item
  return (p, duoWeight e)

-- rootSpan getChart x (i, j) = do
--   Chart{..} <- getChart <$> lift MS.get
--   P.Select $ do
--     P.each $ case M.lookup i donePassiveIni >>= M.lookup x >>= M.lookup j of
--       Nothing -> []
--       Just m -> map (Arr.second duoWeight) (M.toList m)
--                  -- ((M.elems >=> M.toList) m)
--     P.each $ case M.lookup i donePassiveAuxNoTop >>= M.lookup x >>= M.lookup j of
--       Nothing -> []
--       Just m -> map (Arr.second duoWeight) (M.toList m)


-- | Return all active processed items which:
-- * has the given LHS non-terminal,
-- * end on the given position.
rootEnd
    :: (Ord n, Ord t, MS.MonadState s m)
    => (s -> Auto n t)
    -> (s -> Chart n t)
    -> n
    -> Pos
    -> P.ListT m (Active n, DuoWeight)
rootEnd getAuto getChart lhsNT i = do
  compState <- lift MS.get
  let Chart{..} = getChart compState
      auto = getAuto compState
      -- dag = gramDAG auto
  -- loop over each active item
  (q, e) <- each $ M.toList doneActive
  -- determine the LHS non-terminal
  let lhs = lhsNonTerm auto M.! (q ^. state)
  -- check the necessary constraints
  guard $ notFootLabel lhs == lhsNT
  guard $ q ^. spanA ^. end == i
  -- return the item
  return (q, duoWeight e)
--   where
--     getLabel dag did = labNonTerm =<< DAG.label did dag


-- -- | Return all active processed items which:
-- -- * has the given LHS non-terminal,
-- -- * end on the given position.
-- rootEnd
--     :: (Ord n, Ord t, MS.MonadState s m)
--     => (s -> Chart n t)
--     -> n
--     -> Pos
--     -> P.ListT m (Active n, DuoWeight)
-- rootEnd getChart lhsNT i = undefined
-- -- rootEnd getChart lhsNT i = do
-- --     compState <- lift MS.get
-- --     let ch@Chart{..} = getChart compState
-- --     doneSet <- some $ M.lookup (i, lhsNT) doneActiveByRoot
-- --     each $ do
-- --       q <- S.toList doneSet
-- --       e <- maybeToList (activeTrav q ch)
-- --       return (q, duoWeight e)


-- | NEW 20.02.202: Return all passive items which:
-- * provide a given DAG label,
-- * begin on the given position.
provideBeg
    :: (Ord n, Ord t, MS.MonadState s m)
    => (s -> Auto n t)
    -> (s -> Chart n t)
    -> DAG.DID -> Pos
    -> P.ListT m (Passive n t, DuoWeight)
provideBeg _getAuto getChart x i = do
  compState <- lift MS.get
  let Chart{..} = getChart compState
  -- loop over each passive item
  (p, e) <- each $ M.toList donePassive
  -- check the necessary constraints
  guard $ p ^. dagID == x
  guard $ p ^. spanP ^. beg == i
  -- return the item
  return (p, duoWeight e)



-- | Return all processed items which:
-- * are fully matched (i.e. passive)
-- * provide a label with a given non-terminal,
-- * begin on the given position,
--
-- (Note the similarity to `provideBeg`)
--
-- TODO: The result is not top-level auxiliary.
-- See `tryAdjoinInit'` and `tryAdjoinInit`.
-- TODO: Remove the todo above.
provideBeg'
    :: (Ord n, Ord t, MS.MonadState s m)
    => (s -> Chart n t)
    -> n -> Pos
    -> P.ListT m (Passive n t, DuoWeight)
provideBeg' getChart x i = undefined
-- provideBeg' getChart x i = do
--     Chart{..} <- getChart <$> lift MS.get
--     P.Select $ do
--       P.each $ case M.lookup i donePassiveIni >>= M.lookup x of
--         Nothing -> []
--         Just m ->
--           map
--             (Arr.second duoWeight)
--             ((M.elems >=> M.toList) m)
--             -- ((M.elems >=> M.elems >=> M.toList) m)
--       P.each $ case M.lookup i donePassiveAuxNoTop >>= M.lookup x of
--         Nothing -> []
--         Just m ->
--           map
--             (Arr.second duoWeight)
--             ((M.elems >=> M.toList) m)


-- | Return all passive items which:
-- * provide a given label,
-- * begin on the given position.
--
-- TODO: Should be better optimized.
provideBegIni
    :: (Ord n, Ord t, MS.MonadState s m)
    => (s -> Auto n t)
    -> (s -> Chart n t)
    -> n
    -> Pos
    -> P.ListT m (Passive n t, DuoWeight)
provideBegIni getAuto getChart x i = do
  compState <- lift MS.get
  let Chart{..} = getChart compState
      auto = getAuto compState
      dag = gramDAG auto
  -- loop over each passive item
  (p, e) <- each $ M.toList donePassive
  -- check the necessary constraints
  guard $ nonTerm (p ^. dagID) auto == x
  guard $ p ^. spanP ^. beg == i
  -- return the item
  return (p, duoWeight e)


-- -- | Return all initial passive items which:
-- -- * provide a given label,
-- -- * begin on the given position.
-- --
-- -- TODO: Should be better optimized.
-- provideBegIni
--     :: (Ord n, Ord t, MS.MonadState s m)
--     => (s -> Auto n t)
--     -> (s -> Chart n t)
--     -- -> Either (NotFoot n) DAG.DID
--     -> Either n DAG.DID
--     -- -> DAG.DID
--     -> Pos
--     -> P.ListT m (Passive n t, DuoWeight)
-- provideBegIni getAuto getChart x i = do
--   compState <- lift MS.get
--   let Chart{..} = getChart compState
--       auto = getAuto compState
--       dag = gramDAG auto
--       n = 
--         case x of
--           Left nt -> nt
--           Right did -> nonTerm did auto
--       checkNonTerm qDID =
--         case x of
--           Left nt -> nonTerm qDID auto == nt
--           Right did -> qDID == did
--   each $
--     maybeToList ((M.lookup i >=> M.lookup n) donePassiveIni) >>=
--     M.elems >>=
--     -- maybeToList . M.lookup x >>=
--       ( \m -> do
--           p <-
--             [ (q, duoWeight e)
--             | (q, e) <- M.toList m
--             -- , q ^. dagID == x ]
--             , checkNonTerm $ q ^. dagID ]
--           return p )


-- | Return all initial passive items which:
-- * provide a given label,
-- * begin on the given position.
--
-- TODO: Should be better optimized.
provideBegIni'
    :: (Ord n, Ord t, MS.MonadState s m)
    => (s -> Auto n t)
    -> (s -> Chart n t)
    -> Either (NotFoot n) DAG.DID
    -> Pos
    -> P.ListT m (Passive n t, DuoWeight)
provideBegIni' getAuto getChart x i = do
  compState <- lift MS.get
  let Chart{..} = getChart compState
      auto = getAuto compState
      dag = gramDAG auto
      label did =
        case DAG.label did dag >>= labNonTerm of
          Just x -> x
          Nothing -> error "AStar.Chart.provideBegIni': unknown DID"
      labNonTerm (O.Sister y) = Just $ NotFoot
        { notFootLabel = y
        , isSister = True }
      labNonTerm (O.NonTerm y) = Just $ NotFoot
        { notFootLabel = y
        , isSister = False }
      labNonTerm (O.DNode y) = Just $ NotFoot
        { notFootLabel = y
        , isSister = False }
      labNonTerm _ = Nothing
--       n =
--         case x of
--           Left nt -> notFootLabel nt
--           Right did -> nonTerm did auto
      checkNonTerm qDID =
        case x of
          -- Left nt -> nonTerm qDID auto == nt
          Left nt -> label qDID == nt
          Right did -> qDID == did
  -- loop over each passive item
  (q, e) <- each $ M.toList donePassive
  -- check the necessary constraints
  guard . checkNonTerm $ q ^. dagID
  guard $ q ^. spanP ^. beg == i
  -- return the item
  return (q, duoWeight e)


-- | Return all passive items with:
-- * the given non-terminal value
-- * the given span
withGap
    :: (Ord n, MS.MonadState s m)
    => (s -> Auto n t)
    -> (s -> Chart n t)
    -> (Pos, Pos, n)
    -> P.ListT m (Passive n t, DuoWeight)
withGap getAuto getChart gap = do
  compState <- lift MS.get
  let Chart{..} = getChart compState
  -- loop over each passive item
  (p, e) <- each $ M.toList donePassive
  -- check the necessary constraints
  guard $ gap `S.member` (p ^. spanP ^. gaps)
  -- return the item
  return (p, duoWeight e)


-- | Return all auxiliary passive items which:
-- * provide a given DAG label,
-- * begin on the given position.
--
-- TODO: Should be optimized.
provideBegAux
    :: (Ord n, Ord t, MS.MonadState s m)
    => (s -> Auto n t)
    -> (s -> Chart n t)
    -> DAG.DID -> Pos
    -> P.ListT m (Passive n t, DuoWeight)
provideBegAux getAuto getChart x i = undefined
-- provideBegAux getAuto getChart x i = do
--   compState <- lift MS.get
--   let Chart{..} = getChart compState
--       auto = getAuto compState
--       -- n = nonTerm (Right x) auto
--       n = nonTerm x auto
--   each $ case M.lookup i donePassiveAuxNoTop >>= M.lookup n of
--     Nothing -> []
--     Just m ->
--       [ (q, duoWeight e)
--       | (q, e) <- (M.elems >=> M.toList) m
--       -- , q ^. dagID == Right x ]
--       , q ^. dagID == x ]


-- | Return all fully parsed items:
-- * top-level and representing auxiliary trees,
-- * modifying the given source non-terminal,
-- * with the given gap.
auxModifyGap
    :: (Ord n, MS.MonadState s m)
    => (s -> Chart n t)
    -> n -> (Pos, Pos)
    -> P.ListT m (Passive n t, DuoWeight)
auxModifyGap getChart x (i, j) = undefined
-- auxModifyGap getChart x (i, j) = do
--     Chart{..} <- getChart <$> lift MS.get
--     each $ case (M.lookup i >=> M.lookup x >=> M.lookup j) donePassiveAuxTop of
--         Nothing -> []
--         Just m -> -- map (Arr.second priWeight) (M.toList m)
--           [ (p, duoWeight e)
--           | (p, e) <- M.toList m ]


-------------------------------------------------
-- 4-key map operations
--------------------------------------------------


-- -- | Lookup a 4-element key in the map.
-- lookup4
--   :: (Ord a, Ord b, Ord c, Ord d)
--   => a -> b -> c -> d
--   -> M.Map a (M.Map b (M.Map c (M.Map d e)))
--   -> Maybe e
-- lookup4 x y z p =
--   M.lookup x >=>
--   M.lookup y >=>
--   M.lookup z >=>
--   M.lookup p
-- 
-- 
-- -- | Insert a 4-element key and the corresponding value in the map.
-- -- Use the combining function if value already present in the map.
-- insertWith4
--   :: (Ord a, Ord b, Ord c, Ord d)
--   => (e -> e -> e)
--   -> a -> b -> c -> d -> e
--   -> M.Map a (M.Map b (M.Map c (M.Map d e)))
--   -> M.Map a (M.Map b (M.Map c (M.Map d e)))
-- insertWith4 f x y z p q =
--   M.insertWith
--     ( M.unionWith
--       ( M.unionWith
--         ( M.unionWith f )
--       )
--     )
--     x
--     ( M.singleton
--       y
--       ( M.singleton
--         z
--         ( M.singleton p q )
--       )
--     )


--------------------------------------------------
-- Utils
--------------------------------------------------


-- | Sum up traversals.
sumTrav :: [(a, ExtWeight n t)] -> Int
sumTrav xs = sum
    [ S.size (prioTrav ext)
    | (_, ext) <- xs ]


-- | ListT from a maybe.
some :: Monad m => Maybe a -> P.ListT m a
some = each . maybeToList


-- | ListT from a list.
each :: Monad m => [a] -> P.ListT m a
each = P.Select . P.each
