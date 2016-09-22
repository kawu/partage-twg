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
  , finalFrom
  , expectEnd
  , rootSpan
  , provideBeg'
  , provideBegIni
  , provideBegAux
  , auxModifyGap
)
where


import           Control.Monad               ((>=>))
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
-- import qualified NLP.Partage.Tree.Other      as O

import           NLP.Partage.AStar.Auto      (Auto (..))
import           NLP.Partage.AStar.Base
import           NLP.Partage.AStar.ExtWeight
import           NLP.Partage.AStar.Item


-- | A hypergraph dynamically constructed during parsing.
data Chart n t = Chart
    {

      -- , doneActive  :: M.Map (ID, Pos) (S.Set (Active n t))
      doneActive  :: M.Map Pos (M.Map ID
        -- (M.Map Active (S.Set (Trav n t))))
        (M.Map Active (ExtWeight n t)))
    -- ^ Processed active items partitioned w.r.t ending
    -- positions and state IDs.

    -- , donePassive :: M.Map (Pos, n, Pos)
    --    (M.Map (Passive n t) (ExtWeight n t))
    , donePassiveIni ::
        M.Map Pos         -- beginning position
        ( M.Map n         -- non-terminal
          ( M.Map Pos     -- ending position
            -- ( M.Map (Either n DID)   -- full non-terminal label
              ( M.Map (Passive n t) (ExtWeight n t) )
            -- )
          )
        )
    -- ^ Processed initial passive items.

    , donePassiveAuxNoTop ::
        M.Map Pos         -- beginning position
        ( M.Map n         -- non-terminal
          ( M.Map Pos     -- ending position
            ( M.Map (Passive n t) (ExtWeight n t) )
          )
        )
    -- ^ Processed auxiliary top-level passive items.

    , donePassiveAuxTop ::
        M.Map Pos         -- gap starting position
        ( M.Map n         -- non-terminal
          ( M.Map Pos     -- gap ending position
            ( M.Map (Passive n t) (ExtWeight n t) )
          )
        )
    -- ^ Processed auxiliary passive items.
    }


-- | Create an empty chart.
empty :: Chart n t
empty = Chart
    { doneActive  = M.empty
    , donePassiveIni = M.empty
    , donePassiveAuxTop = M.empty
    , donePassiveAuxNoTop = M.empty }



--------------------------------------------------
-- Chart Stats
--------------------------------------------------


-- | List all passive done items together with the corresponding
-- traversals.
listPassive :: Chart n t -> [(Passive n t, ExtWeight n t)]
listPassive hype =
  list (donePassiveIni hype) ++
  list (donePassiveAuxTop hype) ++
  list (donePassiveAuxNoTop hype)
  where list = M.elems >=> M.elems >=> M.elems >=> M.toList


-- | List all active done items together with the corresponding
-- traversals.
listActive :: Chart n t -> [(Active, ExtWeight n t)]
listActive = (M.elems >=> M.elems >=> M.toList) . doneActive


-- | Number of passive (done) nodes in the parsing hypergraph.
doneNodesNum :: Chart n t -> Int
doneNodesNum e
    = length (listPassive e)
    + length (listActive e)


-- | Number of edges outgoing from done nodes in the underlying hypergraph.
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
    => Active -> Chart n t
    -> Maybe (ExtWeight n t)
activeTrav p
    = (   M.lookup (p ^. spanA ^. end)
      >=> M.lookup (p ^. state)
      >=> M.lookup p )
    . doneActive


-- | Check if the active item is not already processed.
isProcessedA :: (Ord n, Ord t) => Active -> Chart n t -> Bool
isProcessedA p =
    check . activeTrav p
  where
    check (Just _) = True
    check _        = False


-- | Mark the active item as processed (`done').
saveActive
    :: (Ord t, Ord n)
    => Active
    -> ExtWeight n t
    -> Chart n t
    -> Chart n t
saveActive p ts chart =
    chart {doneActive = newDone}
  where
    newDone =
        M.insertWith
            ( M.unionWith
                ( M.unionWith joinExtWeight' ) )
            ( p ^. spanA ^. end )
            ( M.singleton (p ^. state)
                ( M.singleton p ts ) )
            ( doneActive chart )


-- | Check if, for the given active item, the given transitions are already
-- present in the hypergraph.
hasActiveTrav
    :: (Ord t, Ord n)
    => Active
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
passiveTrav p auto hype
  | regular (p ^. spanP) = lookup4
      (p ^. spanP ^. beg)
      (nonTerm (p ^. dagID) auto)
      (p ^. spanP ^. end)
      -- (p ^. dagID)
      p (donePassiveIni hype)
--     M.lookup (p ^. spanP ^. beg) (donePassiveIni hype) >>=
--     M.lookup (nonTerm (p ^. dagID) hype) >>=
--     M.lookup (p ^. spanP ^. end) >>=
--     M.lookup p
  | isRoot (p ^. dagID) = lookup4
      (fst . fromJust $ p ^. spanP ^. gap)
      (nonTerm (p ^. dagID) auto)
      (snd . fromJust $ p ^. spanP ^. gap)
      p (donePassiveAuxTop hype)
--     M.lookup (fst . fromJust $ p ^. spanP ^. gap) (donePassiveAuxTop hype) >>=
--     M.lookup (nonTerm (p ^. dagID) hype) >>=
--     M.lookup (snd . fromJust $ p ^. spanP ^. gap) >>=
--     M.lookup p
  | otherwise = lookup4
      (p ^. spanP ^. beg)
      (nonTerm (p ^. dagID) auto)
      (p ^. spanP ^. end)
      p (donePassiveAuxNoTop hype)
--     M.lookup (p ^. spanP ^. beg) (donePassiveAuxNoTop hype) >>=
--     M.lookup (nonTerm (p ^. dagID) hype) >>=
--     M.lookup (p ^. spanP ^. end) >>=
--     M.lookup p


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
savePassive p ts auto chart
  | regular (p ^. spanP) =
      let newDone =
           insertWith4 joinExtWeight'
             (p ^. spanP ^. beg)
             (nonTerm (p ^. dagID) auto)
             (p ^. spanP ^. end)
             -- (p ^. dagID)
             p ts (donePassiveIni chart)
      in chart {donePassiveIni = newDone}
  | isRoot (p ^. dagID) =
       let newDone =
             insertWith4 joinExtWeight'
               (fst . fromJust $ p ^. spanP ^. gap)
               (nonTerm (p ^. dagID) auto)
               (snd . fromJust $ p ^. spanP ^. gap)
               p ts (donePassiveAuxTop chart)
       in chart {donePassiveAuxTop = newDone}
  | otherwise =
       let newDone =
             insertWith4 joinExtWeight'
               (p ^. spanP ^. beg)
               (nonTerm (p ^. dagID) auto)
               (p ^. spanP ^. end)
               p ts (donePassiveAuxNoTop chart)
       in chart {donePassiveAuxNoTop = newDone}


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


-- | Return the list of final, initial, passive chart items.
finalFrom
    :: (Ord n, Eq t)
    => n            -- ^ The start symbol
    -> Int          -- ^ The length of the input sentence
    -> Chart n t    -- ^ Result of the earley computation
    -> [Passive n t]
finalFrom start n Chart{..} =
    case M.lookup 0 donePassiveIni >>= M.lookup start >>= M.lookup n of
        Nothing -> []
        Just m ->
            [ p
            | p <- M.keys m
            , p ^. dagID == Left start ]


-- -- | Return all active processed items which:
-- -- * expect a given label,
-- -- * end on the given position.
-- expectEnd
--     :: (Ord n, Ord t) => Lab n t -> Pos
--     -> P.ListT (Earley n t) Active
-- expectEnd sym i = do
--     Hype{..} <- lift RWS.get
--     -- determine items which end on the given position
--     doneEnd <- some $ M.lookup i doneActive
--     -- determine automaton states from which the given label
--     -- leaves as a body transition
--     stateSet <- some $ M.lookup sym withBody
--     -- pick one of the states
--     stateID <- each $ S.toList stateSet
--     --
--     -- ALTERNATIVE: state <- each . S.toList $
--     --      stateSet `S.intersection` M.keySet doneEnd
--     --
--     -- determine items which refer to the chosen states
--     doneEndLab <- some $ M.lookup stateID doneEnd
--     -- return them all!
--     each $ M.keys doneEndLab


-- | Return all active processed items which:
-- * expect a given label,
-- * end on the given position.
-- Return the weights (`priWeight`s) of reaching them as well.
expectEnd
    :: (HOrd n, HOrd t, MS.MonadState s m)
    => (s -> Auto n t)
    -> (s -> Chart n t)
    -> DAG.DID
    -> Pos
    -> P.ListT m (Active, DuoWeight)
expectEnd getAuto getChart did i = do
  compState <- lift MS.get
  let Chart{..} = getChart compState
      automat = getAuto compState
  -- determine items which end on the given position
  doneEnd <- some $ M.lookup i doneActive
  -- determine automaton states from which the given label
  -- leaves as a body transition
  stateSet <- some $ M.lookup did (withBody automat)
  -- pick one of the states
  stateID <- each . S.toList $
       stateSet `S.intersection` M.keysSet doneEnd
  -- determine items which refer to the chosen states
  doneEndLab <- some $ M.lookup stateID doneEnd
  -- return them all!
  -- each $ M.keys doneEndLab
  each $
    [ (p, duoWeight e)
    | (p, e) <- M.toList doneEndLab ]


-- | Return all passive items with:
-- * the given root non-terminal value (but not top-level auxiliary)
-- * the given span
rootSpan
    :: (Ord n, MS.MonadState s m)
    => (s -> Chart n t)
    -> n -> (Pos, Pos)
    -> P.ListT m (Passive n t, DuoWeight)
rootSpan getChart x (i, j) = do
  Chart{..} <- getChart <$> lift MS.get
  P.Select $ do
    P.each $ case M.lookup i donePassiveIni >>= M.lookup x >>= M.lookup j of
      Nothing -> []
      Just m -> map (Arr.second duoWeight) (M.toList m)
                 -- ((M.elems >=> M.toList) m)
    P.each $ case M.lookup i donePassiveAuxNoTop >>= M.lookup x >>= M.lookup j of
      Nothing -> []
      Just m -> map (Arr.second duoWeight) (M.toList m)


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
provideBeg' getChart x i = do
    Chart{..} <- getChart <$> lift MS.get
    P.Select $ do
      P.each $ case M.lookup i donePassiveIni >>= M.lookup x of
        Nothing -> []
        Just m ->
          map
            (Arr.second duoWeight)
            ((M.elems >=> M.toList) m)
            -- ((M.elems >=> M.elems >=> M.toList) m)
      P.each $ case M.lookup i donePassiveAuxNoTop >>= M.lookup x of
        Nothing -> []
        Just m ->
          map
            (Arr.second duoWeight)
            ((M.elems >=> M.toList) m)


-- | Return all initial passive items which:
-- * provide a given label,
-- * begin on the given position.
--
-- TODO: Should be better optimized.
provideBegIni
    :: (Ord n, Ord t, MS.MonadState s m)
    => (s -> Auto n t)
    -> (s -> Chart n t)
    -> Either n DAG.DID -> Pos
    -> P.ListT m (Passive n t, DuoWeight)
provideBegIni getAuto getChart x i = do
  compState <- lift MS.get
  let Chart{..} = getChart compState
      auto = getAuto compState
      n = nonTerm x auto
  each $
    maybeToList ((M.lookup i >=> M.lookup n) donePassiveIni) >>=
    M.elems >>=
    -- maybeToList . M.lookup x >>=
      ( \m -> do
          p <-
            [ (q, duoWeight e)
            | (q, e) <- M.toList m
            , q ^. dagID == x ]
          return p )


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
provideBegAux getAuto getChart x i = do
  compState <- lift MS.get
  let Chart{..} = getChart compState
      auto = getAuto compState
      n = nonTerm (Right x) auto
  each $ case M.lookup i donePassiveAuxNoTop >>= M.lookup n of
    Nothing -> []
    Just m ->
      [ (q, duoWeight e)
      | (q, e) <- (M.elems >=> M.toList) m
      , q ^. dagID == Right x ]


-- | Return all fully parsed items:
-- * top-level and representing auxiliary trees,
-- * modifying the given source non-terminal,
-- * with the given gap.
auxModifyGap
    :: (Ord n, MS.MonadState s m)
    => (s -> Chart n t)
    -> n -> (Pos, Pos)
    -> P.ListT m (Passive n t, DuoWeight)
auxModifyGap getChart x (i, j) = do
    Chart{..} <- getChart <$> lift MS.get
    each $ case (M.lookup i >=> M.lookup x >=> M.lookup j) donePassiveAuxTop of
        Nothing -> []
        Just m -> -- map (Arr.second priWeight) (M.toList m)
          [ (p, duoWeight e)
          | (p, e) <- M.toList m ]
--     hype <- lift RWS.get
--     each
--         [ (q, priWeight e) | (q, e) <- listPassive hype
--         , q ^. spanP ^. gap == Just gapSpan
--         , q ^. dagID == Left x ]


-------------------------------------------------
-- 4-key map operations
--------------------------------------------------


-- | Lookup a 4-element key in the map.
lookup4
  :: (Ord a, Ord b, Ord c, Ord d)
  => a -> b -> c -> d
  -> M.Map a (M.Map b (M.Map c (M.Map d e)))
  -> Maybe e
lookup4 x y z p =
  M.lookup x >=>
  M.lookup y >=>
  M.lookup z >=>
  M.lookup p


-- | Insert a 4-element key and the corresponding value in the map.
-- Use the combining function if value already present in the map.
insertWith4
  :: (Ord a, Ord b, Ord c, Ord d)
  => (e -> e -> e)
  -> a -> b -> c -> d -> e
  -> M.Map a (M.Map b (M.Map c (M.Map d e)))
  -> M.Map a (M.Map b (M.Map c (M.Map d e)))
insertWith4 f x y z p q =
  M.insertWith
    ( M.unionWith
      ( M.unionWith
        ( M.unionWith f )
      )
    )
    x
    ( M.singleton
      y
      ( M.singleton
        z
        ( M.singleton p q )
      )
    )


-- -- | Lookup a 5-element key in the map.
-- lookup5
--   :: (Ord a, Ord b, Ord c, Ord d, Ord e)
--   => a -> b -> c -> d -> e
--   -> M.Map a (M.Map b (M.Map c (M.Map d (M.Map e f))))
--   -> Maybe f
-- lookup5 x y z w p =
--   M.lookup x >=>
--   M.lookup y >=>
--   M.lookup z >=>
--   M.lookup w >=>
--   M.lookup p


-- -- | Insert a 5-element key and the corresponding value in the map.
-- -- Use the combining function if value already present in the map.
-- insertWith5
--   :: (Ord a, Ord b, Ord c, Ord d, Ord e)
--   => (f -> f -> f)
--   -> a -> b -> c -> d -> e -> f
--   -> M.Map a (M.Map b (M.Map c (M.Map d (M.Map e f))))
--   -> M.Map a (M.Map b (M.Map c (M.Map d (M.Map e f))))
-- insertWith5 f x y z w p q =
--   M.insertWith
--     ( M.unionWith
--       ( M.unionWith
--         ( M.unionWith
--           ( M.unionWith f )
--         )
--       )
--     )
--     x
--     ( M.singleton
--       y
--       ( M.singleton
--         z
--         ( M.singleton
--           w
--           ( M.singleton p q )
--         )
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


-- -- -- | Take the non-terminal of the underlying DAG node.
-- -- nonTerm :: Either n DID -> Hype n t -> n
-- -- nonTerm i =
-- --     check . nonTerm' i . gramDAG . automat
-- --   where
-- --     check Nothing  = error "nonTerm: not a non-terminal ID"
-- --     check (Just x) = x
-- 
-- 
-- -- | Take the non-terminal of the underlying DAG node.
-- nonTerm :: Either n DAG.DID -> Auto n t -> n
-- nonTerm i =
--     check . nonTerm' i . gramDAG
--   where
--     check Nothing  = error "nonTerm: not a non-terminal ID"
--     check (Just x) = x
-- 
-- 
-- -- | Take the non-terminal of the underlying DAG node.
-- nonTerm' :: Either n DAG.DID -> DAG.DAG (O.Node n t) w -> Maybe n
-- nonTerm' i dag = case i of
--     Left rootNT -> Just rootNT
--     Right did   -> labNonTerm =<< DAG.label did dag
--     -- Right did   -> labNonTerm . DAG.label did -- . gramDAG . automat
-- 
-- 
-- -- | Take the non-terminal of the underlying DAG node.
-- labNonTerm :: O.Node n t -> Maybe n
-- labNonTerm (O.NonTerm y) = Just y
-- labNonTerm (O.Foot y) = Just y
-- labNonTerm _ = Nothing


-- | ListT from a maybe.
some :: Monad m => Maybe a -> P.ListT m a
some = each . maybeToList


-- | ListT from a list.
each :: Monad m => [a] -> P.ListT m a
each = P.Select . P.each