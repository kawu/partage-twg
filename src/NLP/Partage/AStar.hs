{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}


-- | A* Earley-style TAG parsing based on automata, with a distinction
-- between active and passive items.


module NLP.Partage.AStar
(
-- * Earley-style parsing
-- ** Input
  Input (..)
, Tok (..)
, fromList
-- , fromSets

-- ** From a factorized grammar
-- , recognize
, recognizeFrom
-- , parse
-- , earley
-- ** With automata precompiled
-- , recognizeAuto

, recognizeFromAuto
-- , parseAuto
, earleyAuto
, earleyAutoP
, earleyAutoGen
-- ** Automaton
, Auto
, mkAuto

-- * Parsing trace (hypergraph)
, Hype (..)
, Item (..)
, Passive (..)
, dagID
, spanP
, Active (..)
, Span (..)
, beg
, end
, gaps
, ExtWeight (priWeight, gapWeight, estWeight)
, totalWeight
, HypeModif (..)
, ModifType (..)
-- ** Extracting parsed trees
, parsedTrees
, fromPassive
, fromActive

-- -- ** Extracting derivation trees
-- , Deriv
-- , DerivNode (..)
-- , derivTrees
-- , derivFromPassive
-- -- , deriv2tree
-- -- , expandDeriv
-- -- -- , fromPassive'
-- -- -- , fromActive'
-- ** Stats
, hyperNodesNum
, hyperEdgesNum
, doneNodesNum
, doneEdgesNum
, waitingNodesNum
, waitingEdgesNum
-- -- ** Printing
-- , printHype

-- * Sentence position
, Pos

-- * Internal (should not be exported here?)
, Trav (..)
, activeTrav
, passiveTrav
, prioTrav
, nonTerm
, finalFrom
-- , isRoot

-- * Provisional
, Earley
, mkHype

-- #ifdef DebugOn
, printItem
-- #endif
) where


import           Prelude hiding             (init, span, (.))
import           Control.Applicative        ((<$>))
import qualified Control.Arrow as Arr
import           Control.Monad      (guard, void, (>=>), when, mplus)
import           Control.Monad.Trans.Class  (lift)
-- import           Control.Monad.Trans.Maybe  (MaybeT (..))
import qualified Control.Monad.RWS.Strict   as RWS
import qualified Control.Monad.State.Strict as State
import           Control.Category ((>>>), (.))

import           Data.Function              (on)
import           Data.Maybe     ( isJust, isNothing, mapMaybe
                                , maybeToList, fromJust )
import qualified Data.Map.Strict            as M
-- import           Data.Ord       ( comparing )
-- import           Data.List      ( sortBy )
import qualified Data.Set                   as S
import qualified Data.PSQueue               as Q
import           Data.PSQueue (Binding(..))
import           Data.Lens.Light
-- import qualified Data.Vector                as V
-- import           Data.Hashable (Hashable)
-- import qualified Data.HashTable.IO          as H
import qualified Data.MemoCombinators as Memo

import qualified Pipes                      as P
import           Pipes                      ((>->))
import qualified Pipes.Prelude              as P

import           Data.DAWG.Ord (ID)

import           NLP.Partage.SOrd
import qualified NLP.Partage.Tree       as T
import qualified NLP.Partage.Tree.Other as O
import qualified NLP.Partage.Auto as A

import           NLP.Partage.DAG (DID, DAG, Weight)
import qualified NLP.Partage.DAG as DAG
import           NLP.Partage.AStar.Auto (Auto(..), mkAuto, NotFoot(..))
-- import qualified NLP.Partage.AStar.Heuristic.Base as H
-- import qualified NLP.Partage.AStar.Heuristic.Dummy as H
import qualified NLP.Partage.AStar.Heuristic as H

import           NLP.Partage.AStar.Base -- hiding (nonTerm)
import qualified NLP.Partage.AStar.Base as Base
import           NLP.Partage.AStar.Item hiding (printPassive)
import qualified NLP.Partage.AStar.Item as Item
import           NLP.Partage.AStar.ExtWeight
import qualified NLP.Partage.AStar.Chart as Chart


-- For debugging purposes
import           Control.Monad.IO.Class     (liftIO)
#ifdef DebugOn
import qualified Data.Time              as Time
#endif

import Debug.Trace (trace)


--------------------------------------------------
-- Item Type
--------------------------------------------------


-- | Passive or active item.
data Item n t
    = ItemP (Passive n t)
    | ItemA (Active n)
    deriving (Show, Eq, Ord)


-- #ifdef DebugOn
-- | Print a passive item.
printPassive :: (Show n, Show t) => Passive n t -> Hype n t -> IO ()
printPassive p hype = Item.printPassive p (automat hype)


-- | Print an active item.
printItem :: (Show n, Show t) => Item n t -> Hype n t -> IO ()
printItem (ItemP p) h = printPassive p h
printItem (ItemA p) _ = printActive p
-- #endif


--------------------------------------------------
-- Earley monad
--------------------------------------------------


-- | A hypergraph dynamically constructed during parsing.
data Hype n t = Hype
    { automat   :: Auto n t
    -- ^ The underlying automaton

    , chart :: Chart.Chart n t
    -- ^ The underlying chart

    , waiting     :: Q.PSQ (Item n t) (ExtWeight n t)
    -- ^ The underlying agenda
    }


-- | Make an initial `Hype` from a set of states.
mkHype
    :: (HOrd n, HOrd t)
    -- => H.Esti t
    => Auto n t
    -> Hype n t
mkHype auto = Hype
    { automat  = auto
    -- , estiCost = esti
    , chart = Chart.empty
    , waiting = Q.empty }


-- | Type of elements produced by the pipe underlying the `Earley` monad.
-- What is produced by the pipe represents all types of modifications which
-- can apply to the underlying, processed (done) part of the hypergraph.
-- TODO: No need to report `modifTrav` if `modifType == NewNode` (then
-- `modifTrav` can be easily induced from `modifHype`).
data HypeModif n t = HypeModif
  { modifHype :: Hype n t
    -- ^ Current version of the hypergraph, with the corresponding
    -- modification applied
  , modifType :: ModifType
    -- ^ Type of modification of the hypergraph
  , modifItem :: Item n t
    -- ^ Hypernode which is either added (if `modifType = NewNode`) or
    -- just the target (if `modifType = NewArcs`) of the newly added
    -- hyperarcs.
  , modifTrav :: ExtWeight n t
    -- ^ New arcs (if any) being added to the passive part of the hypergraph;
    -- IMPORTANT: this (extended) weight is guaranteed to be optimal only in
    -- case of the `NewNode` modifications. In case of the `NewArcs`
    -- modifications, `modifTrav` corresponds to the new traversal and thus
    -- the resulting `priWeight` value might be higher than beta (weight
    -- of the optimal inside derivation) which, by the way, is already
    -- computed and stored in the hypergraph.
  }


-- -- | Type of elements produced by the `EarleyPipe`.
-- -- What is produced by the pipe represents all types of modifications which
-- -- can apply to the underlying, processed (done) part of the hypergraph.
-- -- TODO: No need to report `modifTrav` if `modifType == NewNode` (then
-- -- `modifTrav` can be easily induced from `modifHype`).
-- data HypeModif n t = HypeModif
--   { modifType :: ModifType
--     -- ^ Type of modification of the hypergraph
--   , modifItem :: Item n t
--     -- ^ Hypernode which is either added (if `modifType = NewNode`) or
--     -- just the target (if `modifType = NewArcs`) of the newly added
--     -- hyperarcs.
--   , modifTrav :: ExtWeight n t
--     -- ^ New arcs (if any) being added to the passive part of the hypergraph;
--     -- IMPORTANT: this (extended) weight is guaranteed to be optimal only in
--     -- case of the `NewNode` modifications. In case of the `NewArcs`
--     -- modifications, `modifTrav` corresponds to the new traversal and thus
--     -- the resulting `priWeight` value might be higher than beta (weight
--     -- of the optimal inside derivation) which, by the way, is already
--     -- computed and stored in the hypergraph.
--   }


-- | Type of a modification of a hypergraph.  The modification corresponds
-- to the processed part of the hypergraph (i.e., it could have been already
-- present in the waiting queue).
data ModifType
  = NewNode
    -- ^ When a new node (and the corresponding in-going arcs) is added
  | NewArcs
    -- ^ When only new arcs, leading to an existig hypernode, are added
  deriving (Show, Eq, Ord)


-- | Earley parser monad.  Contains the input sentence (reader) and the state
-- of the computation `Hype'.
--
-- WARNING: The description below is obsolete and, most likely, not correct.
-- Now RWS is embedded in the producer.  Hopefully this will lead to
-- performance gains.  It also seems more intuitive!
--
-- OBSOLETE: Note that the producer is embedded in RWS. There are two reasons
-- for that: (i) this allows to easily treat RWS as a local state which can be
-- easily stripped down in subsequent pipe-based computations, and (ii) RWS
-- component is consulted much more often then the pipe component (it is not
-- clear, however, what are the performance gains stemming from this design
-- choice).
--
type EarleyPipe n t = P.Producer (HypeModif n t) (Earley n t)
type Earley n t = RWS.RWST (Input t) () (Hype n t) IO


-- | Yield `HypeModif` to the underlying pipe. The argument function will be
-- supplied with the current hypergraph, for convenience.
yieldModif
  :: (Hype n t -> HypeModif n t)
  -> EarleyPipe n t ()
yieldModif mkModif = do
  hype <- lift $ RWS.get
  P.yield . mkModif $ hype


-- | Read word from the given position of the input.
readInput :: Pos -> P.ListT (EarleyPipe n t) (Tok t)
readInput i = do
    -- ask for the input
    sent <- RWS.asks inputSent
    -- just a safe way to retrieve the i-th element
    each $ take 1 $ drop i sent
    -- xs <- some $ sent V.!? i
    -- each $ S.toList xs


--------------------------------------------------
-- Automaton-Based Primitives
--------------------------------------------------


-- | Follow the given terminal in the underlying automaton.
followTerm :: (Ord n, Ord t)
           => ID -> Maybe t -> P.ListT (EarleyPipe n t) (Weight, ID)
followTerm i c = do
    -- get the underlying automaton
    auto <- RWS.gets $ automat
    -- get the dag ID corresponding to the given terminal
    did  <- each . S.toList . maybe S.empty id $ M.lookup c (termDID auto)
    -- follow the label
    some $ A.followWei (gramAuto auto) i (A.Body did)


-- | Follow the given body transition in the underlying automaton.
-- It represents the transition function of the automaton.
--
-- TODO: merge with `followTerm`.
follow :: ID -> DID -> P.ListT (EarleyPipe n t) (Weight, ID)
follow i x = do
    -- get the underlying automaton
    auto <- RWS.gets $ gramAuto . automat
    -- follow the label
    some $ A.followWei auto i (A.Body x)


-- | Rule heads outgoing from the given automaton state.
heads :: ID -> P.ListT (EarleyPipe n t) (Weight, DID)
heads i = do
    auto <- RWS.gets $ gramAuto . automat
    let mayHead (x, w, _) = case x of
            A.Body _  -> Nothing
            A.Head y -> Just (w, y)
    each $ mapMaybe mayHead $ A.edgesWei auto i


-- | Rule body elements outgoing from the given automaton state.
elems :: ID -> P.ListT (EarleyPipe n t) (DID, Weight, ID)
elems i = do
    auto <- RWS.gets $ gramAuto . automat
    let mayBody (x, w, j) = case x of
            A.Body y -> Just (y, w, j)
            A.Head _ -> Nothing
    each $ mapMaybe mayBody $ A.edgesWei auto i


-- | Check if any element leaves the given state.
hasElems :: ID -> Earley n t Bool
hasElems i = do
    auto <- RWS.gets $ gramAuto . automat
    let mayBody (x, _, _) = case x of
            A.Body y  -> Just y
            A.Head _ -> Nothing
    return
        . not . null
        . mapMaybe mayBody
        $ A.edgesWei auto i


--------------------------------------------------
-- Hypergraph stats
--------------------------------------------------


-- | List all waiting items together with the corresponding
-- traversals.
listWaiting :: (Ord n, Ord t) => Hype n t -> [(Item n t, ExtWeight n t)]
listWaiting =
  let toPair (p :-> w) = (p, w)
   in map toPair . Q.toList . waiting


-- | Number of nodes in the parsing hypergraph.
doneNodesNum :: (Ord n, Ord t) => Hype n t -> Int
doneNodesNum e = Chart.doneNodesNum (chart e)


-- | Number of waiting nodes in the parsing hypergraph.
waitingNodesNum :: (Ord n, Ord t) => Hype n t -> Int
waitingNodesNum = length . listWaiting


-- | Number of nodes in the parsing hypergraph.
hyperNodesNum :: (Ord n, Ord t) => Hype n t -> Int
hyperNodesNum e = doneNodesNum e + waitingNodesNum e


-- | Number of nodes in the parsing hypergraph.
doneEdgesNum :: (Ord n, Ord t) => Hype n t -> Int
doneEdgesNum e = Chart.doneEdgesNum (chart e)


-- | Number of edges outgoing from waiting nodes in the underlying hypergraph.
waitingEdgesNum :: (Ord n, Ord t) => Hype n t -> Int
waitingEdgesNum = sumTrav . listWaiting


-- | Number of edges in the parsing hypergraph.
hyperEdgesNum :: (Ord n, Ord t) => Hype n t -> Int
hyperEdgesNum e = doneEdgesNum e + waitingEdgesNum e


-- | Sum up traversals.
sumTrav :: [(a, ExtWeight n t)] -> Int
sumTrav xs = sum
    [ S.size (prioTrav ext)
    | (_, ext) <- xs ]


--------------------
-- Active items
--------------------


-- | Check if the active item is not already processed.
isProcessedA :: (Ord n, Ord t) => Active n -> Earley n t Bool
isProcessedA p = Chart.isProcessedA p . chart <$> RWS.get


-- | Mark the active item as processed (`done').
saveActive
    :: (Ord t, Ord n)
    => Active n
    -> ExtWeight n t
    -> Earley n t ()
saveActive p ts =
  RWS.modify' $ \h ->
    let lhsMap = lhsNonTerm (automat h)
    in  h {chart = Chart.saveActive lhsMap p ts (chart h)}


-- | Check if, for the given active item, the given transitions are already
-- present in the hypergraph.
hasActiveTrav
    :: (Ord t, Ord n)
    => Active n
    -> S.Set (Trav n t)
    -> Earley n t Bool
hasActiveTrav p travSet =
  Chart.hasActiveTrav p travSet . chart <$> RWS.get


--------------------
-- Passive items
--------------------


-- | Check if the passive item is not already processed.
isProcessedP :: (Ord n, Ord t) => Passive n t -> Earley n t Bool
isProcessedP p = do
  h <- RWS.get
  return $ Chart.isProcessedP p (automat h) (chart h)


-- | Mark the passive item as processed (`done').
savePassive
    :: (Ord t, Ord n)
    => Passive n t
    -> ExtWeight n t
    -> Earley n t ()
savePassive p ts = RWS.state $
  \h ->
    let newChart = Chart.savePassive p ts (automat h) (chart h)
    in ((), h {chart = newChart})


-- | Check if, for the given active item, the given transitions are already
-- present in the hypergraph.
hasPassiveTrav
    :: (Ord t, Ord n)
    => Passive n t
    -> S.Set (Trav n t)
    -> Earley n t Bool
hasPassiveTrav p travSet = do
  h <- RWS.get
  return $ Chart.hasPassiveTrav p travSet (automat h) (chart h)


--------------------
-- Waiting Queue
--------------------


-- | Add the active item to the waiting queue.  Check first if it
-- is not already in the set of processed (`done') states.
pushActive :: (SOrd t, SOrd n)
           => Active n
           -- -> ExtWeight n t
           -> DuoWeight        -- ^ Weight of reaching the new item
           -> Maybe (Trav n t) -- ^ Traversal leading to the new item (if any)
           -> EarleyPipe n t ()
pushActive p newWeight newTrav = do
  estDist <- lift $ estimateDistA p
  let new = case newTrav of
        Just trav -> extWeight  newWeight estDist trav
        Nothing   -> extWeight0 newWeight estDist
  lift (track estDist >> isProcessedA p) >>= \case
    True -> do
      -- Below we make sure that the `newTrav` is not actually already
      -- in the processed part of the hypergraph.  Normally it should not
      -- happen, but currently it can because we abstract over the exact
      -- form of the passive item matched against a foot.  For the foot
      -- adjoin inference rule it matters, but not in the hypergraph.
      b <- lift $ hasActiveTrav p (prioTrav new)
      when (not b) $ do
        lift $ saveActive p new
        yieldModif $ \hype -> HypeModif
          { modifHype = hype
          , modifType = NewArcs
          , modifItem = ItemA p
          , modifTrav = new }
    False -> modify' $ \s -> s {waiting = newWait new (waiting s)}
  where
    newWait = Q.insertWith joinExtWeight (ItemA p)
#ifdef DebugOn
    track estWeight = liftIO $ do
        putStr ">A>  " >> printActive p
        putStr " :>  " >> print (newWeight, estWeight)
#else
    track _ = return ()
#endif


-- | Add the passive item to the waiting queue.  Check first if it
-- is not already in the set of processed (`done') states.
pushPassive :: (SOrd t, SOrd n)
            => Passive n t
            -> DuoWeight     -- ^ Weight of reaching the new item
            -> Trav n t      -- ^ Traversal leading to the new item
            -> EarleyPipe n t ()
pushPassive p newWeight0 newTrav0 = do

#ifdef HandleDummyArcWeight
  -- In case the item is final, we add the remaining costs (e.g., the cost
  -- of attaching the corresponding tree to the dummy root node)
  sentLen <- length <$> RWS.asks inputSent
  auto <- RWS.gets automat
  -- the extra cost to add
  extra <-
    if Chart.isFinal S.empty sentLen auto p
       then do
         -- NOTE: -1 is the position of the dummy root!  Not very elegant...
         cost <- omegaPos (p ^. dagID) (Just (-1))
         return $ case cost of
                    -- `Nothing` means that the dummy root is not allowed as
                    -- parent of the current item
                    Nothing -> read "Infinity"
                    Just x  -> x
       else return 0
  let newTrav = newTrav0
        { _weight = _weight newTrav0 + extra }
      newWeight = newWeight0
        { duoBeta = duoBeta newWeight0 + extra }
#else
  let newTrav = newTrav0
      newWeight = newWeight0
#endif

  -- TODO: do we have to compute the estimated distance if the node is already
  -- processed (done)?
  estDist <- lift $ estimateDistP p
  let new = extWeight newWeight estDist newTrav
  lift (track newWeight estDist >> isProcessedP p) >>= \case
    True -> do
--       hasPassiveTrav p (prioTrav new) >>= \case
--         False -> return ()
--         True -> error "pushPassive.NewArcs: arcs not new!"
      -- Below we make sure that `newTrav` is not actually already present in
      -- the processed part of the hypergraph. Normally it should not happen,
      -- but currently it can because we abstract over the exact form of the
      -- passive item matched against a foot. For the foot adjoin inference rule
      -- it matters, but not in the hypergraph.
      b <- lift $ hasPassiveTrav p (prioTrav new)
      when (not b) $ do
        lift $ savePassive p new
        yieldModif $ \hype -> HypeModif
          { modifHype = hype
          , modifType = NewArcs
          , modifItem = ItemP p
          , modifTrav = new }
    False -> modify' $ \s -> s {waiting = newWait new (waiting s)}
  where
    newWait = Q.insertWith joinExtWeight (ItemP p)
#ifdef DebugOn
    track newWeight estWeight = do
      hype <- RWS.get
      liftIO $ do
        putStr ">P>  " >> printPassive p hype
        putStr " :>  " >> print (newWeight, estWeight)
#else
    track _ _ = return ()
#endif


-- | Add to the waiting queue all items induced from the given item.
pushInduced
  :: (SOrd t, SOrd n)
  => Active n
  -> DuoWeight     -- ^ Weight of reaching the new item
  -> Trav n t      -- ^ Traversal leading to the new item
  -> EarleyPipe n t ()
pushInduced q newWeight newTrav = do
  pushActive q newWeight (Just newTrav)
--     dag <- RWS.gets (gramDAG . automat)
--     hasElems (getL state q) >>= \b ->
--       when b (pushActive q newWeight $ Just newTrav)
--     P.runListT $ do
--         (headCost, did) <- heads (getL state q)
--         let p = if not (DAG.isRoot did dag)
--                 then Passive (Right did) (getL spanA q)
--                 else check $ do
--                     x <- labNonTerm =<< DAG.label did dag
--                     return $ Passive (Left x) (getL spanA q)
--                 where check (Just x) = x
--                       check Nothing  = error "pushInduced: invalid DID"
--         -- estDist <- lift (estimateDistP p)
--         -- let ext  = new priWeight
--         -- let ext' = ext
--         --         { priWeight = priWeight new + headCost
--         --         , estWeight = estDist }
--         -- lift $ pushPassive p ext'
--         let finalWeight = DuoWeight
--               { duoBeta = duoBeta newWeight + headCost
--               , duoGap = duoGap newWeight }
--         lift $ pushPassive p finalWeight newTrav
-- #ifdef DebugOn
--         -- print logging information
--         hype <- RWS.get
--         liftIO $ do
--             putStr "[DE] " >> printActive q
--             putStr "  :  " >> printPassive p hype
--             putStr " #W  " >> print (duoBeta finalWeight)
--             -- putStr " #E  " >> print estDis
-- #endif


-- | Remove a state from the queue.
popItem
    :: (Ord t, Ord n)
    => Earley n t
        (Maybe (Binding (Item n t) (ExtWeight n t)))
popItem = RWS.state $ \st -> case Q.minView (waiting st) of
    Nothing -> (Nothing, st)
    Just (b, s) -> (Just b, st {waiting = s})


----------------------
-- Distance Estimation
----------------------


-- | Estimate the remaining distance for a passive item (with the exception of
-- the "gap weight", which is threaded via inference rules).
estimateDistP :: (Ord t, Ord n) => Passive n t -> Earley n t Weight
estimateDistP p = do
  (sup, pref, suff) <-  estimateDistP' p
  let dep = pref + suff
  return $ sup + dep
--        let sup = dagAmort (p ^. dagID)
--            dep = prefEsti (p ^. spanP ^. beg)
--                + suffEsti (p ^. spanP ^. end)
--         in sup + dep


-- | Estimate the remaining distance for a passive item.  Version of
-- `estimateDistP` for debugging.
estimateDistP' :: (Ord t, Ord n) => Passive n t -> Earley n t (Double, Double, Double)
estimateDistP' p = do
#ifdef HandleDummyArcWeight
  sentLen <- length <$> RWS.asks inputSent
  auto <- RWS.gets automat
  let H.Esti{..} = estiCost auto
      isFinal = Chart.isFinal S.empty sentLen auto p
#else
  H.Esti{..} <- RWS.gets (estiCost . automat)
  let isFinal = False
#endif
  -- The case of the final item is special: its amortized weight is 0, but its
  -- inside weight should contain the weight of the corresponding dependency
  -- link and ET weight (see `tryDeactivate`).
  return $ if isFinal
    then (0, 0, 0)
    else
      ( dagAmort (p ^. dagID)
      , prefEsti (p ^. spanP ^. beg)
      , suffEsti (p ^. spanP ^. end)
      )


-- | Estimate the remaining distance for an active item.
estimateDistA :: (Ord n, SOrd t) => Active n -> Earley n t Weight
estimateDistA q = do
    H.Esti{..} <- RWS.gets (estiCost . automat)
    let sup = trieAmort (q ^. state)
        dep = prefEsti (q ^. spanA ^. beg)
            + suffEsti (q ^. spanA ^. end)
    return $ sup + dep


-- | Estimate the remaining distance for an active item.
estimateDistA' :: (Ord n, SOrd t) => Active n -> Earley n t (Double, Double, Double)
estimateDistA' q = do
    H.Esti{..} <- RWS.gets (estiCost . automat)
    return $
      ( trieAmort (q ^. state)
      , prefEsti (q ^. spanA ^. beg)
      , suffEsti (q ^. spanA ^. end)
      )


-- | Compute the amortized weight of the given passive item.
amortizedWeight :: Passive n t -> Earley n t Weight
#ifdef NewHeuristic
amortizedWeight p = do
  dagAmort <- RWS.gets (H.dagAmort . estiCost . automat)
  return $ dagAmort (p ^. dagID)
#else
amortizedWeight = const $ return zeroWeight
#endif


-- | Compute the amortized weight of the given passive item.
amortizedWeight' :: Active n -> Earley n t Weight
#ifdef NewHeuristic
amortizedWeight' q = do
  H.Esti{..} <- RWS.gets (estiCost . automat)
  return $ trieAmort (q ^. state)
#else
amortizedWeight' = error "amortizedWeight' not implemented"
#endif


-- -- | TODO: Compute *the weight of* the bag of terminals for the given span.
-- bagOfTerms :: (Ord t) => Span -> Earley n t (H.Bag t)
-- bagOfTerms span = do
--     n <- sentLen
--     x <- estOn 0 (span ^. beg)
--     y <- estOn (span ^. end) n
-- #ifdef NewHeuristic
--     let z = H.bagEmpty
-- #else
--     z <- case span ^. gap of
--         Nothing -> return H.bagEmpty
--         Just (i, j) -> estOn i j
-- #endif
--     return $ x `H.bagAdd` y `H.bagAdd` z
--   where
--     sentLen = length <$> RWS.asks inputSent
--     estOn i j = H.bagFromList . map terminal . over i j <$> RWS.asks inputSent


-- | The minimal possible cost of the given token as a dependent.
minDepCost :: Tok t -> Earley n t Weight
minDepCost tok = do
  let pos = position tok
  H.Esti{..} <- RWS.gets (estiCost . automat)
  return $ minDepEsti pos


---------------------------------
-- Extraction of Processed Items
---------------------------------


-- | See `Chart.expectEnd`.
expectEnd
    :: (HOrd n, HOrd t) => DID -> Pos
    -> P.ListT (EarleyPipe n t) (Active n, DuoWeight)
expectEnd = Chart.expectEnd automat chart


-- | Return all passive items with:
-- * the given root non-terminal value (but not top-level auxiliary)
-- * the given span
rootSpan
    :: Ord n => n -> (Pos, Pos)
    -> P.ListT (EarleyPipe n t) (Passive n t, DuoWeight)
rootSpan = Chart.rootSpan automat chart


-- | See `Chart.rootEnd`.
rootEnd :: (Ord n, Ord t)
        => n -> Pos -> P.ListT (EarleyPipe n t) (Active n, DuoWeight)
rootEnd = Chart.rootEnd automat chart


-- | See `Chart.provideBeg`.
provideBeg
    :: (Ord n, Ord t) => DID -> Pos
    -> P.ListT (EarleyPipe n t) (Passive n t, DuoWeight)
provideBeg = Chart.provideBeg automat chart


-- | See `Chart.provideBeg'`.
provideBeg'
    :: (Ord n, Ord t) => n -> Pos
    -> P.ListT (EarleyPipe n t) (Passive n t, DuoWeight)
provideBeg' = Chart.provideBeg' chart


-- | See `Chart.provideBegIni`.
provideBegIni
    :: (Ord n, Ord t) => n -> Pos
    -> P.ListT (EarleyPipe n t) (Passive n t, DuoWeight)
provideBegIni =
  Chart.provideBegIni automat chart


-- | See `Chart.provideBegIni`.
provideBegIni'
    :: (Ord n, Ord t) => Either (NotFoot n) DID -> Pos
    -> P.ListT (EarleyPipe n t) (Passive n t, DuoWeight)
provideBegIni' = Chart.provideBegIni' automat chart


-- | TODO
withGap
    :: Ord n => (Pos, Pos, n)
    -> P.ListT (EarleyPipe n t) (Passive n t, DuoWeight)
withGap = Chart.withGap automat chart


-- | See `Chart.provideBegAux`.
provideBegAux
    :: (Ord n, Ord t) => DID -> Pos
    -> P.ListT (EarleyPipe n t) (Passive n t, DuoWeight)
provideBegAux = Chart.provideBegAux automat chart


-- | See `Chart.auxModifyGap`.
auxModifyGap
    :: Ord n => n -> (Pos, Pos)
    -> P.ListT (EarleyPipe n t) (Passive n t, DuoWeight)
auxModifyGap = Chart.auxModifyGap chart


--------------------------------------------------
-- SCAN
--------------------------------------------------


-- | Try to perform SCAN on the given active state.
tryScan :: (SOrd t, SOrd n) => Active n -> DuoWeight -> EarleyPipe n t ()
tryScan p duo = void $ P.runListT $ do
#ifdef DebugOn
  begTime <- liftIO $ Time.getCurrentTime
#endif
  -- read the word immediately following the ending position of
  -- the state
  tok <- readInput $ getL (spanA >>> end) p
--   -- determine the minimal cost of `tok` being a dependent
--   depCost <- liftIO $ minDepCost tok
  -- follow appropriate terminal transition outgoing from the
  -- given automaton state
  (termCost, j) <- followTerm (getL state p) (Just $ terminal tok)
  -- construct the resultant active item
  let q = setL state j
        . modL' (spanA >>> end) (+1)
        $ p
  -- push the resulting state into the waiting queue
  let newBeta = addWeight (duoBeta duo) termCost
      newGap = duoGap duo
      newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
  lift $ pushInduced q newDuo
           (Scan p tok termCost)
       -- . extWeight (addWeight cost termCost) estDist
#ifdef CheckMonotonic
  totalP <- lift . lift $ est2total duo <$> estimateDistA p
  totalQ <- lift . lift $ est2total newDuo <$> estimateDistA q
  when (totalQ + epsilon < totalP) $ do
    P.liftIO . putStrLn $
      "[SCAN: MONOTONICITY TEST FAILED] TAIL WEIGHT: " ++ show totalP ++
      ", HEAD WEIGHT: " ++ show totalQ
#endif
#ifdef DebugOn
  -- print logging information
  liftIO $ do
      endTime <- Time.getCurrentTime
      putStr "[S]  " >> printActive p
      putStr "  :  " >> printActive q
      putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
      putStr " #W  " >> print newBeta
      -- putStr " #E  " >> print estDist
#endif


-- | Try to scan an empty terminal.
tryEmpty :: (SOrd t, SOrd n) => Active n -> DuoWeight -> EarleyPipe n t ()
tryEmpty p duo = void $ P.runListT $ do
#ifdef DebugOn
  begTime <- liftIO $ Time.getCurrentTime
#endif
--   -- read the word immediately following the ending position of
--   -- the state
--   tok <- readInput $ getL (spanA >>> end) p
--   -- determine the minimal cost of `tok` being a dependent
--   depCost <- lift . lift $ minDepCost tok
  -- follow appropriate terminal transition outgoing from the
  -- given automaton state
  (termCost, j) <- followTerm (getL state p) Nothing
  -- construct the resultant active item
  let q = setL state j $ p
  -- compute the estimated distance for the resulting item
  -- estDist <- lift . estimateDistA $ q
  -- push the resulting state into the waiting queue
  let newBeta = addWeight (duoBeta duo) termCost
      newGap = duoGap duo
      newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
  lift $ pushInduced q newDuo
           (Empty p termCost)
       -- . extWeight (addWeight cost termCost) estDist
#ifdef CheckMonotonic
  totalP <- lift . lift $ est2total duo <$> estimateDistA p
  totalQ <- lift . lift $ est2total newDuo <$> estimateDistA q
  when (totalQ + epsilon < totalP) $ do
    P.liftIO . putStrLn $
      "[EMPTY: MONOTONICITY TEST FAILED] TAIL WEIGHT: " ++ show totalP ++
      ", HEAD WEIGHT: " ++ show totalQ
#endif
#ifdef DebugOn
  -- print logging information
  liftIO $ do
      endTime <- Time.getCurrentTime
      putStr "[E]  " >> printActive p
      putStr "  :  " >> printActive q
      putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
      putStr " #W  " >> print newBeta
      -- putStr " #E  " >> print estDist
#endif


--------------------------------------------------
-- OMEGA
--------------------------------------------------


-- | Weight of the given node becoming a dependent of another node.
-- Returns `Nothing` if the operation is not allowed.
omega
  :: DID  -- ^ Dependent
  -> DID  -- ^ Head
  -> EarleyPipe n t (Maybe Weight)
omega depDid hedDid = do
  auto <- RWS.gets automat
  let anchorMap = anchorPos auto
--       headMap = headPos auto
--       dag = gramDAG auto
  omegaPos depDid (M.lookup hedDid anchorMap)
--   let cost = do
--         -- determine the position of the dependent tree
--         depPos <- M.lookup depDid anchorMap
--         -- determine the position of the head tree
--         hedPos <- M.lookup hedDid anchorMap
--         -- determine the accepted positions of the dependent tree
--         posMap <- M.lookup depPos headMap
--         -- return the corresponding weight
--         return $ M.lookup hedPos posMap
--   -- we additionally add the dependent node weight (which should be non-zero
--   -- only if `depDid` is a root)
--   let treeWeight =
--         case DAG.value depDid dag of
--           Nothing -> 0
--           Just x -> x
--   -- combine and return
--   return . fmap (+treeWeight) $
--     case cost of
--       Just Nothing -> Nothing
--       Nothing -> Just 0
--       Just (Just x) -> Just x


-- | Weight of the given node becoming a dependent of another node.
-- Returns `Nothing` if the operation is not allowed.
omega'
  :: DID  -- ^ Dependent
  -> ID   -- ^ Head
  -> EarleyPipe n t (Maybe Weight)
omega' depDid hedId = do
  auto <- RWS.gets automat
--   let anchorMap = anchorPos auto
  let anchorMap' = anchorPos' auto
--       headMap = headPos auto
--       dag = gramDAG auto
  omegaPos depDid (M.lookup hedId anchorMap')
--   let cost = do
--         -- determine the position of the dependent tree
--         depPos <- M.lookup depDid anchorMap
--         -- determine the position of the head tree
--         hedPos <- M.lookup hedId anchorMap'
--         -- determine the accepted positions of the dependent tree
--         posMap <- M.lookup depPos headMap
--         -- return the corresponding weight
--         return $ M.lookup hedPos posMap
--   -- we additionally add the dependent node weight (which should be non-zero
--   -- only if `depDid` is a root)
--   let treeWeight =
--         case DAG.value depDid dag of
--           Nothing -> 0
--           Just x -> x
--   -- combine and return
--   return . fmap (+treeWeight) $
--     case cost of
--       Just Nothing -> Nothing
--       Nothing -> Just 0
--       Just (Just x) -> Just x


-- | Weight of the given node becoming a dependent of another node.
-- Returns `Nothing` if the operation is not allowed.
omegaPos
  :: DID        -- ^ Dependent
  -> Maybe Pos  -- ^ Head position (maybe)
  -> EarleyPipe n t (Maybe Weight)
omegaPos depDid hedPosMay = do
  auto <- RWS.gets automat
  let anchorMap = anchorPos auto
      anchorMap' = anchorPos' auto
      headMap = headPos auto
      dag = gramDAG auto
  let cost = do
        -- determine the position of the dependent tree
        depPos <- M.lookup depDid anchorMap
        -- determine the position of the head tree
        hedPos <- hedPosMay
        -- determine the accepted positions of the dependent tree
        posMap <- M.lookup depPos headMap
        -- return the corresponding weight
        return $ M.lookup hedPos posMap
  -- we additionally add the dependent node weight (which should be non-zero
  -- only if `depDid` is a root)
  let treeWeight =
        case DAG.value depDid dag of
          Nothing -> 0
          Just x -> x
  -- combine and return
  return . fmap (+treeWeight) $
    case cost of
      Just Nothing -> Nothing
      Nothing -> Just 0
      Just (Just x) -> Just x


--------------------------------------------------
-- PSEUDO SUBSTITUTION
--------------------------------------------------


-- | The PS rule.
tryPseudoSubst
    :: (SOrd t, SOrd n)
    => Passive n t
    -> DuoWeight
    -> EarleyPipe n t ()
tryPseudoSubst p pw = void $ P.runListT $ do
#ifdef DebugOn
    begTime <- liftIO $ Time.getCurrentTime
#endif
    let pDID = getL dagID p
        pSpan = getL spanP p

--     -- make sure that `p' represents regular rules
--     guard . regular $ pSpan

    -- some underlying maps
    auto <- RWS.gets automat
    let dag = gramDAG auto

    -- make sure the node is not a root (NOTE: perhaps this is redundant, but
    -- shouldn't hurt)
    guard . not $ DAG.isRoot pDID dag

--     NEW: if it's not root, it cannot represent a sister tree
--     -- make sure that `p` does not represent sister tree
--     guard $ not (isSister' pDID dag)

    -- find active items which end where `p' begins and which
    -- expect the non-terminal provided by `p' (ID included)
    (q, qw) <- expectEnd pDID (pSpan ^. beg)

    -- follow the transition symbol
    (tranCost, j) <- follow (q ^. state) pDID

    -- NEW: we need to know the span of the matched active item
    let qSpan = q ^. spanA

    -- construct the resultant state
    let q' = setL state j
           . setL (spanA >>> end) (pSpan ^. end)
           . setL (spanA >>> gaps)
                    ((pSpan ^. gaps) `S.union` (qSpan ^. gaps))
           $ q

    -- push the resulting state into the waiting queue
    let newBeta = sumWeight [duoBeta pw, duoBeta qw, tranCost]
        newGap = disjointUnion (duoGap pw) (duoGap qw)
        newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
    lift $ pushInduced q' newDuo (Subst p q tranCost)
#ifdef CheckMonotonic
    lift . lift $ testMono "PS" (p, pw) (q, qw) (q', newDuo)
#endif
#ifdef DebugOn
    -- print logging information
    hype <- RWS.get
    liftIO $ do
        endTime <- Time.getCurrentTime
        putStr "[?]  " >> printPassive p hype
        putStr "  +  " >> printActive q
        putStr "  :  " >> printActive q'
        putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
        putStr " #W  " >> print newBeta
#endif


-- | Reversed `PS` version.
tryPseudoSubst'
    :: (SOrd t, SOrd n)
    => Active n
    -> DuoWeight
    -> EarleyPipe n t ()
tryPseudoSubst' q qw = void $ P.runListT $ do
#ifdef DebugOn
    begTime <- liftIO $ Time.getCurrentTime
#endif
    -- some underlying maps
    auto <- RWS.gets automat
    let dag = gramDAG auto
--         spine = isSpine auto

    -- span of the q item
    let qSpan = q ^. spanA

    -- Learn what non-terminals `q` actually expects.
    -- WARNING: in the automaton-based parser, this seems not
    -- particularly efficient in some corner cases...
    -- For instance, when `q` refers to the root node of an
    -- automaton.  Can we bypass this issue?
    (qDID, tranCost, j) <- elems (q ^. state)

    -- Make sure it's not a leaf
    guard . not $ DAG.isLeaf qDID dag

    -- Find processed items which begin where `q` ends and which provide the
    -- DAG node expected by `q`.
    (p, pw) <- provideBeg qDID (q ^. spanA ^. end)
--       if spine qDID
--          then provideBegAux        qDID  (q ^. spanA ^. end)
--          else provideBegIni (Right qDID) (q ^. spanA ^. end)
    let pSpan = p ^. spanP

    -- construct the resultant state
    let q' = setL state j
           . setL (end . spanA) (pSpan ^. end)
           . setL (gaps . spanA)
                    ((pSpan ^. gaps) `S.union` (qSpan ^. gaps))
           $ q

    -- compute the estimated distance for the resulting state
    -- estDist <- lift . estimateDistA $ q'
    -- push the resulting state into the waiting queue
    let newBeta = sumWeight [duoBeta pw, duoBeta qw, tranCost]
        newGap = disjointUnion (duoGap pw) (duoGap qw)
        newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
    lift $ pushInduced q' newDuo (Subst p q tranCost)
#ifdef CheckMonotonic
    lift . lift $ testMono "PS'" (p, pw) (q, qw) (q', newDuo)
#endif
#ifdef DebugOn
    -- print logging information
    hype <- RWS.get
    liftIO $ do
        endTime <- Time.getCurrentTime
        putStr "[?'] " >> printActive q
        putStr "  +  " >> printPassive p hype
        putStr "  :  " >> printActive q'
        putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
        putStr " #W  " >> print newBeta
        -- putStr " #E  " >> print estDist
#endif


--------------------------------------------------
-- SUBST
--------------------------------------------------


-- | Try to use the passive item `p` to complement
-- (=> substitution) other rules.
trySubst
    :: (SOrd t, SOrd n)
    => Passive n t
    -> DuoWeight
    -> EarleyPipe n t ()
trySubst p pw = void $ P.runListT $ do
#ifdef DebugOn
    begTime <- liftIO $ Time.getCurrentTime
#endif
    let pDID = getL dagID p
        pSpan = getL spanP p
    -- some underlying maps
    auto <- RWS.gets automat
    let dag = gramDAG auto
        leafMap = leafDID auto
--  UPDATE 20.02.2020: no longer necessary to check if `regular`,
--  check the inference rules
--     -- make sure that `p' represents regular rules
--     guard . regular $ pSpan
    -- make sure that `p` is good for substitution
    guard $ DAG.isRoot pDID dag
    -- make sure that `p` does not represent sister tree
    guard . not $ isSister' pDID dag
    -- UPDATE 20.02.202: make sure that `p` has ?ws == False
    guard . not $ getL ws p
    -- now, we need to choose the DAG node to search for
    (theDID, depCost) <- do
      -- take the `DID` of a leaf with the appropriate non-terminal
      did <- each . S.toList . maybe S.empty id $
        M.lookup (nonTerm pDID auto) leafMap
      -- verify that the substitution is OK w.r.t. the dependency info
      Just cost <- lift $ omega pDID did
      return (did, cost)
    -- find active items which end where `p' begins and which
    -- expect the non-terminal provided by `p' (ID included)
    (q, qw) <- expectEnd theDID (getL beg pSpan)
    -- follow the transition symbol
    (tranCost, j) <- follow (q ^. state) theDID
    -- construct the resulting state
    let q' = setL state j
           . setL (spanA >>> end) (pSpan ^. end)
           -- UPDATE 20.02.202: handle the gap set
           . modL' (spanA >>> gaps) (S.union $ getL gaps pSpan)
           $ q
    -- push the resulting state into the waiting queue
    let newBeta = sumWeight [duoBeta pw, duoBeta qw, tranCost, depCost]
        newGap = disjointUnion (duoGap qw) (duoGap pw)
        newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
    lift $ pushInduced q' newDuo (Subst p q $ tranCost + depCost)
#ifdef CheckMonotonic
    lift . lift $ testMono "SUBST" (p, pw) (q, qw) (q', newDuo)
#endif
#ifdef DebugOn
    -- print logging information
    hype <- RWS.get
    liftIO $ do
        endTime <- Time.getCurrentTime
        putStr "[U]  " >> printPassive p hype
        putStr "  +  " >> printActive q
        putStr "  :  " >> printActive q'
        putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
        putStr " #W  " >> print newBeta
        -- putStr " #E  " >> print estDist
#endif


-- | Reversed `trySubst` version.  Try to completent the item with
-- another fully parsed item.
trySubst'
    :: (SOrd t, SOrd n)
    => Active n
    -> DuoWeight
    -> EarleyPipe n t ()
trySubst' q qw = void $ P.runListT $ do
#ifdef DebugOn
    begTime <- liftIO $ Time.getCurrentTime
#endif
    -- some underlying maps
    auto <- RWS.gets automat
    let dag = gramDAG auto
        leafMap = leafDID auto

    -- Learn what non-terminals `q` actually expects.
    -- WARNING: in the automaton-based parser, this seems not
    -- particularly efficient in some corner cases...
    -- For instance, when `q` refers to the root node of an
    -- automaton.  Can we bypass this issue?
    (qDID, tranCost, j) <- elems (q ^. state)

    -- Make sure that `qDID` is a leaf
    guard $ DAG.isLeaf qDID dag

    -- Determine the corresponding non-terminal
    qNT <- some $ do
      O.NonTerm x <- DAG.label qDID dag
      return x

    -- Find processed items which begin where `q` ends and which
    -- provide the non-terminal expected by `q`.
    (p, pw) <- provideBegIni qNT (q ^. spanA ^. end)
    let pDID = p ^. dagID

    -- make sure that `pDID` is a root
    guard $ DAG.isRoot pDID dag

    -- make sure that `p` does not represent a sister tree
    guard $ not (isSister' pDID dag)

    -- UPDATE 20.02.202: make sure that `p` has ?ws == False
    guard . not $ getL ws p

    -- verify that the substitution is OK w.r.t. the dependency info
    Just depCost <- lift $ omega (p ^. dagID) qDID

    let pSpan = p ^. spanP
    -- construct the resultant state
    let q' = setL state j
           . setL (end . spanA) (pSpan ^. end)
           -- UPDATE 20.02.202: handle the gap set
           . modL' (gaps . spanA) (S.union $ getL gaps pSpan)
           $ q

    -- push the resulting state into the waiting queue
    let newBeta = sumWeight [duoBeta pw, duoBeta qw, tranCost, depCost]
        newGap = disjointUnion (duoGap qw) (duoGap pw)
        newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
    lift $ pushInduced q' newDuo (Subst p q $ tranCost + depCost)
#ifdef CheckMonotonic
    lift . lift $ testMono "SUBST'" (p, pw) (q, qw) (q', newDuo)
#endif
#ifdef DebugOn
    -- print logging information
    hype <- RWS.get
    liftIO $ do
        endTime <- Time.getCurrentTime
        putStr "[U'] " >> printActive q
        putStr "  +  " >> printPassive p hype
        putStr "  :  " >> printActive q'
        putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
        putStr " #W  " >> print newBeta
        -- putStr " #E  " >> print estDist
#endif


--------------------------------------------------
-- FOOT ADJOIN
--------------------------------------------------


-- -- | `tryAdjoinInit p q':
-- -- * `p' is a completed state (regular or auxiliary)
-- --     ; if `p` auxiliary, then not top-level
-- -- * `q' not completed and expects a *real* foot
-- tryAdjoinInit
--     :: (SOrd n, SOrd t)
--     => Passive n t
--     -> DuoWeight
--     -> EarleyPipe n t ()
-- tryAdjoinInit p pw = void $ P.runListT $ do
-- #ifdef DebugOn
--     begTime <- liftIO $ Time.getCurrentTime
-- #endif
--     let pDID = p ^. dagID
--         pSpan = p ^. spanP
--     -- the underlying dag grammar
--     hype <- RWS.get
--     -- dag <- RWS.gets (gramDAG . automat)
--     let dag = gramDAG . automat $ hype
--     -- footMap <- RWS.gets (footDID  . automat)
--     let footMap = footDID  . automat $ hype
--     -- make sure that the corresponding rule is either regular or
--     -- intermediate auxiliary ((<=) used as implication here)
-- --     guard $ auxiliary pSpan <= not (isRoot pDID)
--     guard $ auxiliary pSpan <= not (DAG.isRoot pDID dag)
--     -- NEW 09.09.2019: make sure `p` does not correspond to a sister node
--     guard . not $ isSister' (p ^. dagID) dag
--     -- find all active items which expect a foot with the given
--     -- symbol and which end where `p` begins
--     footNT <- some (nonTerm' pDID dag)
--     -- what is the corresponding foot DAG ID?
--     footID <- each . S.toList . maybe S.empty id $ M.lookup footNT footMap
--     -- find all active items which expect a foot with the given
--     -- symbol and which end where `p` begins
--     (q, qw) <- expectEnd footID (pSpan ^. beg)
--     -- follow the foot
--     (tranCost, j) <- follow (q ^. state) footID
--     -- construct the resultant state
--     let q' = setL state j
--            . setL (spanA >>> end) (pSpan ^. end)
--            . setL (spanA >>> gap) (Just
--                 ( pSpan ^. beg
--                 , pSpan ^. end ))
--            $ q
--     -- compute the estimated distance for the resulting state
--     -- -- estDist <- lift . estimateDistA $ q'
--     -- compute the amortized weight of item `p`
--     amortWeight <- lift . lift $ amortizedWeight p
--     -- push the resulting state into the waiting queue
--     let newBeta = addWeight (duoBeta qw) tranCost
--         -- newGap = duoBeta pw + duoGap pw + amortWeight
--         -- TODO: `duaGap qw` should be equal to `0` again!
--         newGap = duoGap qw + duoBeta pw + duoGap pw + amortWeight
--         newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
--     lift $ pushInduced q' newDuo
--              (Foot q (nonTermH (p ^. dagID) hype) tranCost)
-- --     -- push the resulting state into the waiting queue
-- --     lift $ pushInduced q' $ Foot q p -- -- $ nonTerm foot
-- #ifdef CheckMonotonic
--     lift . lift $ testMono "ADJOIN-INIT" (p, pw) (q, qw) (q', newDuo)
-- #endif
-- #ifdef DebugOn
--     -- print logging information
--     liftIO $ do
--         endTime <- Time.getCurrentTime
--         putStr "[A]  " >> printPassive p hype
--         putStr "  +  " >> printActive q
--         putStr "  :  " >> printActive q'
--         putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
--         putStr " #W  " >> print newBeta
--         -- putStr " #E  " >> print estDist
-- #endif
-- 
-- 
-- -- | Reverse of `tryAdjoinInit` where the given state `q`
-- -- expects a real foot.
-- -- * `q' not completed and expects a *real* foot
-- -- * `p' is a completed state (regular or auxiliary)
-- --     ; if `p` auxiliary, then not top-level
-- tryAdjoinInit'
--     :: (SOrd n, SOrd t)
--     => Active
--     -> DuoWeight
--     -> EarleyPipe n t ()
-- tryAdjoinInit' q qw = void $ P.runListT $ do
-- #ifdef DebugOn
--     begTime <- liftIO $ Time.getCurrentTime
-- #endif
--     -- the underlying dag
--     -- dag <- RWS.gets (gramDAG . automat)
--     hype <- RWS.get
--     let dag = gramDAG . automat $ hype
--     -- Retrieve the foot expected by `q`.
--     -- (AuxFoot footNT, _) <- some $ expects' q
--     -- (AuxFoot footNT, tranCost, j) <- elems (q ^. state)
--     (qDID, tranCost, j) <- elems (q ^. state)
--     qNT <- some $ do
--         O.Foot x <- DAG.label qDID dag
--         return x
--     -- Find all passive items which provide the given source
--     -- non-terminal and which begin where `q` ends.
--     (p, pw) <- provideBeg' qNT (q ^. spanA ^. end)
--     let pDID = p ^. dagID
--         pSpan = p ^. spanP
--     -- The retrieved items must not be auxiliary top-level.
--     guard $ auxiliary pSpan <= not (DAG.isRoot pDID dag)
--     -- NEW 09.09.2019: make sure `p` does not correspond to a sister node
--     guard . not $ isSister' (p ^. dagID) dag
--     -- Construct new item
--     let q' = setL state j
--            . setL (spanA >>> end) (pSpan ^. end)
--            . setL (spanA >>> gap) (Just
--                 ( pSpan ^. beg
--                 , pSpan ^. end ))
--            $ q
--     -- compute the estimated distance for the resulting state
--     -- -- estDist <- lift . estimateDistA $ q'
--     -- compute the amortized weight of item `p`
--     amortWeight <- lift . lift $ amortizedWeight p
--     -- push the resulting state into the waiting queue
--     let newBeta = addWeight (duoBeta qw) tranCost
--         -- newGap = duoBeta pw + duoGap pw + amortWeight
--         -- NEW 28.12.2018:
--         newGap = duoGap qw + duoBeta pw + duoGap pw + amortWeight
--         newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
--     lift $ pushInduced q' newDuo
--              (Foot q (nonTermH (p ^. dagID) hype) tranCost)
-- #ifdef CheckMonotonic
--     lift . lift $ testMono "ADJOIN-INIT'" (p, pw) (q, qw) (q', newDuo)
-- #endif
-- #ifdef DebugOn
--     -- print logging information
--     liftIO $ do
--         endTime <- Time.getCurrentTime
--         putStr "[A'] " >> printActive q
--         putStr "  +  " >> printPassive p hype
--         putStr "  :  " >> printActive q'
--         putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
--         putStr " #W  " >> print newBeta
--         -- putStr " #E  " >> print estDist
-- #endif


--------------------------------------------------
-- ROOT ADJOIN
--------------------------------------------------


-- -- | Adjoin a fully-parsed auxiliary tree represented by `q` to
-- -- a partially parsed tree represented by a passive item `p`.
-- tryAdjoinTerm
--     :: (SOrd t, SOrd n)
--     => Passive n t
--     -> DuoWeight
--     -> EarleyPipe n t ()
-- tryAdjoinTerm q qw = void $ P.runListT $ do
-- #ifdef DebugOn
--     begTime <- liftIO $ Time.getCurrentTime
-- #endif
--     let qDID = q ^. dagID
--         qSpan = q ^. spanP
--     -- the underlying dag grammar
--     auto <- RWS.gets automat
--     let dag = gramDAG auto
--     -- make sure the label is top-level, i.e. that
--     -- `qDID` represents a fully parsed (auxiliary) tree
--     guard $ DAG.isRoot qDID dag
--     -- make sure that it is an auxiliary item (by definition only
--     -- auxiliary states have gaps)
--     (gapBeg, gapEnd) <- some $ qSpan ^. gap
--     -- take all passive items with a given span and a given
--     -- root non-terminal (IDs irrelevant); it must not be
--     -- a top-level auxiliary item (which should be guaranteed
--     -- by `rootSpan`)
--     qNonTerm <- some (nonTerm' qDID dag)
--     (p, pw) <- rootSpan qNonTerm (gapBeg, gapEnd)
--     -- NEW 17.04.2019: make sure `p` does not correspond to a sister node
--     guard . not $ isSister' (p ^. dagID) dag
-- #ifdef NoAdjunctionRestriction
--     let changeAdjState = id
-- #else
--     -- make sure that node represented by `p` was not yet adjoined to
--     guard . not $ getL isAdjoinedTo p
--     let changeAdjState = setL isAdjoinedTo True
-- #endif
--     -- check w.r.t. the dependency structure
-- --     let anchorMap = anchorPos auto
-- --         headMap = headPos auto
-- --     let cost = do
-- --     -- guard . (/= Just False) $ do
-- --           -- determine the position of the head tree
-- --           hedPos <- M.lookup (p ^. dagID) anchorMap
-- --           -- determine the position of the dependent tree
-- --           depPos <- M.lookup (q ^. dagID) anchorMap
-- --           -- determine the accepted positions of the dependent tree
-- --           posMap <- M.lookup depPos headMap
-- --           -- check if they agree
-- --           return $ M.lookup hedPos posMap
-- --     guard $ cost /= Just Nothing
-- --     let depCost = maybe 0 fromJust cost
--     Just depCost <- lift $ omega (q ^. dagID) (p ^. dagID)
-- 
--     -- construct the resulting item
--     let p' = setL (spanP >>> beg) (qSpan ^. beg)
--            . setL (spanP >>> end) (qSpan ^. end)
--            . changeAdjState
--            $ p
--     -- lift $ pushPassive p' $ Adjoin q p
--     -- compute the estimated distance for the resulting state
--     -- estDist <- lift . estimateDistP $ p'
--     -- push the resulting state into the waiting queue
--     let newBeta = sumWeight [duoBeta pw, duoBeta qw, depCost]
--         newGap = duoGap pw
--         newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
--     lift $ pushPassive p' newDuo (Adjoin q p depCost)
-- #ifdef CheckMonotonic
--     totalP <- lift . lift $ est2total pw <$> estimateDistP p
--     totalQ <- lift . lift $ est2total qw <$> estimateDistP q
--     totalQ' <- lift . lift $ est2total newDuo <$> estimateDistP p'
--     let tails =  [totalP, totalQ]
--     when (any (totalQ' + epsilon <) tails) $ do
--       P.liftIO . putStrLn $
--         "[ADJOIN-TERM: MONOTONICITY TEST FAILED] TAILS: " ++ show tails ++
--         ", HEAD: " ++ show totalQ'
-- #endif
-- #ifdef DebugOn
--     hype <- RWS.get
--     liftIO $ do
--         endTime <- Time.getCurrentTime
--         putStr "[C]  " >> printPassive q hype
--         putStr "  +  " >> printPassive p hype
--         putStr "  :  " >> printPassive p' hype
--         putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
--         putStr " #W  " >> print newBeta
--         -- putStr " #E  " >> print estDist
-- #endif
-- 
-- 
-- -- | Reversed `tryAdjoinTerm`.
-- tryAdjoinTerm'
--     :: (SOrd t, SOrd n)
--     => Passive n t
--     -> DuoWeight
--     -> EarleyPipe n t ()
-- tryAdjoinTerm' p pw = void $ P.runListT $ do
-- #ifdef DebugOn
--     begTime <- liftIO $ Time.getCurrentTime
-- #endif
--     let pDID = p ^. dagID
--         pSpan = p ^. spanP
--     -- the underlying dag grammar
--     auto <- RWS.gets automat
--     let dag = gramDAG auto
--     -- Ensure that `p` is auxiliary but not top-level
--     guard $ auxiliary pSpan <= not (DAG.isRoot pDID dag)
--     -- NEW 17.04.2019: make sure `p` does not correspond to a sister node
--     guard . not $ isSister' (p ^. dagID) dag
--     -- Retrieve the non-terminal in the p's root
--     pNT <- some $ nonTerm' pDID dag
--     -- Retrieve all completed, top-level items representing auxiliary
--     -- trees which have a specific gap and modify a specific source
--     -- non-terminal.
--     (q, qw) <- auxModifyGap pNT
--         -- (nonTerm $ p ^. label)
--         ( p ^. spanP ^. beg
--         , p ^. spanP ^. end )
--     let qSpan = q ^. spanP
--     -- check w.r.t. the dependency structure
-- --     let anchorMap = anchorPos auto
-- --         headMap = headPos auto
-- --     let cost = do
-- --           -- determine the position of the head tree
-- --           hedPos <- M.lookup (p ^. dagID) anchorMap
-- --           -- determine the position of the dependent tree
-- --           depPos <- M.lookup (q ^. dagID) anchorMap
-- --           -- determine the accepted positions of the dependent tree
-- --           posMap <- M.lookup depPos headMap
-- --           -- check if they agree
-- --           return $ M.lookup hedPos posMap
-- --     guard $ cost /= Just Nothing
-- --     let depCost = maybe 0 fromJust cost
--     Just depCost <- lift $ omega (q ^. dagID) (p ^. dagID)
-- 
--     -- Construct the resulting state:
--     let p' = setL (spanP >>> beg) (qSpan ^. beg)
--            . setL (spanP >>> end) (qSpan ^. end)
--            $ p
--     -- compute the estimated distance for the resulting state
--     -- estDist <- lift . estimateDistP $ p'
--     -- push the resulting state into the waiting queue
--     let newBeta = sumWeight [duoBeta pw, duoBeta qw, depCost]
--         newGap = duoGap pw
--         newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
--     lift $ pushPassive p' newDuo (Adjoin q p depCost)
-- #ifdef CheckMonotonic
--     totalP <- lift . lift $ est2total pw <$> estimateDistP p
--     totalQ <- lift . lift $ est2total qw <$> estimateDistP q
--     totalQ' <- lift . lift $ est2total newDuo <$> estimateDistP p'
--     let tails = [totalP, totalQ]
--     when (any (totalQ' + epsilon <) tails) $ do
--       P.liftIO . putStrLn $
--         "[ADJOIN-TERM': MONOTONICITY TEST FAILED] TAILS: " ++ show tails ++
--         ", HEAD: " ++ show totalQ'
-- #endif
-- #ifdef DebugOn
--     hype <- RWS.get
--     liftIO $ do
--         endTime <- Time.getCurrentTime
--         putStr "[C'] " >> printPassive p hype
--         putStr "  +  " >> printPassive q hype
--         putStr "  :  " >> printPassive p' hype
--         putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
--         putStr " #W  " >> print newBeta
--         -- putStr " #E  " >> print estDist
-- #endif


--------------------------------------------------
-- DEACTIVATE
--------------------------------------------------


-- | Try to perform DEACTIVATE.
tryDeactivate
  :: (SOrd t, SOrd n)
  => Active n
  -> DuoWeight
  -> EarleyPipe n t ()
tryDeactivate q qw = void $ P.runListT $ do
#ifdef DebugOn
  begTime <- liftIO $ Time.getCurrentTime
#endif
  -- sentLen <- length <$> RWS.asks inputSent
  dag <- RWS.gets (gramDAG . automat)
  -- (headCost, did) <- heads (getL state q)
  (_headCost, did) <- heads (getL state q)
  let p = Passive
          { _dagID = did
          , _spanP = getL spanA q
          -- , _isAdjoinedTo = False }
          , _ws = DAG.isDNode did dag }
  let finalWeight = DuoWeight
        { duoBeta = duoBeta qw -- + headCost
        -- { duoBeta = duoBeta qw + headCost
        , duoGap = duoGap qw }
  -- lift $ pushPassive p finalWeight (Deactivate q headCost)
  lift $ pushPassive p finalWeight (Deactivate q 0)
#ifdef CheckMonotonic
  totalQ <- lift . lift $ est2total qw <$> estimateDistA q
  totalP <- lift . lift $ est2total finalWeight <$> estimateDistP p
  when (totalP + epsilon < totalQ) $ do
    P.liftIO . putStrLn $
      "[DEACTIVATE: MONOTONICITY TEST FAILED] TAIL WEIGHT: " ++ show totalQ ++
      ", HEAD WEIGHT: " ++ show totalP
#endif
#ifdef DebugOn
  -- print logging information
  hype <- RWS.get
  liftIO $ do
      endTime <- Time.getCurrentTime
      putStr "[DE] " >> printActive q
      putStr "  :  " >> printPassive p hype
      putStr " #W  " >> print (duoBeta finalWeight)
      putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
      putStr " ?R  " >> putStr (show $ DAG.isRoot (p ^. dagID) dag)
                     >> putStrLn " (is the new item a root?)"
      -- putStr " #E  " >> print estDis
#endif
  where
    mkRoot node = case node of
      O.NonTerm x -> NotFoot {notFootLabel=x, isSister=False}
      O.Sister x  -> NotFoot {notFootLabel=x, isSister=True}
      _ -> error "pushInduced: invalid root"
    check (Just x) = x
    check Nothing  = error "pushInduced: invalid DID"


--------------------------------------------------
-- SISTER ADJUNCTION
--------------------------------------------------


-- | Try to apply sister-adjunction w.r.t. the given passive item.
trySisterAdjoin
  :: (SOrd t, SOrd n)
  => Passive n t
  -> DuoWeight
  -> EarleyPipe n t ()
trySisterAdjoin p pw = void $ P.runListT $ do
#ifdef DebugOn
    begTime <- liftIO $ Time.getCurrentTime
#endif
    hype <- RWS.get
    let auto = automat hype
        dag = gramDAG auto
        pDID = getL dagID p
        pSpan = getL spanP p
--  UPDATE 20.02.2020: no longer necessary to check if `regular`,
--  check the inference rules
--     -- make sure that `p' is not gapped
--     guard . regular $ pSpan
    -- make sure that `p` represents a sister tree
--     Left root <- return pDID
    guard $ DAG.isRoot pDID dag && isSister' pDID dag
    -- UPDATE 19.02.202: make sure that `p` has ?ws == False
    guard . not $ getL ws p
    -- find active items which end where `p' begins and which have the
    -- corresponding LHS non-terminal
--     (q, qw) <- rootEnd (notFootLabel root) (getL beg pSpan)
    (q, qw) <- rootEnd (nonTermH pDID hype) (getL beg pSpan)
    -- check w.r.t. the dependency structure
--     let anchorMap = anchorPos auto
--         anchorMap' = anchorPos' auto
--         headMap = headPos auto
--     let cost = do
--           -- determine the position of the head tree
--           hedPos <- M.lookup (q ^. state) anchorMap'
--           -- determine the position of the dependent tree
--           depPos <- M.lookup (p ^. dagID) anchorMap
--           -- determine the accepted positions of the dependent tree
--           posMap <- M.lookup depPos headMap
--           -- check if they agree
--           return $ M.lookup hedPos posMap
--     guard $ cost /= Just Nothing
--     let depCost = maybe 0 fromJust cost
    Just depCost <- lift $ omega' (p ^. dagID) (q ^. state)

    -- construct the resultant item with the same state and extended span
    let q' = setL (end . spanA) (getL end pSpan)
           -- UPDATE 19.02.202: handle the gap set
           . modL' (gaps . spanA) (S.union $ getL gaps pSpan)
           $ q
        newBeta = sumWeight [duoBeta pw, duoBeta qw, depCost]
        newGap = disjointUnion (duoGap pw) (duoGap qw)
        newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
    -- push the resulting state into the waiting queue
    lift $ pushInduced q' newDuo (SisterAdjoin p q depCost)
#ifdef CheckMonotonic
    lift . lift $ testMono "SISTER-ADJOIN" (p, pw) (q, qw) (q', newDuo)
#endif
#ifdef DebugOn
    -- print logging information
    hype <- RWS.get
    liftIO $ do
        endTime <- Time.getCurrentTime
        putStr "[I]  " >> printPassive p hype
        putStr "  +  " >> printActive q
        putStr "  :  " >> printActive q'
        putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
#endif


-- | Sister adjunction reversed.
trySisterAdjoin'
  :: (SOrd t, SOrd n)
  => Active n
  -> DuoWeight
  -> EarleyPipe n t ()
trySisterAdjoin' q qw = void $ P.runListT $ do
#ifdef DebugOn
  begTime <- liftIO $ Time.getCurrentTime
#endif
  -- the underlying dag
  auto <- RWS.gets automat
  let dag = gramDAG auto
  -- the underlying LHS map
  lhsMap <- RWS.gets (lhsNonTerm . automat)
  -- Learn what is the LHS of `q`
  let lhsNotFoot = lhsMap M.! getL state q
  guard . not $ isSister lhsNotFoot
  -- Find processed passive items which begin where `q` ends and which represent
  -- sister trees.
  let sister = lhsNotFoot {isSister = True}
  (p, pw) <- provideBegIni' (Left sister) (q ^. spanA ^. end)
  -- check w.r.t. the dependency structure
--   let anchorMap = anchorPos auto
--       anchorMap' = anchorPos' auto
--       headMap = headPos auto
--   let cost = do
--         -- determine the position of the head tree
--         hedPos <- M.lookup (q ^. state) anchorMap'
--         -- determine the position of the dependent tree
--         depPos <- M.lookup (p ^. dagID) anchorMap
--         -- determine the accepted positions of the dependent tree
--         posMap <- M.lookup depPos headMap
--         -- check if they agree
--         return $ M.lookup hedPos posMap
--   guard $ cost /= Just Nothing
--   let depCost = maybe 0 fromJust cost
  Just depCost <- lift $ omega' (p ^. dagID) (q ^. state)

  -- construct the resulting item with the same state and extended span
  let pSpan = getL spanP p
      q' = setL (end . spanA) (getL end pSpan) $ q
      newBeta = sumWeight [duoBeta pw, duoBeta qw, depCost]
      newGap = disjointUnion (duoGap pw) (duoGap qw)
      newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
  -- push the resulting state into the waiting queue
  lift $ pushInduced q' newDuo (SisterAdjoin p q depCost)
#ifdef CheckMonotonic
  lift . lift $ testMono "SISTER-ADJOIN'" (p, pw) (q, qw) (q', newDuo)
#endif
#ifdef DebugOn
  -- print logging information
  hype <- RWS.get
  liftIO $ do
      endTime <- Time.getCurrentTime
      putStr "[I'] " >> printPassive p hype
      putStr "  +  " >> printActive q
      putStr "  :  " >> printActive q'
      putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
#endif


--------------------------------------------------
-- PREDICT WRAPPING
--
-- UPDATE 20.02.2020: new rule!
--------------------------------------------------


-- | TODO: add description
tryPredictWrapping 
  :: (SOrd t, SOrd n)
  => Passive n t
  -> DuoWeight
  -> EarleyPipe n t ()
tryPredictWrapping p pw = void $ P.runListT $ do
#ifdef DebugOn
    begTime <- liftIO $ Time.getCurrentTime
#endif
    let pDID = getL dagID p
        pSpan = getL spanP p
    -- the underlying dag grammar
    dag <- RWS.gets (gramDAG . automat)
    -- make sure that `p` has ?ws == True
    guard $ getL ws p
    -- the underlying leaf map
    leafMap <- RWS.gets (leafDID  . automat)
    -- now, we need to choose the DAG node to search for
    nodeNT <- some (nonTerm' pDID dag)
    theDID <- each . S.toList . maybe S.empty id $ M.lookup nodeNT leafMap
    -- find active items which end where `p' begins and which
    -- expect the DAG node provided by `p'
    (q, qw) <- expectEnd theDID (getL beg pSpan)
    -- follow the DAG node
    (tranCost, j) <- follow (getL state q) theDID
    -- construct the resulting state
    let pBeg = getL beg pSpan
        pEnd = getL end pSpan
        newGap = (pBeg, pEnd, nodeNT)
        q' = setL state j
           . setL (end . spanA) pEnd
           . modL' (gaps . spanA) (S.insert newGap)
           $ q
    -- compute the amortized weight of item `p`
    amortWeight <- lift . lift $ amortizedWeight p
    -- push the resulting state into the waiting queue
    let newBeta = addWeight (duoBeta qw) tranCost
        -- newGap = sum [duoGap qw, duoBeta pw, duoGap pw, amortWeight]
        newGap = safeInsert pBeg
          (duoBeta pw + sum (M.elems (duoGap pw)) + amortWeight)
          (duoGap qw)
        newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
    -- push the resulting state into the waiting queue
    -- liftIO $ putStrLn "==>> PW !!" 
    lift $ pushInduced q' newDuo (PredictWrapping p q tranCost)
#ifdef CheckMonotonic
    lift . lift $ testMono "PREDICT-WRAPPING" (p, pw) (q, qw) (q', newDuo)
#endif
#ifdef DebugOn
    -- print logging information
    hype <- RWS.get
    liftIO $ do
        endTime <- Time.getCurrentTime
        putStr "[P]  " >> printPassive p hype
        putStr "  +  " >> printActive q
        putStr "  :  " >> printActive q'
        putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
#endif


-- | The alternative `tryPredictWrapping`
tryPredictWrapping'
  :: (SOrd t, SOrd n)
  => Active n
  -> DuoWeight
  -> EarleyPipe n t ()
tryPredictWrapping' q qw = void $ P.runListT $ do
#ifdef DebugOn
    begTime <- liftIO $ Time.getCurrentTime
#endif

    -- some underlying maps
    auto <- RWS.gets automat
    let dag = gramDAG auto
        leafMap = leafDID auto

    -- Learn what non-terminals `q` actually expects.
    (qDID, tranCost, j) <- elems (q ^. state)

    -- Make sure that `qDID` is a leaf
    guard $ DAG.isLeaf qDID dag

    -- Determine the corresponding non-terminal
    qNT <- some $ do
      O.NonTerm x <- DAG.label qDID dag
      return x

    -- Find processed items which:
    -- (a) begin where `q` ends, and which          (OK)
    -- (b) provide the non-terminal expected by `q` (OK)
    (p, pw) <- provideBegIni qNT (q ^. spanA ^. end)
    let pDID = p ^. dagID
        pSpan = p ^. spanP

    -- make sure that `p` has ?ws == True
    guard $ getL ws p

    -- construct the resulting state
    let pBeg = getL beg pSpan
        pEnd = getL end pSpan
        -- TODO: is qNT correct?  see also `tryPredictWrapping`
        newGap = (pBeg, pEnd, qNT)
        q' = setL state j
           . setL (end . spanA) pEnd
           . modL' (gaps . spanA) (S.insert newGap)
           $ q
    -- compute the amortized weight of item `p`
    amortWeight <- lift . lift $ amortizedWeight p
    -- push the resulting state into the waiting queue
    let newBeta = addWeight (duoBeta qw) tranCost
        -- TODO: `duaGap qw` should be equal to `0`?
        -- newGap = sum [duoGap qw, duoBeta pw, duoGap pw, amortWeight]
        newGap = safeInsert pBeg
          (duoBeta pw + sum (M.elems (duoGap pw)) + amortWeight)
          (duoGap qw)
        newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
    -- push the resulting state into the waiting queue
    -- liftIO $ putStrLn "==>> PW !!" 
    lift $ pushInduced q' newDuo (PredictWrapping p q tranCost)
#ifdef CheckMonotonic
    lift . lift $ testMono "PREDICT-WRAPPING'" (p, pw) (q, qw) (q', newDuo)
#endif
#ifdef DebugOn
    -- print logging information
    hype <- RWS.get
    liftIO $ do
        endTime <- Time.getCurrentTime
        putStr "[P'] " >> printPassive p hype
        putStr "  +  " >> printActive q
        putStr "  :  " >> printActive q'
        putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
#endif


--------------------------------------------------
-- COMPLETE WRAPPING
--
-- UPDATE 20.02.2020: new rule!
--------------------------------------------------


-- | Wrap a fully-parsed tree represented by `q` over a partially parsed
-- tree represented by `p`.
tryCompleteWrapping
  :: (SOrd t, SOrd n)
  => Passive n t
  -> DuoWeight
  -> EarleyPipe n t ()
tryCompleteWrapping q qw = void $ P.runListT $ do
#ifdef DebugOn
    begTime <- liftIO $ Time.getCurrentTime
#endif
    -- let qLab = q ^. label
    let qDID = q ^. dagID
        qSpan = q ^. spanP
    -- the underlying dag grammar
    dag <- RWS.gets (gramDAG . automat)
    parMap <- RWS.gets (dagParMap . automat)
    -- make sure the label is top-level
    guard $ DAG.isRoot qDID dag
    -- for each available gap
    gap@(gapBeg, gapEnd, gapNT) <- each . S.toList $ qSpan ^. gaps
    -- TODO: add desc
    qNonTerm <- some (nonTerm' qDID dag)
    -- take all passive items with a given span and a given
    -- root non-terminal (IDs irrelevant)
    (p, pw) <- rootSpan gapNT (gapBeg, gapEnd)
    -- make sure that `p` has ?ws == True
    guard $ getL ws p
    -- verify the label of the parent
    parIdSet <- some $ M.lookup (p ^. dagID) parMap
    if (S.size parIdSet > 1)
       then error "tryCompleteWrapping: d-daughter node with several parents"
       else return ()
    parID <- each (S.toList parIdSet)
    -- TODO: implement line below in terms of `nonTerm'`?
    parNonTerm <- some (labNonTerm =<< DAG.label parID dag)
    guard $ qNonTerm == parNonTerm
    -- check the operation w.r.t. the dependency info
    Just depCost <- lift $ omega (q ^. dagID) (p ^. dagID)
    -- calculate the new set of gaps
    let newGaps = S.union (p ^. spanP ^. gaps)
                . S.delete gap
                $ qSpan ^. gaps
    -- construct the resulting item
    let p' = setL (spanP >>> beg) (qSpan ^. beg)
           . setL (spanP >>> end) (qSpan ^. end)
           . setL (spanP >>> gaps) newGaps
           . setL ws False
           $ p
    -- calculate the resulting weights
    let newBeta = sumWeight [duoBeta pw, duoBeta qw, depCost]
        -- newGap = duoGap qw
        newGap = disjointUnion
          (safeDelete gapBeg (duoGap qw))
          (duoGap pw)
        newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
    -- push the resulting state into the waiting queue
    lift $ pushPassive p' newDuo (CompleteWrapping q p depCost)
#ifdef CheckMonotonic
    lift . lift $ testMono' "COMPLETE-WRAPPING" (p, pw) (q, qw) (p', newDuo)
--     totalP <- lift . lift $ est2total pw <$> estimateDistP p
--     totalQ <- lift . lift $ est2total qw <$> estimateDistP q
--     totalQ' <- lift . lift $ est2total newDuo <$> estimateDistP p'
--     let tails =  [totalP, totalQ]
--     when (any (totalQ' + epsilon <) tails) $ do
--       P.liftIO . putStrLn $
--         "[COMPLETE-WRAPPING: MONOTONICITY TEST FAILED] TAILS: " ++ show tails ++
--         ", HEAD: " ++ show totalQ'
#endif
#ifdef DebugOn
    hype <- RWS.get
    liftIO $ do
        endTime <- Time.getCurrentTime
        putStr "[C]  " >> printPassive q hype
        putStr "  +  " >> printPassive p hype
        putStr "  :  " >> printPassive p' hype
        putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
#endif


-- | Alternative version of `tryCompleteWrapping`.
tryCompleteWrapping'
  :: (SOrd t, SOrd n)
  => Passive n t
  -> DuoWeight
  -> EarleyPipe n t ()
tryCompleteWrapping' p pw = void $ P.runListT $ do
#ifdef DebugOn
    begTime <- liftIO $ Time.getCurrentTime
#endif
    let pDID = p ^. dagID
        pSpan = p ^. spanP

    -- the underlying dag grammar
    dag <- RWS.gets (gramDAG . automat)
    parMap <- RWS.gets (dagParMap . automat)

    -- make sure that `p` has ?ws == True
    guard $ getL ws p

    -- take all passive items with the corresponding gap
    pNonTerm <- some (nonTerm' pDID dag)
    let gap = (pSpan ^. beg, pSpan ^. beg, pNonTerm)
    (q, qw) <- withGap gap

    -- local names
    let qDID = q ^. dagID
        qSpan = q ^. spanP

    -- make sure `q` is top-level
    guard $ DAG.isRoot qDID dag

    -- determine q's non-terminal
    qNonTerm <- some (nonTerm' qDID dag)

    -- verify the label of the parent
    parIdSet <- some $ M.lookup (p ^. dagID) parMap
    if (S.size parIdSet > 1)
       then error "tryCompleteWrapping': d-daughter node with several parents"
       else return ()
    parID <- each (S.toList parIdSet)
    -- TODO: implement line below in terms of `nonTerm'`?
    parNonTerm <- some (labNonTerm =<< DAG.label parID dag)
    guard $ qNonTerm == parNonTerm

    -- check the operation w.r.t. the dependency info
    Just depCost <- lift $ omega (q ^. dagID) (p ^. dagID)
    -- calculate the new set of gaps
    let newGaps = S.union (p ^. spanP ^. gaps)
                . S.delete gap
                $ qSpan ^. gaps
    -- construct the resulting item
    let p' = setL (spanP >>> beg) (qSpan ^. beg)
           . setL (spanP >>> end) (qSpan ^. end)
           . setL (spanP >>> gaps) newGaps
           . setL ws False
           $ p
    -- calculate the resulting weights
    let newBeta = sumWeight [duoBeta pw, duoBeta qw, depCost]
        newGap = disjointUnion
          (safeDelete (pSpan ^. beg) (duoGap qw))
          (duoGap pw)
        newDuo = DuoWeight {duoBeta = newBeta, duoGap = newGap}
    -- push the resulting state into the waiting queue
    lift $ pushPassive p' newDuo (CompleteWrapping q p depCost)

#ifdef CheckMonotonic
    lift . lift $ testMono' "COMPLETE-WRAPPING'" (p, pw) (q, qw) (p', newDuo)
#endif
#ifdef DebugOn
    hype <- RWS.get
    liftIO $ do
        endTime <- Time.getCurrentTime
        putStr "[C]  " >> printPassive q hype
        putStr "  +  " >> printPassive p hype
        putStr "  :  " >> printPassive p' hype
        putStr "  @  " >> print (endTime `Time.diffUTCTime` begTime)
#endif


---------------------------
-- Extracting Parsed Trees
---------------------------


-- | Extract the set of the parsed trees w.r.t. to the given active item.
fromActive
  :: (SOrd n, Ord t)
  => Active n
  -> Hype n t
  -> [[T.Tree (Maybe n) (Maybe (Tok t))]]
fromActive active hype =
  case activeTrav active hype of
    Nothing  -> case Q.lookup (ItemA active) (waiting hype) of
      Just _  -> error $
        "fromActive: active item in the waiting queue"
        ++ "\n" ++ show active
      Nothing -> error $
        "fromActive: unknown active item (not even in the queue)"
        ++ "\n" ++ show active
    Just ext -> if S.null (prioTrav ext)
        then [[]]
        else concatMap
            (fromActiveTrav active)
            (S.toList (prioTrav ext))
  where
    fromActiveTrav _p (Scan q t _) =
        [ T.Leaf (Just t) : ts
        | ts <- fromActive q hype ]
    fromActiveTrav _p (Empty q _) =
        [ T.Leaf Nothing : ts
        | ts <- fromActive q hype ]
--     fromActiveTrav _p (Foot q x _) =
--         [ T.Branch x [] : ts
--         | ts <- fromActive q hype ]
    fromActiveTrav _p (Subst qp qa _) =
        [ t : ts
        | ts <- fromActive qa hype
        , t  <- fromPassive qp hype ]
    fromActiveTrav _p (SisterAdjoin qp qa _) =
        [ ts' ++ ts
        | ts  <- fromActive qa hype
        , ts' <- T.subTrees <$> fromPassive qp hype ]
    fromActiveTrav _p (PredictWrapping qp qa _) =
        [ T.Branch (Just (nonTermH (qp ^. dagID) hype)) [] : ts
        | ts <- fromActive qa hype ]
    fromActiveTrav _ _ =
        error "fromActive: impossible fromActiveTrav"


-- | Extract the set of the parsed trees w.r.t. to the given passive item.
fromPassive
  :: (SOrd n, Ord t)
  => Passive n t
  -> Hype n t
  -> [T.Tree (Maybe n) (Maybe (Tok t))]
fromPassive passive hype = concat
  [ fromPassiveTrav passive trav
  | ext <- maybeToList $ passiveTrav passive hype
  , trav <- S.toList (prioTrav ext) ]
  where
--     fromPassiveTrav _p (Adjoin qa qm _) =
--         [ replaceFoot ini aux
--         | aux <- fromPassive qa hype
--         , ini <- fromPassive qm hype ]
    fromPassiveTrav _p (CompleteWrapping qa qm _) =
      [ replaceSlot ini aux
      | aux <- rmRoot <$> fromPassive qa hype
      , ini <- fromPassive qm hype ]
    fromPassiveTrav p (Deactivate q _) =
--       | dEdgeParent (p ^. dagID) hype = do
--           [t] <- fromActive q hype
--           return t
--       | otherwise =
      [ T.Branch
          (Just (nonTermH (p ^. dagID) hype))
          (reverse ts)
      | ts <- fromActive q hype
      ]
    -- processDEdge
    fromPassiveTrav _ _ =
        error "fromPassive: impossible fromPassiveTrav"
    -- Replace foot (the only non-terminal leaf) by the given initial tree.
    -- Mark the root as to-be-removed
    rmRoot x@T.Branch{} = x {T.labelI = Nothing}
    rmRoot x@T.Leaf{} = x


-- | Replace the first "slot" (substitution node) in the second tree with the
-- first tree.
replaceSlot :: T.Tree n t -> T.Tree n t -> T.Tree n t
replaceSlot plug tree =
  State.evalState (go tree) True
  where
    go t@T.Leaf{} = return t
    go t@(T.Branch _ []) = do
      flag <- State.get
      if flag 
         then do
           State.put False
           return plug
         else do
           return t
    go (T.Branch x ts) = T.Branch x <$> mapM go ts

-- replaceSlot t (T.Branch _ []) = t
-- replaceSlot t (T.Branch x ts) = T.Branch x $ map (replaceSlot t) ts
-- replaceSlot _ t@(T.Leaf _)    = t


-- | Extract the set of parsed trees obtained on the given input
-- sentence.  Should be run on the result of the earley parser.
parsedTrees
    :: forall n t. (SOrd n, Ord t)
    => Hype n t     -- ^ Final state of the earley parser
    -> S.Set n      -- ^ The start symbol set
    -> Int          -- ^ Length of the input sentence
    -> [T.Tree n (Maybe (Tok t))]
parsedTrees hype start n
  = map process
  . concatMap (`fromPassive` hype)
  $ finalFrom start n hype
  where
    process t =
      case removeRedundant t of
        [t'] -> t'
        _ -> error "AStar.parsedTrees: no non-terminal in the root"


-- | Remove redundant non-terminal nodes.
removeRedundant :: T.Tree (Maybe n) t -> [T.Tree n t]
removeRedundant = go
  where
    go T.Branch{..} =
      case labelI of
        Nothing -> subTrees'
        Just nt -> [T.Branch nt subTrees']
      where
        subTrees' = concatMap go subTrees
    go (T.Leaf x) = [T.Leaf x]


--------------------------------------------------
-- EARLEY
--------------------------------------------------


-- | Does the given grammar generate the given sentence from the given
-- non-terminal symbol (i.e. from an initial tree with this symbol in its root)?
-- Uses the `earley` algorithm under the hood.
recognizeFrom
    :: (SOrd t, SOrd n)
    => Memo.Memo t             -- ^ Memoization strategy for terminals
    -> [ ( O.Tree n (Maybe t)
         , Weight ) ]          -- ^ Weighted grammar
    -> S.Set n              -- ^ The start symbol set
    -> M.Map t Int          -- ^ Position map
    -> M.Map Int (M.Map Int Weight)
                            -- ^ Head map
    -> Input t              -- ^ Input sentence
    -> IO Bool
-- recognizeFrom memoTerm gram dag termWei start input = do
recognizeFrom memoTerm gram start posMap hedMap input = do
    let auto = mkAuto memoTerm (DAG.mkGram gram) input posMap hedMap
--     mapM_ print $ M.toList (DAG.nodeMap $ gramDAG auto)
--     putStrLn "========="
--     mapM_ print $ A.allEdges (A.fromWei $ gramAuto auto)
--     putStrLn "========="
    recognizeFromAuto auto start input


--------------------------------------------------
-- Parsing with automaton
--------------------------------------------------


-- | See `recognizeFrom`.
recognizeFromAuto
    :: (SOrd t, SOrd n)
    => Auto n t         -- ^ Grammar automaton
    -> S.Set n          -- ^ The start symbol set
    -> Input t          -- ^ Input sentence
    -> IO Bool
recognizeFromAuto auto start input = do
    hype <- earleyAuto auto input
    -- let n = V.length (inputSent input)
    let n = length (inputSent input)
    return $ (not.null) (finalFrom start n hype)


-- | See `earley`.
earleyAuto
    :: (SOrd t, SOrd n)
    => Auto n t         -- ^ Grammar automaton
    -> Input t          -- ^ Input sentence
    -> IO (Hype n t)
-- earleyAuto auto input = P.runEffect $
--   earleyAutoP auto input >-> P.drain
earleyAuto auto input = earleyAutoP auto input P.drain
--   fst <$> RWS.evalRWST
--     (P.runEffect (earleyAutoGen >-> P.drain))
--     input (mkHype auto)


earleyAutoP 
  :: (SOrd n, SOrd t) 
  => Auto n t
  -> Input t
  -- -> P.Proxy () (HypeModif n t) () P.X (Earley n t) (Hype n t)
  -> P.Consumer (HypeModif n t) (Earley n t) (Hype n t)
  -> IO (Hype n t)
earleyAutoP auto input consumer =
  fst <$> RWS.evalRWST
    (P.runEffect (earleyAutoGen >-> consumer))
    input (mkHype auto)


-- | Produce the constructed items (and the corresponding hypergraphs) on the
-- fly. See also `earley`.
earleyAutoGen
    :: (SOrd t, SOrd n)
    => EarleyPipe n t (Hype n t)
earleyAutoGen =
  init >> loop
  where
    -- initialize hypergraph with initial active items
    init = P.runListT $ do
      -- input length
      n <- lift $ length <$> RWS.asks inputSent
      auto <- lift $ RWS.gets automat
      root <- each . S.toList
            . A.roots . A.fromWei
            . gramAuto $ auto
      i    <- each [0 .. n - 1]
      let q = Active root Span
              { _beg   = i
              , _end   = i
              , _gaps  = S.empty }
      lift $ pushActive q (DuoWeight zeroWeight M.empty) Nothing
    -- the computation is performed as long as the waiting queue
    -- is non-empty.
    loop = lift popItem >>= \mp -> case mp of
        Nothing -> RWS.get
        Just p  -> do
#ifdef DebugOn
          let item :-> e = p
          hype <- RWS.get
          liftIO $ do
            putStr "POP: " >> printItem item hype
            putStr " :>  " >> print (priWeight e, gapWeight e, estWeight e)
#endif
          step p >> loop


--------------------------------------------------
-- Earley step
--------------------------------------------------


-- | Step of the algorithm loop.  `p' is the state popped up from
-- the queue.
step
    :: (SOrd t, SOrd n)
    => Binding (Item n t) (ExtWeight n t)
    -> EarleyPipe n t ()
step (ItemP p :-> e) = do
    -- TODO: consider moving before the inference applications
    -- UPDATE: DONE
    lift $ savePassive p e
    yieldModif $ \hype -> HypeModif
      { modifHype = hype
      , modifType = NewNode
      , modifItem = ItemP p
      , modifTrav = e}
    mapM_ (\f -> f p $ duoWeight e)
      [ trySubst
      , tryPseudoSubst
--       , tryAdjoinInit
--       , tryAdjoinTerm
--       , tryAdjoinTerm'
      , trySisterAdjoin
      , tryPredictWrapping
      , tryCompleteWrapping
      , tryCompleteWrapping'
      -- , trySubstPS
      -- , tryAdjoinCont
      ]
step (ItemA p :-> e) = do
    -- TODO: consider moving before the inference applications
    -- UPDATE: DONE
    lift $ saveActive p e
    yieldModif $ \hype -> HypeModif
      { modifHype = hype
      , modifType = NewNode
      , modifItem = ItemA p
      , modifTrav = e }
    mapM_ (\f -> f p $ duoWeight e)
      [ tryScan
      , tryEmpty
      , tryDeactivate
      , trySubst'
      , tryPseudoSubst'
--       , tryAdjoinInit'
      , trySisterAdjoin'
      , tryPredictWrapping'
      -- , trySubstPS'
      -- , tryAdjoinCont'
      ]


--------------------------------------------------
-- New utilities
--------------------------------------------------


-- | Return the corresponding set of traversals for an active item.
activeTrav
  :: (Ord n, Ord t)
  => Active n
  -> Hype n t
  -> Maybe (ExtWeight n t)
activeTrav p h = Chart.activeTrav p (chart h)


-- | Return the corresponding set of traversals for a passive item.
passiveTrav
  :: (Ord n, Ord t)
  => Passive n t
  -> Hype n t
  -> Maybe (ExtWeight n t)
passiveTrav p h = Chart.passiveTrav p (automat h) (chart h)


-- | Return the list of final, initial, passive chart items.
finalFrom
    :: (Ord n, Eq t)
    => S.Set n      -- ^ The start symbol set
    -> Int          -- ^ The length of the input sentence
    -> Hype n t     -- ^ Result of the earley computation
    -> [Passive n t]
finalFrom startSet n hype =
  Chart.finalFrom startSet n (automat hype) (chart hype)


-- -- -- | Return the list of final passive chart items.
-- -- final
-- --     :: (Ord n, Eq t)
-- --     -> Int          -- ^ The length of the input sentence
-- --     -> Hype n t    -- ^ Result of the earley computation
-- --     -> [Passive n t]
-- -- final start n Hype{..} =
-- --     case M.lookup (0, start, n) donePassive of
-- --         Nothing -> []
-- --         Just m ->
-- --             [ p
-- --             | p <- M.keys m
-- --             , p ^. label == NonT start Nothing ]
--
--
-- -- | Check whether the given sentence is recognized
-- -- based on the resulting state of the earley parser.
-- isRecognized
--     :: (SOrd t, SOrd n)
--     => Input t           -- ^ Input sentence
--     -> Hype n t          -- ^ Earley parsing result
--     -> Bool
-- isRecognized input Hype{..} =
--     (not . null)
--     (complete
--         (agregate donePassive))
--   where
--     n = V.length (inputSent input)
--     complete done =
--         [ True | item <- S.toList done
--         , item ^. spanP ^. beg == 0
--         , item ^. spanP ^. end == n
--         , isNothing (item ^. spanP ^. gap)
--         -- admit only *fully* recognized trees
--         , isRoot (item ^. label) ]
--     agregate = S.unions . map M.keysSet . M.elems
--     isRoot (NonT _ Nothing) = True
--     isRoot _ = False


--------------------------------------------------
-- Utilities
--------------------------------------------------


-- | Strict modify (in mtl starting from version 2.2).
modify' :: RWS.MonadState s m => (s -> s) -> m ()
modify' f = do
  x <- RWS.get
  RWS.put $! f x


-- -- | MaybeT transformer.
-- maybeT :: Monad m => Maybe a -> MaybeT m a
-- maybeT = MaybeT . return


-- | ListT from a list.
each :: Monad m => [a] -> P.ListT m a
each = P.Select . P.each


-- | ListT from a maybe.
some :: Monad m => Maybe a -> P.ListT m a
some = each . maybeToList


-- -- | Is the rule with the given head top-level?
-- topLevel :: Lab n t -> Bool
-- topLevel x = case x of
--     NonT{..}  -> isNothing labID
--     AuxRoot{} -> True
--     _         -> False


-- | Get a range of the given list.
over :: Pos -> Pos -> [a] -> [a]
over i j = take (j - i) . drop i


-- -- | Take the non-terminal of the underlying DAG node.
-- nonTerm :: Either (NotFoot n) DID -> Hype n t -> n
-- nonTerm i = Base.nonTerm i . automat


-- | Take the non-terminal of the underlying DAG node.
nonTermH :: DID -> Hype n t -> n
nonTermH i = Item.nonTerm i . automat


-- -- | Check if the given DAG node is a "d-parent".
-- dEdgeParent :: DID -> Hype n t -> Bool
-- dEdgeParent i hype =
--   case DAG.children i dag of
--     [j] -> DAG.isDNode j dag
--     _ -> False
--   where
--     dag = gramDAG (automat hype)


--------------------------------------------------
-- Testing monotonicity
--------------------------------------------------


-- | Total weight form the duo-weight and the corresponding estimated weight.
est2total :: DuoWeight -> Weight -> Weight
est2total duo = totalWeight . extWeight0 duo


-- | Epsilon to compare small values.
epsilon :: Weight
epsilon = 1e-10


#ifdef CheckMonotonic
testMono
    :: (SOrd n, SOrd t)
    => String
    -> (Passive n t, DuoWeight)
    -> (Active n, DuoWeight)
    -> (Active n, DuoWeight)
    -> Earley n t ()
testMono opStr (p, pw) (q, qw) (q', newDuo) = do
    distP <- estimateDistP p
    dist'P <- estimateDistP' p
    distQ <- estimateDistA q
    dist'Q <- estimateDistA' q
    distQ' <- estimateDistA q'
    dist'Q' <- estimateDistA' q'
    let totalP = est2total pw distP
        totalQ = est2total qw distQ
        totalQ' = est2total newDuo distQ'
    let tails = [totalP, totalQ]
    when (any (totalQ' + epsilon <) tails) $ do
      hype <- RWS.get
      amortWeightP <- amortizedWeight p
      amortWeightQ <- amortizedWeight' q
      amortWeightQ' <- amortizedWeight' q'
      P.liftIO $ do
        putStrLn $ "[" ++ opStr ++ ": MONOTONICITY TEST FAILED]" ++
          " TAILS: " ++ show tails ++
          ", HEAD: " ++ show totalQ'

        putStrLn "TAILS:"

        printPassive p hype
        putStr " => "
        putStrLn $ show (pw, dist'P)
        putStr " => amortized weight: "
        putStrLn $ show amortWeightP

        printActive q
        putStr " => "
        putStrLn $ show (qw, dist'Q)
        putStr " => amortized weight: "
        putStrLn $ show amortWeightQ

        putStrLn "HEAD:"
        printActive q'
        putStr " => "
        putStrLn $ show (newDuo, dist'Q')
        putStr " => amortized weight: "
        putStrLn $ show amortWeightQ'
#endif


#ifdef CheckMonotonic
testMono'
    :: (SOrd n, SOrd t)
    => String
    -> (Passive n t, DuoWeight)
    -> (Passive n t, DuoWeight)
    -> (Passive n t, DuoWeight)
    -> Earley n t ()
testMono' opStr (p, pw) (q, qw) (p', newDuo) = do
  totalP <- est2total pw <$> estimateDistP p
  totalQ <- est2total qw <$> estimateDistP q
  totalQ' <- est2total newDuo <$> estimateDistP p'
  let tails =  [totalP, totalQ]
  when (any (totalQ' + epsilon <) tails) $ do
    P.liftIO $ do
      putStrLn $ "[" ++ opStr ++ ": MONOTONICITY TEST FAILED]" ++
        " TAILS: " ++ show tails ++
        ", HEAD: " ++ show totalQ'
#endif
