{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TupleSections   #-}


-- | Internal automaton data type which, aparat from the automaton itself,
-- contains several other components needed for parsing.


module NLP.Partage.AStar.Auto
( Auto(..)
, mkAuto

-- * Core
, NotFoot(..)
) where


import           Data.DAWG.Ord               (ID)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (maybeToList, catMaybes)
import qualified Data.MemoCombinators        as Memo
import qualified Data.Set                    as S

import qualified NLP.Partage.Auto            as A

import           NLP.Partage.SOrd            (SOrd)
import qualified NLP.Partage.AStar.Base      as B
import qualified NLP.Partage.AStar.Heuristic as H
-- import qualified NLP.Partage.AStar.Heuristic.Base as H
import qualified NLP.Partage.Auto.WeiTrie    as Trie
import qualified NLP.Partage.Auto.WeiSet     as WS
import           NLP.Partage.DAG             (DID, Gram, Weight)
import qualified NLP.Partage.DAG             as DAG
import qualified NLP.Partage.Tree.Other      as O


--------------------------------------------------
-- Core (TODO: Should be moved out of here?
--   Besides, it's also a copy of what is in
--   Early.Auto).
--------------------------------------------------


-- | Non-terminal which is not a foot.
data NotFoot n = NotFoot
  { notFootLabel :: n
    -- ^ The corresponding non-terminal
  , isSister :: Bool
    -- ^ Is the non-terminal marked for sister-adjunction?
  } deriving (Show, Eq, Ord)


--------------------------------------------------
-- Local trie type
--------------------------------------------------


-- | Each leaf contains a set of `Maybe t`s, which corresponds to the
-- possibility of representing (i) non-determinism and (ii) empty terminal
-- symbols.
type DAG n t =
  DAG.DAG
    (O.Node n (Maybe t))
    Weight


-- | Local automaton type based on `A.GramAuto`.
data Auto n t = Auto
    { gramDAG  :: DAG n t -- DAG (O.Node n t) Weight
    -- ^ The underlying grammar DAG; the weights must be consistent
    -- with what is in the `gramAuto`
--     , isSpine  :: DID -> Bool
--     -- ^ Is the given DAG node a spine node?
    , gramAuto :: A.WeiGramAuto n t
    -- ^ The underlying grammar automaton
    , withBody :: M.Map DID (S.Set ID)
    -- ^ A data structure which, for each label, determines the set of automaton
    -- states from which this label goes out as a body transition.
    -- , termWei  :: M.Map t Weight
    -- ^ The lower bound estimates on reading terminal weights. Based on the
    -- idea that weights of the elementary trees are evenly distributed over its
    -- terminals.
    , termDID  :: M.Map (Maybe t) (S.Set DID)
    -- ^ A map which assigns DAG IDs to the corresponding terminals. Note that
    -- each grammar terminal is represented by exactly one grammar DAG node.

--     , footDID  :: M.Map n (S.Set DID)
--     -- ^ A map which assigns DAG IDs to the corresponding foot non-terminals.
--     -- Note that each grammar foot non-terminal is represented by exactly one
--     -- grammar DAG node.

    , leafDID  :: M.Map n (S.Set DID)
    -- ^ A map which assigns DAG IDs to the corresponding leaf non-terminals.
    -- Note that each grammar leaf non-terminal is represented by exactly one
    -- grammar DAG node.
    , estiCost :: H.Esti t
    -- ^ Heuristic estimations.

    , lhsNonTerm :: M.Map ID (NotFoot n)
    -- ^ A map which uniquely determines the LHS corresponding to the rule
    -- containing the given ID. WARNING: The LHS can be uniquely determined only
    -- if one has a separate FSA/Trie for each such non-terminal!

    -- <<< NEW 12.12.2018 >>>

    -- Note that the new data structures defined below do not intergrate with
    -- the rest of the code very well.  In particular, the remaining code is
    -- rather abstract and does not assume that it is possible to uniquely
    -- deterine the position corresponding to a `DID`. Indeed, this does not
    -- work with grammar compression in general.

    , anchorPos :: M.Map DID Int
    -- ^ A map which determines the position of the attachment of the tree with
    -- the given `DID`.

    , anchorPos' :: M.Map ID Int
    -- ^ A map which determines the position of the attachment of the tree with
    -- the given `ID`.

--     , headPos :: M.Map Int Int
--     -- ^ A map which tells what is the head of the given word.  Both `Int`s
--     -- refer to positions in the input sentence.
--     -- TODO: there is a `Pos` type defined, but in the `Base` module which
--     -- relies on the `Auto` module...

    -- <<< NEW 27.12.2018 >>>

    , headPos :: M.Map Int (M.Map Int Weight)
    -- ^ A map which tells what are the *potential* heads of the given word.
    -- For each such potential head, the corresponding arc (non-negative)
    -- weight is assigned.  Both `Int`s refer to positions in the input
    -- sentence.
    --
    -- If there is no `M.Map Int Weight` specified for a given position, any
    -- head will be allowed.
    -- 
    -- TODO: there is a `Pos` type defined, but in the `Base` module which
    -- relies on the `Auto` module...

--     , depEsti :: H.DepEsti
--     -- ^ Dependency-related heuristic estimations

    , dagParMap :: DAG.ParentMap
    -- ^ Need it to determine the parents of the individual DAG nodes (see the
    -- complete-wrapping rule)
    }


-- | Construct `Auto` based on the weighted grammar.
mkAuto
  -- :: (Hashable t, Ord t, Hashable n, Ord n)
  :: (SOrd t, Ord n)
  => Memo.Memo t        -- ^ Memoization strategy for terminals
  -> Gram n t
  -> B.Input t     -- ^ Input sentence
  -> M.Map t Int   -- ^ Position map
  -> M.Map Int (M.Map Int Weight)
                   -- ^ Head map
  -> Auto n t
mkAuto memoTerm gram input posMap hedMap =
    let auto = WS.fromGram Trie.fromGram (DAG.factGram gram)
        dag = DAG.dagGram gram
        lhsMap = mkLhsNonTerm dag auto
        (ancPos, ancPos') = mkAnchorPos dag (A.fromWei auto) posMap
    in  Auto
        { gramDAG  = dag
--         , isSpine  = DAG.isSpine dag
        , gramAuto = auto
        , withBody = mkWithBody auto
        -- , termWei  = DAG.termWei gram
        , termDID  = mkTermDID dag
--         , footDID  = mkFootDID dag
        , leafDID  = mkLeafDID dag
        , estiCost = H.mkEsti memoTerm gram auto input posMap hedMap
        , lhsNonTerm = lhsMap
        -- NEW 12.12.2018
        , anchorPos = ancPos
        , anchorPos' = ancPos'
        , headPos = hedMap
        , dagParMap = DAG.parentMap dag
        }


-- TODO: the code below is a copy of the code in 'Early.Auto'!


-- | Create the `withBody` component based on the automaton.
mkWithBody
    :: A.WeiGramAuto n t
    -> M.Map DID (S.Set ID)
mkWithBody dag = M.fromListWith S.union
    [ (x, S.singleton i)
    | (i, A.Body x, _j) <- A.allEdges (A.fromWei dag) ]


-- | Create the `termDID` component of the hypergraph.
mkTermDID
    :: (Ord t)
    => DAG n t
    -> M.Map (Maybe t) (S.Set DID)
mkTermDID dag = M.fromListWith S.union
    [ (t, S.singleton i)
    | i <- S.toList (DAG.nodeSet dag)
    , O.Term t <- maybeToList (DAG.label i dag) ]


-- -- | Create the `footDID` component of the hypergraph.
-- mkFootDID
--     :: (Ord n)
--     => DAG n t
--     -> M.Map n (S.Set DID)
-- mkFootDID dag = M.fromListWith S.union
--     [ (x, S.singleton i)
--     | i <- S.toList (DAG.nodeSet dag)
--     , O.Foot x <- maybeToList (DAG.label i dag) ]


-- | Create the `leafDID` component of the hypergraph.
mkLeafDID
    :: (Ord n)
    => DAG n t
    -> M.Map n (S.Set DID)
mkLeafDID dag = M.fromListWith S.union
    [ (x, S.singleton i)
    | i <- S.toList (DAG.nodeSet dag)
    , DAG.isLeaf i dag
    , O.NonTerm x <- maybeToList (DAG.label i dag) ]


-- | Create the `lhsNonTerm` component.
mkLhsNonTerm
  :: DAG n t
  -> A.WeiGramAuto n t
  -> M.Map ID (NotFoot n)
mkLhsNonTerm dag auto = M.fromList
  [ (i, pick $ lhs i)
  | i <- S.toList $ A.allIDs (A.fromWei auto)
  , (not . null) (A.edgesWei auto i) ]
  where
    -- lhs = Memo.wrap DID unDID Memo.integral lhs'
    lhs = Memo.integral lhs'
    lhs' i = concat
      [ case edge of
          A.Head did -> [label did]
          A.Body _ -> lhs j
      | (edge, _, j) <- A.edgesWei auto i
      ]
    label did =
      case DAG.label did dag >>= labNonTerm of
        Just x -> x
        Nothing -> error "Auto.mkLhsNonTerm: unknown DID"
    pick xs =
      case xs of
        [x] -> x
        _ -> error "Auto.mkLhsNonTerm: multiple LHS non-terminals per DID"
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


-- | Create the `anchorPos` and `anchorPos'` components.
--
-- TODO: copy from `Earley.Auto`
mkAnchorPos
  :: (Ord t)
  => DAG n t
  -> A.GramAuto
  -> M.Map t Int   -- ^ Position map
  -> (M.Map DID Int, M.Map ID Int)
#if ExtraCompression
mkAnchorPos _ _ _ = (M.empty, M.empty)
#else
mkAnchorPos dag auto posMap = 

  (didRes, idRes)

  where
    
    idRes = M.fromList $ catMaybes
      [ (i,) <$> pick (idOn i)
      | i <- S.toList $ A.allIDs auto
      , (not . null) (A.edges auto i) ]
    didRes = M.fromList $ catMaybes
      [ (i,) <$> pick (didOn i)
      | i <- S.toList $ DAG.nodeSet dag ]

    idOn i = nub . concat $
      [ case edge of
          A.Head did -> didOn did
          A.Body did -> didOn did
      | (edge, _) <- A.edges auto i
      ]

    didOn = Memo.wrap DAG.DID DAG.unDID Memo.integral didOn'
    didOn' did = nub $
      if DAG.isRoot did dag
         then down did 
         else concat
                [ didOn parDid
                | parDid <- S.toList $ DAG.parents did parMap
                ]

    down = Memo.wrap DAG.DID DAG.unDID Memo.integral down'
    down' did =
      case DAG.label did dag of
        -- Just (O.Term ts) -> nub . mapMaybe (flip M.lookup posMap) $ S.toList ts
        Just (O.Term (Just t)) -> maybeToList $ M.lookup t posMap
        _ -> concat [down child | child <- DAG.children did dag]

    parMap = DAG.parentMap dag

    pick xs =
      case xs of
        [x] -> Just x
        [] -> Nothing
        _ -> error $ "Auto.mkAnchorPos: multiple choices -- " ++ show xs
        -- _ -> Nothing
#endif
          
          
--------------------------------------------------
-- Utils
--------------------------------------------------


-- | Remove duplicates.
nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
