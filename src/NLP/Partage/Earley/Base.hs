module NLP.Partage.Earley.Base
(
-- * Core Types
  Pos
, NotFoot (..)
-- * Input
, Input (..)
, fromList
, fromSets
-- * Utils
, nonTerm
, nonTerm'
, labNonTerm
) where


import qualified Data.Set as S
import qualified Data.Vector as V

import qualified NLP.Partage.DAG             as DAG
import qualified NLP.Partage.Tree.Other      as O
import           NLP.Partage.Earley.Auto
  (Auto(..), NotFoot(..))


--------------------------------------------------
-- Core Types
--------------------------------------------------


-- | A position in the input sentence.
type Pos = Int


--------------------------------------------------
-- Input
--------------------------------------------------


-- | Input of the parser.
newtype Input t = Input {
      inputSent :: V.Vector (S.Set t)
    -- ^ The input sentence
    }


-- | Construct `Input` from a list of terminals.
fromList :: [t] -> Input t
fromList = fromSets . map S.singleton


-- | Construct `Input` from a list of sets of terminals, each set
-- representing all possible interpretations of a given word.
fromSets :: [S.Set t] -> Input t
fromSets xs = Input (V.fromList xs)


--------------------------------------------------
-- Utils
--------------------------------------------------


-- -- | Take the non-terminal of the underlying DAG node.
-- nonTerm :: Either n DID -> Hype n t -> n
-- nonTerm i hype =
--     case i of
--         Left rootNT -> rootNT
--         Right did   -> check $
--             DAG.label did (gramDAG $ automat hype)
--   where
--     check Nothing  = error "nonTerm: not a non-terminal ID"
--     check (Just x) = x


-- | Take the non-terminal of the underlying DAG node.
nonTerm :: Either (NotFoot n) DAG.DID -> Auto n t -> n
nonTerm i =
    check . nonTerm' i . gramDAG
  where
    check Nothing  = error "nonTerm: not a non-terminal ID"
    check (Just x) = x


-- | Take the non-terminal of the underlying DAG node.
nonTerm' :: Either (NotFoot n) DAG.DID -> DAG.DAG (O.Node n t) () -> Maybe n
nonTerm' i dag = case i of
    Left root -> Just (notFootLabel root)
    Right did -> labNonTerm =<< DAG.label did dag


--------------------------------------------------
-- Utils
--------------------------------------------------


-- | Take the non-terminal of the underlying DAG node.
labNonTerm :: O.Node n t -> Maybe n
labNonTerm (O.NonTerm y) = Just y
labNonTerm (O.Sister y) = Just y
labNonTerm (O.DNode y) = Just y
-- labNonTerm (O.Foot y) = Just y
labNonTerm _ = Nothing
