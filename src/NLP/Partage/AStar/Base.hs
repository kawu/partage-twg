module NLP.Partage.AStar.Base
  ( Pos
  -- , NotFoot (..)
  -- * Input
  , Input (..)
  , Tok (..)
  , fromList
  , mkInput
  -- * Utils
  -- , nonTerm
  , nonTerm'
  , isSister'
  , isDNode'
  , labNonTerm
  )
where


import           Data.Function              (on)
import           Data.Maybe (isJust)

import qualified NLP.Partage.DAG             as DAG
import qualified NLP.Partage.Tree.Other      as O
-- import           NLP.Partage.AStar.Auto      (Auto (..), NotFoot(..))


-- | A position in the input sentence.
type Pos = Int


--------------------------------------------------
-- Input
--------------------------------------------------


-- | Input+configuration of the parser.
data Input t = Input
  { inputSent :: [Tok t]
    -- ^ The input sentence
  , keepAllArcs :: Bool
    -- ^ Keep all hyper-arcs or only the single-best ones
  , maxChartSize :: Maybe Int
    -- ^ Upper limit on the size of the chart (if any)
  }


-- | A token is a terminal enriched with information about the position
-- in the input sentence.
data Tok t = Tok
  { position :: Int
    -- ^ Position of the node in the dependency tree
  , terminal :: t
    -- ^ Terminal on the corresponding position
  } deriving (Show)


instance Eq (Tok t) where
  (==) = (==) `on` position
instance Ord (Tok t) where
  compare = compare `on` position


-- | Construct `Input` from a list of terminals.
fromList :: [t] -> Input t
fromList = mkInput True Nothing


-- | Construct `Input` from a list of terminals, info about whether all arcs
-- should be preserved, and (optional) limit on the size of the chart.
mkInput :: Bool -> Maybe Int -> [t] -> Input t
mkInput kaa sizeLimit =
    inp . map (uncurry Tok) . zip [0..]
    where
      inp xs = Input xs kaa sizeLimit


--------------------------------------------------
-- Utils
--------------------------------------------------


-- | Take the non-terminal of the underlying DAG node.
nonTerm' :: DAG.DID -> DAG.DAG (O.Node n t) w -> Maybe n
nonTerm' did dag = labNonTerm =<< DAG.label did dag


-- | Is the node marked for sister adjunction?
isSister' :: DAG.DID -> DAG.DAG (O.Node n t) w -> Bool
isSister' did dag = isJust $ do
  O.Sister _ <- DAG.label did dag
  return $ Just ()


-- | Is the node marked as d-daughter node?
isDNode' :: DAG.DID -> DAG.DAG (O.Node n t) w -> Bool
isDNode' did dag = isJust $ do
  O.DNode _ <- DAG.label did dag
  return $ Just ()


--------------------------------------------------
-- Utils
--------------------------------------------------


-- | Take the non-terminal of the underlying DAG node.
labNonTerm :: O.Node n t -> Maybe n
labNonTerm (O.NonTerm y) = Just y
labNonTerm (O.Sister y) = Just y
-- labNonTerm (O.Foot y) = Just y
labNonTerm (O.DNode y) = Just y
labNonTerm _ = Nothing
