{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_)
import Control.Arrow (first)

import Data.Lens.Light

import qualified Data.Set                 as S
import qualified Data.Map.Strict          as M
import qualified Data.Tree                as R
import qualified Data.MemoCombinators     as Memo

import qualified NLP.Partage.AStar        as E
import qualified NLP.Partage.AStar.Base   as Base
import qualified NLP.Partage.AStar.Item   as Item
import qualified NLP.Partage.AStar.Chart  as Chart
import qualified NLP.Partage.Auto.WeiTrie as W
-- import           NLP.Partage.FactGram   (flattenWithSharing)
import qualified NLP.Partage.DAG          as DAG
import qualified NLP.Partage.Tree.Other   as O


-- | Non-terminal and terminal types.
type N = String
type T = String


-- | Memoization for terminals.
memoTerm = Memo.list Memo.char


-- | Some smart constructors.
node x = R.Node (O.NonTerm x)
leaf x = R.Node (O.NonTerm x) []
term x = R.Node (O.Term x) []
foot x = R.Node (O.Foot x) []


-- | A sample TAG grammar.
trees =
  [ node "NP"
    [ node "D" [term "the"]
    , foot "NP"
    ]
  , node "N"
    [ node "A" [term "prime"]
    , foot "N"
    ]
  , node "NP" [node "N" [term "minister"]]
  , node "S"
    [ leaf "NP"
    , node "VP"
      [ node "V" [term "made"]
      , leaf "NP" ]
    ]
  , node "NP" [node "N" [term "decisions"]]
  , node "NP"
    [ node "D" [term "a"]
    , foot "NP"
    ]
  , node "NP"
    [ node "D" [term "few"]
    , foot "NP"
    ]
  , node "N"
    [ node "D" [term "good"]
    , foot "N"
    ]
  , node "NP"
    [ node "A" [term "prime"]
    , node "N" [term "minister"]
    ]
  , node "NP"
    [ node "D" [term "a"]
    , node "D" [term "few"]
    , foot "NP"
    ]
  , node "S"
    [ leaf "NP"
    , node "VP"
      [ node "V" [term "made"]
      , node "NP" [node "N" [term "decisions"]]
      ]
    ]
  ]


data Arc = Arc
  { from :: S.Set (E.Item N T)
  , to :: E.Item N T
  , typ :: String
  } deriving (Show, Eq, Ord)


arcsFrom :: E.Item N T -> E.ExtWeight N T -> [Arc]
arcsFrom hd ext =
  [ Arc {from = S.fromList src, to = hd, typ = arcTyp}
  | trav <- S.toList (E.prioTrav ext)
  , let (src, arcTyp) = tails trav ]
  where
    tails trav = case trav of
      E.Scan{..} -> ([E.ItemA scanFrom], "SC")
      E.Subst{..} -> ([E.ItemA actArg, E.ItemP passArg], "SU")
      E.Foot{..} -> ([E.ItemA actArg], "FA")
      E.Adjoin{..} -> ([E.ItemP passAdj, E.ItemP passMod], "AD")


printHype :: E.Hype N T -> IO ()
printHype hype = do
  -- let ts = E.parsedTrees hype start (length sent)
  -- forM_ ts $ putStrLn . R.drawTree . fmap show . O.unTree
  let passive = map (first E.ItemP) . Chart.listPassive $ E.chart hype
      active = map (first E.ItemA) . Chart.listActive $ E.chart hype
      nodes = map fst active ++ map fst passive
      nodeMap = M.fromList $ zip [0..] nodes
      nodeMapRev = revMap nodeMap
      edges = concat . map (uncurry arcsFrom) $ passive ++ active
      regularNodeNum = M.size nodeMap
      edgeMap = M.fromList $ zip [regularNodeNum+1..] edges
      edgeMapRev = revMap edgeMap
  putStrLn "digraph {"
  forM_ (M.toList nodeMap) $ \(nodeID, node) -> do
    putStrLn $ "  " ++ show nodeID
      ++ " [label=\"" ++ showItem (E.automat hype) node ++ "\"]"
      ++ ";"
  forM_ (M.toList edgeMap) $ \(edgeID, edge) -> do
    putStrLn $ "  " ++ show edgeID
      ++ " [label=\"" ++ typ edge ++ "\", shape=diamond]"
      ++ ";"
  forM_ (M.toList edgeMap) $ \(edgeID, edge) -> do
    forM_ (S.toList $ from edge) $ \src -> do
      putStrLn $ "  " ++ show (nodeMapRev M.! src) ++ " -> " ++ show edgeID ++ ";"
    putStrLn $ "  " ++ show edgeID ++ " -> " ++ show (nodeMapRev M.! to edge) ++ ";"
  putStrLn "}"


analyzeSent :: DAG.Gram N T -> T -> [T] -> IO ()
analyzeSent gram start sent = do
  let input = E.fromList sent
      auto = E.mkAuto memoTerm gram
      -- recognize s = E.recognizeFromAuto auto s . E.fromList
  printHype =<< E.earleyAuto auto input


main = do
  let gram = DAG.mkGram (map (,1) trees)
      analyze = analyzeSent gram
  analyze "S" ["the", "prime", "minister", "made", "a", "few", "good", "decisions"]
  -- print =<< recognize "S" ["the", "prime", "minister", "made", "a", "few", "good", "decisions"]


-----------------------
-- Utils
-----------------------


revMap :: Ord b => M.Map a b -> M.Map b a
revMap =
  let swap (x, y) = (y, x)
  in  M.fromList . map swap . M.toList



showItem :: E.Auto N T -> E.Item N T -> String
showItem auto (E.ItemP x) = showPassive auto x
showItem _ (E.ItemA x) = showActive x


showActive :: E.Active -> String
showActive x = concat
  [ "("
  , show $ getL Item.state x
  , ", "
  , showSpan $ getL Item.spanA x
  , ")"
  ]


showPassive :: E.Auto N T -> E.Passive N T -> String
showPassive auto x = concat
  [ "("
  , case getL Item.dagID x of
        Left rootNT -> rootNT
        Right did   ->
          Base.nonTerm (getL Item.dagID x) auto
          ++ "[" ++ show (DAG.unDID did)  ++ "]"
  , ", "
  , showSpan $ getL Item.spanP x
  , ")"
  ]


showSpan :: Item.Span -> String
showSpan span = concat
  [ show $ getL Item.beg span
  , ", "
  , case getL Item.gap span of
      Nothing -> ""
      Just (p, q) -> concat
          [ show p
          , ", "
          , show q
          , ", " ]
  , show $ getL Item.end span ]