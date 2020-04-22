{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}


module NLP.Partage.AStar.Item
  ( Span (..)
  , beg
  , end
  , gaps
  , Active (..)
  , state
  , spanA
  , Passive (..)
  , dagID
  , spanP
  , ws
--   , isAdjoinedTo
--   , regular
--   , auxiliary
  , noGaps
--   , isRoot

-- #ifdef DebugOn
  , printActive
  , printPassive
-- #endif

  -- * Provisional
  , nonTerm
  )
where


import           Control.Monad (forM_)

import           Data.Lens.Light
-- import           Data.Maybe             (isJust, isNothing)
import           Prelude                hiding (span)
import qualified Data.Set as S

import           Data.DAWG.Ord          (ID)

import           NLP.Partage.AStar.Base (Pos)
import           NLP.Partage.DAG        (DID)
import qualified NLP.Partage.DAG as DAG

import           NLP.Partage.AStar.Base (nonTerm', isSister')
import           NLP.Partage.AStar.Auto (Auto (..))


data Span n = Span {
    -- | The starting position.
      _beg :: Pos
    -- | The ending position (or rather the position of the dot).
    , _end :: Pos
    -- | Set of labeled of the gaps (if any)
    , _gaps  :: S.Set (Pos, Pos, n)
    } deriving (Show, Eq, Ord)
$( makeLenses [''Span] )


-- | Active chart item : state reference + span.
data Active n = Active {
      _state :: ID
    , _spanA :: Span n
    } deriving (Show, Eq, Ord)
$( makeLenses [''Active] )


-- | Passive chart item : label + span.
data Passive n t = Passive
  { _dagID :: DID
    -- ^ The `DID` of the elementary tree node
  , _spanP :: Span n
    -- ^ Span of the chart item
  , _ws :: Bool
    -- ^ TODO: see the inference rules
  } deriving (Show, Eq, Ord)
$( makeLenses [''Passive] )


-- | Has no gaps
noGaps :: Span n -> Bool
noGaps = S.null . getL gaps


-- -- | Does it represent auxiliary rules?
-- auxiliary :: Span -> Bool
-- auxiliary = isJust . getL gap


-- -- | Does it represent a root?
-- isRoot :: Either n DID -> Bool
-- isRoot x = case x of
--     Left _  -> True
--     Right _ -> False


-- #ifdef DebugOn
-- | Print an active item.
printSpan :: (Show n) => Span n -> IO ()
printSpan span = do
    putStr . show $ getL beg span
    putStr ", "
    putStr . show $ getL end span
    putStr ", ["
    forM_ (S.toList $ getL gaps span) $ \(p, q, x) -> do
        putStr $ show p
        putStr ", "
        putStr $ show q
        putStr ", "
        putStr $ show x
        putStr "; "
    putStr "]"


-- | Print an active item.
printActive :: (Show n) => Active n -> IO ()
printActive p = do
    putStr "("
    putStr . show $ getL state p
    putStr ", "
    printSpan $ getL spanA p
    putStrLn ")"


-- | Print a passive item.
printPassive :: (Show n, Show t) => Passive n t -> Auto n t -> IO ()
printPassive p auto = do
    let did = getL dagID p
    putStr "("
    putStr $
      show (DAG.unDID did) ++ "[" ++
      show (nonTerm did auto) ++ "]"
    putStr ", "
    putStr $ "root=" ++ show (DAG.isRoot did (gramDAG auto))
    putStr ", "
    putStr $ "sister=" ++ show (isSister' did (gramDAG auto))
    putStr ", "
    printSpan $ getL spanP p
    putStrLn ")"
-- #endif


-- | Take the non-terminal of the underlying DAG node.
nonTerm :: DAG.DID -> Auto n t -> n
nonTerm i =
    check . nonTerm' i . gramDAG
  where
    check Nothing  = error "nonTerm: not a non-terminal ID"
    check (Just x) = x
