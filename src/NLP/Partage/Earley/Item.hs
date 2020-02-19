{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}


module NLP.Partage.Earley.Item
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
-- , isAdjoinedTo
-- , regular
, noGaps
-- , auxiliary
, isRoot

-- #ifdef DebugOn
, printActive
, printPassive
-- #endif
) where


import           Control.Monad (forM_)

import           Data.Lens.Light
import           Data.Maybe             (isJust, isNothing)
import           Prelude                hiding (span)

-- import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Data.DAWG.Ord          (ID)

import           NLP.Partage.Earley.Base (Pos, NotFoot(..))
import           NLP.Partage.DAG        (DID)
import qualified NLP.Partage.DAG as DAG

-- #ifdef DebugOn
import           NLP.Partage.Earley.Base (nonTerm)
import           NLP.Partage.Earley.Auto (Auto(..))
-- #endif


--------------------------------------------------
-- BASE TYPES
--------------------------------------------------


data Span n = Span {
    -- | The starting position.
      _beg   :: Pos
    -- | The ending position (or rather the position of the dot).
    , _end   :: Pos
    -- | Coordinates of the gaps (if any)
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
-- TODO: remove the redundant 't' parameter
data Passive n t = Passive
  { _dagID :: Either (NotFoot n) DID
    -- ^ We store non-terminal 'n' (actually, NonFoot n) for items representing
    -- fully recognized elementary trees.
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


-- | Does it represent a root?
isRoot :: Either n DID -> Bool
isRoot x = case x of
    Left _  -> True
    Right _ -> False


-- #ifdef DebugOn
-- | Print an active item.
printSpan :: (Show n) => Span n -> IO ()
printSpan span = do
    putStr . show $ getL beg span
    putStr ", "
    forM_ (S.toList $ getL gaps span) $ \(p, q, x) -> do
        putStr $ show p
        putStr ", "
        putStr $ show q
        putStr ", "
    putStr . show $ getL end span


-- | Print an active item.
printActive :: (Show n) => Active n -> IO ()
printActive p = do
    putStr "("
    putStr . show $ getL state p
    putStr ", "
    printSpan $ getL spanA p
    putStrLn ")"


-- | Print a passive item.
printPassive :: (Show n) => Passive n t -> Auto n t -> IO ()
printPassive p auto = do
    putStr "("
    -- putStr . viewLab $ getL label p
    putStr $ case getL dagID p of
        Left root ->
          show (notFootLabel root) ++
          if isSister root then "*" else ""
        Right did   ->
          show (DAG.unDID did) ++ "[" ++
          show (nonTerm (Right did) auto) ++ "]"
    putStr ", "
    printSpan $ getL spanP p
    putStrLn ")"
-- #endif
