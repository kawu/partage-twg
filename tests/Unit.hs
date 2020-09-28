{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


-- | Testing the automata-based Earley-style TAG parser.


module Unit where


import           Control.Applicative     ((<$>))
import qualified Control.Arrow           as Arr
import           Control.Monad           (forM_, void)
import           Test.Tasty              (TestTree)

import qualified Data.MemoCombinators    as Memo
import qualified Data.Set                as S
import qualified Data.Map                as M
import qualified Data.Text               as Txt
import           Data.Maybe              (maybeToList)

-- import qualified Pipes                   as P

import qualified NLP.Partage.AStar       as A
import qualified NLP.Partage.AStar.Parsed as A
import qualified NLP.Partage.AStar.Deriv as D
import qualified NLP.Partage.DAG         as DAG
import qualified NLP.Partage.Earley      as E
import qualified NLP.Partage.Tree.Other  as O

import qualified UnitTestSet                 as T


-- | All the tests of the parsing algorithm.
unitTestEarley :: TestTree
unitTestEarley =
  T.testTree "Earley" parser
  where
    parser = T.dummyParser
      { T.recognize = Just recFrom
--       , T.parsedTrees = Just parseFrom
      , T.dependencySupport = False }
    recFrom gram start input _headMap = do
        let dag = mkGram gram
        E.recognizeFrom dag (S.singleton start) (E.fromList input)
--     parseFrom gram start input _headMap = do
--         let dag = mkGram gram
--         fmap S.fromList
--             . E.parse dag (S.singleton start)
--             . E.fromList
--             $ input
    mkGram = DAG.mkGram . map (Arr.first termToSet)
    termToSet = fmap (O.mapTerm $ fmap S.singleton)


-- | All the tests of the parsing algorithm.
unitTestAStar :: TestTree
unitTestAStar =
  T.testTree "A*" parser
  where
    parser = T.dummyParser
      { T.recognize = Just recFrom
      , T.parsedTrees = Just parseFrom
      , T.derivTrees = Just derivFrom
      -- , T.derivPipe  = Just derivPipe
      , T.dependencySupport = True 
      }

    recFrom gram start input headMap
      = A.recognizeFrom memoTerm gram (S.singleton start) (posMap input) headMap
      . A.fromList
      $ input

    parseFrom gram start sent headMap = do
      let dag = DAG.mkGram gram
          input = A.fromList sent
          auto = A.mkAuto memoTerm dag input (posMap sent) headMap
      hype <- A.earleyAuto auto input
      return
        . S.fromList
        -- below we just map (Tok t -> t) but we have to also
        -- do the corresponding encoding/decoding
        . map (O.mkTree . fmap (O.mapTerm $ fmap A.terminal) . O.unTree)
        $ A.parsedTrees hype (S.singleton start) (length sent)

    derivFrom gram start sent headMap = do
      let dag = DAG.mkGram gram
          input = A.fromList sent
          auto = A.mkAuto memoTerm dag input (posMap sent) headMap
      hype <- A.earleyAuto auto input
      return $ D.derivTrees hype (S.singleton start) (length sent)

--     derivPipe gram start sent headMap =
--       let dag = DAG.mkGram gram
--           input = A.fromList sent
--           auto = A.mkAuto memoTerm dag input (posMap sent) headMap
--       in  D.consumeDerivs auto input (S.singleton start)

    memoTerm = Memo.wrap
      (uncurry T.Term)
      ((,) <$> T.orth <*> T.pos)
      (Memo.pair memoString (Memo.maybe Memo.integral))
    -- memoString = Memo.list Memo.char
    memoString = Memo.wrap
      Txt.pack Txt.unpack
      (Memo.list Memo.char)

    posMap input = M.fromList $ do
      tok <- input
      pos <- maybeToList (T.pos tok)
      return (tok, pos)


-- -- | All the tests of the parsing algorithm.
-- testAStar :: TestTree
-- testAStar =
--   T.testTree "A*" parser
--   where
--     parser = T.dummyParser
--       { T.recognize = Just recFrom
--       , T.parsedTrees = Just parseFrom
--       , T.derivTrees = Just derivFrom
--       , T.encodes = Just encodesFrom
--       , T.derivPipe  = Just derivPipe
--       , T.dependencySupport = True 
--       }
--     recFrom gram start input headMap
--       = A.recognizeFrom memoTerm gram (S.singleton start) (posMap input) headMap
--       . A.fromList
--       $ input
--     parseFrom gram start sent headMap = do
--       let dag = DAG.mkGram gram
--           input = A.fromList sent
--           auto = A.mkAuto memoTerm dag input (posMap sent) headMap
--       hype <- A.earleyAuto auto input
--       return
--         . S.fromList
--         -- below we just map (Tok t -> t) but we have to also
--         -- do the corresponding encoding/decoding
--         . map (O.mkTree . fmap (O.mapTerm $ fmap A.terminal) . O.unTree)
--         $ A.parsedTrees hype (S.singleton start) (length sent)
--     derivFrom gram start sent headMap = do
--       let dag = DAG.mkGram gram
--           input = A.fromList sent
--           auto = A.mkAuto memoTerm dag input (posMap sent) headMap
--       hype <- A.earleyAuto auto input
--       return $ D.derivTrees hype (S.singleton start) (length sent)
--     encodesFrom hype start input = D.encodes hype (S.singleton start) (length input)
--     derivPipe gram start sent headMap =
--       let dag = DAG.mkGram gram
--           input = A.fromList sent
--           auto = A.mkAuto memoTerm dag input (posMap sent) headMap
--       in  D.consumeDerivs auto input (S.singleton start)
-- --           conf = D.DerivR
-- --             { D.startSym = start
-- --             , D.sentLen = length sent }
-- --       in  A.earleyAutoP auto input P.>-> D.derivsPipe conf
--     memoTerm = Memo.wrap
--       (uncurry T.Term)
--       ((,) <$> T.orth <*> T.pos)
--       (Memo.pair memoString (Memo.maybe Memo.integral))
--     memoString = Memo.list Memo.char
--     posMap input = M.fromList $ do
--       tok <- input
--       pos <- maybeToList (T.pos tok)
--       return (tok, pos)
