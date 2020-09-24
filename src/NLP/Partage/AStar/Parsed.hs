{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Generating parsed trees.
module NLP.Partage.AStar.Parsed
  ( parsedTrees,
  )
where

import qualified Data.Set as S
import qualified NLP.Partage.AStar as A
import NLP.Partage.AStar.Base
import qualified NLP.Partage.AStar.Deriv as D
import NLP.Partage.SOrd (SOrd)
import qualified NLP.Partage.Tree as T
import qualified NLP.Partage.Tree.Other as O

-- | Extract the set of parsed trees obtained on the given input sentence.
-- Should be run on the result of the earley parser.
parsedTrees ::
  forall n t.
  (SOrd n, SOrd t) =>
  -- | Final state of the earley parser
  A.Hype n t ->
  -- | The start symbol set
  S.Set n ->
  -- | Length of the input sentence
  Int ->
  [T.Tree n (Maybe (Tok t))]
parsedTrees hype start n =
  map (O.mkTree . D.toParse)
    . concatMap (flip D.fromPassive hype)
    $ A.finalFrom start n hype
