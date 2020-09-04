{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}


-- | Alternative (to "NLP.Partage.Tree") representation of TAG
-- trees, in which information about the foot is present in the tree
-- itself.


module NLP.Partage.Tree.Other
(
-- * TAG Tree
  Tree
, Node (..)
-- , SomeTree
-- , follow

-- * Conversion
, mkTree
, unTree
-- , encode
-- , decode

-- * Utils
, isTerm
, mapTerm
, isFinal
-- , isInitial
-- , isAuxiliary
, hasRoot
, project
-- , replaceFoot
, replaceSlot
, replaceSlot'
) where


-- import           Control.Applicative ((<$>))
import           Control.Monad (msum) -- , foldM)
import qualified Control.Monad.State.Strict as State
import qualified Data.Foldable as F

import qualified Data.Tree as R

import qualified NLP.Partage.Tree as T


---------------------------------------------------------------------
-- Types
---------------------------------------------------------------------


-- | Node of a TAG tree.
data Node n t
    = NonTerm n     -- ^ Standard non-terminal
--     | Foot n        -- ^ Foot node
    | DNode n       -- ^ D-daughter node
    | Sister n      -- ^ Sister non-terminal root
    | Term t        -- ^ Terminal
    deriving (Show, Eq, Ord)


-- | Is it a teminal?
isTerm :: Node n t -> Bool
isTerm (Term _) = True
isTerm _        = False


-- | Apply the function to terminals.
mapTerm :: (t -> t') -> Node n t -> Node n t'
mapTerm f =
  onNode
  where
    onNode (Term t) = Term (f t)
    onNode (NonTerm x) = NonTerm x
    onNode (Sister x) = Sister x
--     onNode (Foot x) = Foot x
    onNode (DNode x) = DNode x


-- | An initial or auxiliary TAG tree.  Note that the type doesn't
-- ensure that the foot is placed in a leaf, nor that there is at
-- most one foot node.  On the other hand, and in contrast to
-- "NLP.Partage.Tree", information about the foot is available at
-- the level of the corresponding foot node.
type Tree n t = R.Tree (Node n t)


-- -- | An original tree representation (see "NLP.Partage.Tree").
-- type SomeTree n t = Either (T.Tree n t) (T.AuxTree n t)


---------------------------------------------------------------------
-- Encoding
---------------------------------------------------------------------


-- -- | Encode the tree using the alternative representation.
-- encode :: SomeTree n t -> Tree n t
-- encode (Left t) = unTree t
-- encode (Right T.AuxTree{..}) = markFoot auxFoot (unTree auxTree)


-- | Encode the initial tree using the alternative representation.
unTree :: T.Tree n t -> Tree n t
unTree (T.Branch x xs) = R.Node (NonTerm x) (map unTree xs)
unTree (T.Leaf x)    = R.Node (Term x) []


-- -- | Mark non-terminal under the path as a foot.
-- markFoot :: T.Path -> Tree n t -> Tree n t
-- markFoot [] (R.Node (NonTerm x) []) = R.Node (Foot x) []
-- markFoot (i:is) (R.Node y ys) =
--     R.Node y $ doit i ys
--   where
--     doit 0 (x:xs) = markFoot is x : xs
--     doit k (x:xs) = x : doit (k-1) xs
--     doit _      _      = error "markFoot.doit: unhandled case"
-- markFoot _ _ = error "markFoot: unhandled case"


---------------------------------------------------------------------
-- Decoding
---------------------------------------------------------------------


-- -- | Decode the tree represented with the alternative representation.
-- decode :: Tree n t -> SomeTree n t
-- decode t = case findFoot t of
--     Just is -> Right $ T.AuxTree (mkTree t) is
--     Nothing -> Left $ mkTree t


-- | Convert the parsed tree into an LTAG tree.  Beware that this function
-- does not preserve DNode/Sister markings!
mkTree :: Tree n t -> T.Tree n t
mkTree (R.Node n xs) = case n of
    Term x  -> T.Leaf x
--     Foot x  -> T.Branch
--         { T.labelI = x
--         , T.subTrees = [] }
    NonTerm x   -> T.Branch
        { T.labelI = x
        , T.subTrees = map mkTree xs }
    Sister x   -> T.Branch
        { T.labelI = x
        , T.subTrees = map mkTree xs }
    DNode x  -> T.Branch
        { T.labelI = x
        , T.subTrees = map mkTree xs }


-- -- | Find the path of the foot (if present) in the tree.
-- findFoot :: Tree n t -> Maybe T.Path
-- findFoot (R.Node n xs) = case n of
--     Foot _  -> Just []
--     _       -> msum
--         $ zipWith addID [0..]
--         $ map findFoot xs
--   where
--     addID i (Just is) = Just (i:is)
--     addID _ Nothing   = Nothing


---------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------


-- -- | Is it an initial (i.e. non-auxiliary) tree?
-- isInitial :: Tree n t -> Bool
-- isInitial = not . isAuxiliary


-- -- | Is it an auxiliary (i.e. with a foot) tree?
-- isAuxiliary :: Tree n t -> Bool
-- isAuxiliary (R.Node (Foot _) _) = True
-- isAuxiliary (R.Node _ xs) = any isAuxiliary xs


-- | Is it a final tree (i.e. does it contain only terminals
-- in its leaves)?
isFinal :: Tree n t -> Bool
isFinal (R.Node n []) = isTerm n
isFinal (R.Node _ xs) = all isFinal xs


-- | Projection of a tree, i.e. a list of its terminals.
project :: Tree n t -> [t]
project =
    F.foldMap term
  where
    term (Term x) = [x]
    term _        = []


-- | Is it a root label of the given tree?
hasRoot :: Eq n => n -> Tree n t -> Bool
hasRoot x (R.Node (NonTerm y) _) = x == y
hasRoot _ _ = False


-- -- | Replace the foot in the second tree with the first tree (if any).
-- replaceFoot :: Tree n t -> Tree n t -> Tree n t
-- replaceFoot t (R.Node n xs) =
--   case n of
--     Foot _ -> t
--     _      -> R.Node n (map (replaceFoot t) xs)


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


-- -- | Version of `replaceSlot` working on a different representation of trees.
-- NOTE 02.09.2020: This version is lossy, markers (DNode, Sister) are not
-- preserved.
-- replaceSlot' :: Tree n t -> Tree n t -> Tree n t
-- replaceSlot' plug tree =
--   unTree $ replaceSlot (mkTree plug) (mkTree tree)


-- | Version of `replaceSlot` working on a different representation of trees.
replaceSlot'
  :: Int  -- ^ Slot number
  -> Tree n t
  -> Tree n t
  -> Tree n t
replaceSlot' slotNum plug tree =
  State.evalState (go tree) slotNum
  where
    go t@(R.Node (NonTerm x) []) = do
      k <- State.get
      State.put $ k - 1
      return $ if k == 0
         then plug
         else t
    go (R.Node nd ts) = R.Node nd <$> mapM go ts
