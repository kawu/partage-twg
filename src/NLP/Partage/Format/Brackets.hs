{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module NLP.Partage.Format.Brackets
  (

    -- * Types
    Tree
  , NonTerm
  , Term(..)
  , LexTree
  , Super
  , SuperSent
  , SuperTok(..)

    -- * Anchoring
  , anchor
  , mapTerm

    -- * Parsing
  , parseTree
  , parseTree'
  , parseSuper

    -- * Rendering
  , showTree
  )
where


import           Control.Applicative ((<|>))
-- import           Data.List (intersperse)
import           Data.Monoid (mconcat, mappend)
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Tree as R

import qualified NLP.Partage.Tree.Other as O

-- import Debug.Trace (trace)


-------------------------------------------------------------
-- Core types
-------------------------------------------------------------


-- | Non-terminal
type NonTerm = T.Text


-- | Terminal or anchor
data Term
  = Term T.Text
  | Anchor
  deriving (Show, Eq, Ord)


-- | Non-lexicalized tree (with anchor)
type Tree = O.Tree NonTerm Term


-- | Lexicalized tree (with the anchor lexicalized)
type LexTree = O.Tree NonTerm T.Text


-- | Supertagging token is a pair (word, tags), where:
--
--   * word -- word on the given position in the sentence
--   * tags -- list of possible supertags interpretation of the word
--
data SuperTok = SuperTok
  { tokWord :: T.Text
  , tokTags :: [Tree]
  } deriving (Show, Eq)


-- | Sentence after supertagging
type SuperSent = [SuperTok]


-- | List of sentences after supertagging
type Super = [SuperSent]


-------------------------------------------------------------
-- Anchoring
-------------------------------------------------------------


-- | Replace the anchor with the given terminal.
anchor :: T.Text -> Tree -> LexTree
anchor lx =
  fmap onNode
  where
    onNode (O.Term Anchor) = O.Term lx
    onNode (O.Term (Term x)) = O.Term x
    onNode (O.NonTerm x) = O.NonTerm x
    onNode (O.Sister x) = O.Sister x
    onNode (O.Foot x) = O.Foot x


-- | A generalized `anchor`ing function, which applies the given function to all
-- terminals, both anchors and regular ones (see `Term`).
mapTerm :: (Term -> a) -> Tree -> O.Tree NonTerm a
mapTerm f =
  fmap onNode
  where
    onNode (O.Term t) = O.Term (f t)
    onNode (O.NonTerm x) = O.NonTerm x
    onNode (O.Sister x) = O.Sister x
    onNode (O.Foot x) = O.Foot x


-------------------------------------------------------------
-- Tree Parsing
-------------------------------------------------------------


-- | Parse a tree in the bracketed format.
parseTree :: T.Text -> Either String Tree
parseTree = Atto.parseOnly (treeP <* Atto.endOfInput)


-- | Version of `parseTree` which fails if the input is incorrectly formatted.
parseTree' :: T.Text -> Tree
parseTree' txt =
  case parseTree txt of
    Left err -> error $ unlines
      [ "Brackets.parseTree': failed to parsed with the following error:"
      , err ]
    Right x -> x


-- | Parse a tree in bracketed format. Only anchors are allowed as terminals.
treeP :: Atto.Parser Tree
treeP = nodeP <|> leafP


forestP :: Atto.Parser [Tree]
forestP = treeP `Atto.sepBy` Atto.skipMany1 Atto.space


-- | Non-leaf tree.
nodeP :: Atto.Parser Tree
nodeP = between '(' ')' $ do
  (nonTerm, starred) <- nonTermP
  Atto.skipWhile C.isSpace
  subTrees <- forestP
  let nodeLabel =
        if starred && null subTrees
        then O.Foot nonTerm
        else if starred
             then O.Sister nonTerm
             else O.NonTerm nonTerm
  return $ R.Node nodeLabel subTrees


leafP :: Atto.Parser Tree
leafP =
  leaf <$> (anchorP <|> terminalP)
  where
    leaf x = R.Node (O.Term x) []


terminalP :: Atto.Parser Term
terminalP =
  Term <$> Atto.takeWhile1 pr
  where
    pr c = not $ C.isSpace c || elem c [')', '(']


anchorP :: Atto.Parser Term
anchorP = Anchor <$ Atto.string "<>"


-- | Parses a non-terminal + information if it's marked with a star
-- (sister/foot).
nonTermP :: Atto.Parser (NonTerm, Bool)
nonTermP = do
  nonTerm <- Atto.takeTill $ \c -> C.isSpace c || c == '*'
  starred <- (True <$ Atto.char '*') <|> pure False
  return (nonTerm, starred)


-------------------------------------------------------------
-- Super Parsing
-------------------------------------------------------------


parseSuperTok :: T.Text -> SuperTok
parseSuperTok xs =
  case T.splitOn "\t" xs of
    [] -> error "Brackets.parseSuperTok: empty line"
    [_] -> error "Brackets.parseSuperTok: no supertags"
    word : tags -> SuperTok
      { tokWord = word
      , tokTags = map (parseTree') tags
      }


-- | Parse a sentence in a supertagged file.
parseSuperSent :: T.Text -> SuperSent
parseSuperSent = map parseSuperTok . T.lines


-- | Parse a supertagged file.
parseSuper :: L.Text -> Super
parseSuper
  = map (parseSuperSent . L.toStrict)
  . filter realSent
  . L.splitOn "\n\n"
  where
    realSent = not . L.null


-------------------------------------------------------------
-- Tree Rendering
-------------------------------------------------------------


-- | Render the given lexicalized tree in the bracketed format.
showTree :: LexTree -> L.Text
showTree = B.toLazyText . buildTree


buildTree :: LexTree -> B.Builder
buildTree tree
  | null (R.subForest tree) =
      buildLabel (R.rootLabel tree)
  | otherwise = mconcat
      [ "("
      , buildLabel (R.rootLabel tree)
      , buildForest (R.subForest tree)
      , ")"
      ]


buildForest :: [LexTree] -> B.Builder
buildForest =
  mconcat . map (mappend " " . buildTree)
  -- mconcat . intersperse " " . map buildTree


buildLabel :: O.Node NonTerm T.Text -> B.Builder
buildLabel = \case
  O.NonTerm x -> B.fromText x
  O.Term x -> B.fromText x
  O.Sister x -> B.fromText x `mappend` "*"
  O.Foot x -> B.fromText x `mappend` "*"


-------------------------------------------------------------
-- Utils
-------------------------------------------------------------


between :: Char -> Char -> Atto.Parser a -> Atto.Parser a
between p q par = do
  _ <- Atto.char p
  x <- par
  _ <- Atto.char q
  return x
