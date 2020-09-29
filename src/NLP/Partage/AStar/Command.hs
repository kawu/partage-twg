{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


-- | IO for the A* parsing mode


module NLP.Partage.AStar.Command
  ( AStarCommand (..)
  , defAStarCommand
  , processCommand
  )
where

import System.IO (stdout, withFile, Handle, IOMode(WriteMode), hPutStrLn, hPutStr)

import Control.Monad (forM_, when, guard)
import qualified Control.Arrow as Arr
import qualified Data.Foldable as F
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Time as Time
import qualified Data.Tree as R
import qualified Data.Vector as V
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Monoid (Sum(..))

import qualified Pipes as P

import qualified NLP.Partage.Tree.Other as O
import qualified NLP.Partage.AStar as A
import qualified NLP.Partage.AStar.Chart as C
import qualified NLP.Partage.AStar.Deriv as D
import qualified NLP.Partage.DAG as DAG
import qualified NLP.Partage.Format.Brackets as Br
import qualified NLP.Partage.AStar.Deriv.Gorn as DG


data AStarCommand = AStarCommand
  { -- | Input file with supertagging info; use stdin if not provided
    inputPath :: Maybe FilePath,
    outputPath :: Maybe FilePath,
    -- | Verbosity level
    verbosity :: Int,
    maxTags :: Maybe Int,
    maxDeps :: Maybe Int,
    minProb :: Maybe Double,
    betaParam :: Maybe Double,
    startSym :: S.Set T.Text,
    fullHype :: Bool,
    maxLen :: Maybe Int,
    fullParse :: Bool,
    -- brackets :: Bool
    showParses :: Int,
    showParseNum :: Maybe Int,
    allDerivs :: Bool,
    useSoftMax :: Bool
    -- checkRepetitions :: Bool
  }


defAStarCommand :: AStarCommand
defAStarCommand = AStarCommand
  { inputPath = Nothing
  , outputPath = Nothing
  , verbosity = 0
  , maxTags = Nothing
  , maxDeps = Nothing
  , minProb = Nothing
  , betaParam = Nothing
  , startSym = S.empty
  , fullHype = False
  , maxLen = Nothing
  , fullParse = False
  , showParses = 0
  , showParseNum = Nothing
  , allDerivs = False
  , useSoftMax = False
  }


-- | Run an action using the stdout handle
withStdout :: (Handle -> IO ()) -> IO ()
withStdout action = action stdout


-- | Process the IO A* command
processCommand :: AStarCommand -> IO ()
processCommand AStarCommand {..} = do
  -- Read input supertagging file
  let parseSuper = Br.parseSuperProb
      filterLen =
        case maxLen of
          Nothing -> id
          Just ml -> filter ((<= ml) . length)

  -- Determine output handle
  let withOut = case outputPath of
        Nothing -> withStdout
        Just outPath -> withFile outPath WriteMode

  withOut $ \hOut -> do

    super <-
      filterLen
        . limitTagsProb minProb
        . limitDepsProb minProb
        . limitTagsBeta betaParam
        . limitDepsBeta betaParam
        . limitDepsNum maxDeps
        . limitTagsNum maxTags
        . parseSuper
        <$> readInput inputPath

    forM_ super $ \sent -> do
      let -- Add token IDs in order to distinguish tokens with identical word
          -- forms (which may have different supertags)
          input = zip [0 :: Int ..] (map Br.tokWord sent)
          inputVect = V.fromList (map Br.tokWord sent)

          -- Calculate the position map (mapping from tokens to their
          -- positions)
          posMap = M.fromList [(x, fst x) | x <- input]

          -- Create the corresponding dependency map
          depMap = mkDepMap' useSoftMax $ zip [0 :: Int ..] sent

          -- Create the compressed grammar representation
          gram =
            DAG.mkGram
              . anchorTags
              . zip [0 :: Int ..]
              $ sent
          automat =
            A.mkAuto
              memoTerm
              gram
              (A.fromList input)
              posMap
              depMap
          memoTerm =
            Memo.wrap
              (\i -> (i, inputVect V.! i))
              (\(i, _w) -> i)
              Memo.integral

      -- Check against the gold file or perform simple recognition
      when (verbosity > 0) $ do
        hPutStr hOut "# SENT: "
        TIO.hPutStrLn hOut . T.unwords $ map snd input
        hPutStr hOut "# LENGTH: "
        hPutStrLn hOut . show $ length input

      begTime <- Time.getCurrentTime
      hypeRef <- IORef.newIORef Nothing
      let n = length input
          consume = do
            A.HypeModif {..} <- P.await
            case (modifType, modifItem) of
              (A.NewNode, A.ItemP p) ->
                -- if (D.isFinal_ modifHype startSym n p) then do
                if (C.isFinal startSym n (A.automat modifHype) p)
                  then do
                    P.liftIO $ do
                      semiTime <- Time.getCurrentTime
                      IORef.modifyIORef hypeRef $ \case
                        Nothing -> Just (modifHype, semiTime)
                        Just x -> Just x
                    --                 semiTime <- P.liftIO Time.getCurrentTime
                    --                 P.liftIO . IORef.writeIORef hypeRef $ Just (modifHype, semiTime)
                    if fullHype
                      then consume
                      else return modifHype
                  else consume
              _ -> consume
      finalHype <- A.earleyAutoP automat (A.fromList input) consume
      endTime <- Time.getCurrentTime
      (semiHype, semiTime) <-
        maybe (finalHype, endTime) id
          <$> IORef.readIORef hypeRef
      when (verbosity > 0) $ do
        --       let reco = (not.null) (A.finalFrom startSym n semiHype)
        --       hPutStr hOut "# RECO: "
        --       hPutStrLn hOut . show reco
        hPutStr hOut "# ARCS: "
        hPutStr hOut (show n)
        hPutStr hOut "\t"
        hPutStr hOut (show $ A.hyperEdgesNum semiHype)
        hPutStr hOut "\t"
        hPutStr hOut $ show (semiTime `Time.diffUTCTime` begTime)
        if fullHype
          then do
            hPutStr hOut "\t"
            hPutStr hOut (show $ A.hyperEdgesNum finalHype)
            hPutStr hOut "\t"
            hPutStrLn hOut . show $ endTime `Time.diffUTCTime` begTime
          else do
            hPutStrLn hOut ""

      -- Calculating derivations
      let getDerivs () =
            if allDerivs
              then D.derivTreesAllW finalHype startSym (length input)
              else D.derivTreesW finalHype startSym (length input)

      -- Calculate the derivations
      let derivs = getDerivs ()
      when (null derivs) $ do
        hPutStr hOut "# NO PARSE FOR: "
        TIO.hPutStrLn hOut . T.unwords $ map snd input

      case showParseNum of
        Nothing -> return ()
        Just _k -> do
          error "not implemented"
      --         hPutStr hOut "# PARSE NUM: "
      --         hPutStrLn hOut . show $ sum
      --           -- Evaluate the trees to avoid the memory leak
      --           [ L.length txtTree `seq` (1 :: Int)
      --           | (deriv, _weight) <- take k (getDerivs ())
      --           , let txtTree = showParse deriv
      --           ]

      -- Print a single best derivation
      forM_ (take showParses derivs) $ \(deriv, w) -> do
        if fullParse
          then renderParse hOut deriv >> hPutStrLn hOut ""
          else renderDeriv hOut deriv
        when (verbosity > 0) $ do
          hPutStrLn hOut $ "# WEIGHT: " ++ show w
          when (verbosity > 1) $ do
            hPutStrLn hOut
              . R.drawTree
              . fmap show
              . D.deriv4show
              . D.normalize
              $ deriv
      hPutStrLn hOut ""


  where

    -- Additional supertag-related constraints
    limitTagsNum = \case
      Nothing -> id
      Just m -> map . map $ \superTok -> superTok
        {Br.tokTags = takeBest m (Br.tokTags superTok)}
    limitTagsProb = \case
      Nothing -> id
      Just p -> map . map $ \superTok -> superTok
        {Br.tokTags = filter ((>=p) . snd) (Br.tokTags superTok)}
    limitTagsBeta = \case
      Nothing -> id
      Just beta -> map . map $ \superTok ->
        let maxProb = case map snd (Br.tokTags superTok) of
              [] -> 0.0
              xs -> maximum xs
            p = maxProb * beta
        in  superTok
            {Br.tokTags = filter ((>=p) . snd) (Br.tokTags superTok)}

    -- Additional dependency-related constraints
    limitDepsNum = \case
      Nothing -> id
      Just m -> map . map $ \superTok -> superTok
        {Br.tokDeph =
          (M.fromList . takeBest m . M.toList)
          (Br.tokDeph superTok)
        }
    limitDepsProb = \case
      Nothing -> id
      Just p -> map . map $ limitDepsProbFor p
    limitDepsBeta = \case
      Nothing -> id
      Just beta -> map . map $ \superTok ->
        let maxProb = case M.elems (Br.tokDeph superTok) of
              [] -> 0.0
              xs -> maximum xs
            p = maxProb * beta
        in  limitDepsProbFor p superTok
    limitDepsProbFor p superTok = superTok
      { Br.tokDeph
          = M.fromList
          . filter ((>=p) . snd)
          . M.toList
          $ Br.tokDeph superTok
      }

    takeBest k xs
      = take k . reverse
      $ List.sortBy (comparing snd) xs

    readInput mayPath =
      case mayPath of
        Nothing -> LIO.getContents
        Just path -> LIO.readFile path


--------------------------------------------------
-- Anchoring
--------------------------------------------------


-- -- | Local tree type
-- type Tree =
--   O.Tree
--     T.Text
--     (Maybe (S.Set (Int, T.Text)))


-- -- | Tag anchoring function which:
-- --
-- --   (a) Joins identical trees with different terminals
-- --   (b) Replaces the weights by 0s (stems from (a))
-- --
-- anchorTagsIgnoreProbs
--   :: [(Int, Br.SuperTok)]
--   -> [(Tree, DAG.Weight)]
-- anchorTagsIgnoreProbs xs = do
--   (tag, termSet) <- M.toList tagMap
--   return (anchorTag (Just termSet) onTerm tag, 0)
--   where
--     tagMap = M.fromListWith S.union $ do
--       (tokID, Br.SuperTok{..}) <- xs
--       (tag, _weight) <- tokTags
--       return (tag, S.singleton (tokID, tokWord))
--     onTerm = \case
--       Nothing -> Nothing
--       Just _ -> error "Cannot process a co-anchor terminal node"


-- | A version of `anchorTagsIgnoreProbs` which preserves probabilities, but
-- does not join identical supertags.  Besides, it replaces each probability
-- `p` by `-log(p)`.
--
-- WARNING: At the end, the minimum weight of a tree becoming a dependendent is
-- added to the corresponding weight.  This allows the new definition of the
-- amortized weight.
anchorTags
  :: [(Int, Br.SuperTok)]
  -> [(O.Tree T.Text (Maybe (Int, T.Text)), DAG.Weight)]
anchorTags =
  concatMap (uncurry anchor)
  where
    anchor tokID Br.SuperTok{..} = map
      ( Arr.first (anchorTag (Just (tokID, tokWord)) onTerm)
      -- . Arr.second (\p -> - (log p + log maxDepProb))
      . Arr.second (\p -> - (log p))
      )
      tokTags
--         where
--           maxDepProb =
--             case M.elems tokDeph of
--               [] -> error
--                 "partage.anchorTags: dependency weights not specified"
--               xs -> maximum xs
    onTerm = \case
      Nothing -> Nothing
      Just _ -> error "Cannot process a co-anchor terminal node"


-- | Anchor the given elementary tree with the given anchor terminal symbol.
anchorTag
  :: t
    -- ^ To substitute the anchor
  -> (Maybe T.Text -> t)
    -- ^ To map over standard terminals
  -> Br.Tree
  -> O.Tree T.Text t
anchorTag x f tree =
  case getSum (anchorNum tree) of
    1 -> doAnchor tree
    n -> error $ "anchorTag: number of anchors = " ++ show n
  where
    doAnchor = fmap . O.mapTerm $ \case
      Br.Anchor -> x
      Br.Term t -> f t
    anchorNum = F.foldMap $ \case
      O.Term Br.Anchor -> Sum (1 :: Int)
      _ -> Sum 0


--------------------------------------------------
-- Dependency Map
--------------------------------------------------

-- | Create the dependency map corresponding to the given list of tokens.  Note
-- that the `Br.deph` values have to be handled carefully -- in the
-- corresponding format, tokens are numbered from 1 and not from 0.
--
-- TODO: we may want to take the dummy ROOT word into account!
--
-- mkDepMap :: [(Int, Br.SuperTok)] -> M.Map Int Int
-- mkDepMap toks = M.fromList
--   [ (dep, hed - 1)
--   | (dep, Br.SuperTok{..}) <- toks
--   , (hed, _weight) <- tokDeph ]


-- | A variant of `mkDepMap` which creates a map of possible head positions
-- together with the corresponding heads.  A stub so far, really.
mkDepMap'
  :: Bool -- ^ Apply softmax to get probabilities from weights first
  -> [(Int, Br.SuperTok)]
  -> M.Map Int (M.Map Int DAG.Weight)
mkDepMap' applySM toks = M.fromList $ catMaybes
  [ (dep,) <$> do
      guard . not $ M.null tokDeph
      return $ mapMap
        (\k -> k-1)
        (\p -> -log(p))
        (trySoftMax tokDeph)
    | (dep, Br.SuperTok{..}) <- toks
  ] where
    trySoftMax = if applySM then softMax else id


-- | Map a function over keys and values of the given map.
mapMap
  :: (Ord k')
  => (k -> k')
  -> (v -> v')
  -> M.Map k v
  -> M.Map k' v'
mapMap f g m = M.fromList
  [ (f key, g val)
    | (key, val) <- M.toList m
  ]


-- | Apply softmax to the given map.
softMax
  :: (Ord k)
  => M.Map k Double
  -> M.Map k Double
softMax m =
  fmap (\x -> exp x / normFact) m
  where
    normFact = sum . map exp $ M.elems m


--------------------------------------------------
-- Rendering input (dummy mode)
--------------------------------------------------


-- -- | Render the given derivation.
-- renderInput :: Handle -> [(Int, Br.SuperTok)] -> IO ()
-- renderInput hOut inp = do
--   forM_ inp $ \(tokID, Br.SuperTok{..}) -> do
--     let takeBest df xs =
--           case List.sortBy (comparing snd) xs of
--             [] -> df
--             ys -> fst . head $ reverse ys
--         depHed = takeBest (-1) (M.toList tokDeph)
--         supTag = Br.anchor tokWord $
--           takeBest (error "renderInput: no supertags") tokTags
--     LIO.hPutStr hOut . L.pack . show $ tokID
--     LIO.hPutStr hOut "\t"
--     LIO.hPutStr hOut $ L.fromStrict tokWord
--     LIO.hPutStr hOut "\t"
--     LIO.hPutStr hOut . L.pack . show $ depHed
--     LIO.hPutStr hOut "\t"
--     LIO.hPutStrLn hOut $ Br.showTree supTag


--------------------------------------------------
-- Rendering derivations/parses
--------------------------------------------------


-- | Render the given derivation.
renderDeriv
  :: Handle
  -> D.Deriv D.UnNorm T.Text (A.Tok (Int, T.Text))
  -> IO ()
renderDeriv hOut deriv0 = do
  let deriv = DG.fromDeriv deriv0
      tagMap = tagsFromDeriv deriv
      depMap = depsFromDeriv deriv
      getPos = L.pack . show . (+1) . A.position
      getTerm = L.fromStrict . snd . A.terminal
  forM_ (M.toList tagMap) $ \(tok, et) -> do
    LIO.hPutStr hOut . L.intercalate "," $
      map getPos (S.toList tok)
    LIO.hPutStr hOut "\t"
    LIO.hPutStr hOut . L.intercalate "," $
      map getTerm (S.toList tok)
    LIO.hPutStr hOut "\t"
    LIO.hPutStr hOut . L.intercalate "," $
        maybe ["0"] (map getPos . S.toList) $
          M.lookup tok depMap
    LIO.hPutStr hOut "\t"
    LIO.hPutStrLn hOut . Br.showTree $ fmap rmTokID et


-- | Render the given derivation.
renderParse :: Handle -> D.Deriv D.UnNorm T.Text (A.Tok (Int, T.Text)) -> IO ()
renderParse hOut = LIO.hPutStr hOut . showParse


-- | Render the given derivation.
showParse
  :: D.Deriv D.UnNorm T.Text (A.Tok (Int, T.Text))
  -> L.Text
showParse deriv
  = showIt
  . check
  $ parse
  where
    showIt = Br.showTree . fmap rmTokID'
    -- parse = fst $ D.toParse deriv
    parse = D.toParse deriv
    check t =
      let posList = map A.position (catMaybes $ O.project t) in
      if posList == List.sort posList
         then t
         else error "partage.showParse: words not in order!"


--------------------------------------------------
-- ETs from derivation
--------------------------------------------------


-- | Complex token.
type Tok t = S.Set (A.Tok t)


-- | Retrieve the list of selected ETs for the individual tokens.
tagsFromDeriv
  :: DG.Deriv n (A.Tok t)
  -> M.Map (Tok t) (O.Tree n (Maybe t))
tagsFromDeriv =
  go
    where
      go DG.Deriv{..} =
        let tok = getTok rootET
            chMap = M.unions . map go . concat $ M.elems modifs
        in  M.insert tok (fmap (O.mapTerm $ fmap A.terminal) rootET) chMap


-- | Retrieve the map of selected dependency heads.
depsFromDeriv
  :: DG.Deriv n (A.Tok t)
  -- -> M.Map (Tok t) (S.Set (Tok t))
  -> M.Map (Tok t) (Tok t)
depsFromDeriv =
  snd . go
    where
      go DG.Deriv{..} =
        let tok = getTok rootET
            children = map go . concat $ M.elems modifs
            chToks = map fst children
            chMap = M.unions $ map snd children
            newMap = List.foldl' (\m dep -> M.insert dep tok m) chMap chToks
        in  (tok, newMap)


-- -- | Determine the position set in the given tree.
-- getPos :: O.Tree n (A.Tok t) -> S.Set A.Pos
-- getPos = S.fromList . map A.position . O.project


-- | Determine the token set in the given tree.
getTok :: O.Tree n (Maybe (A.Tok t)) -> S.Set (A.Tok t)
getTok = S.fromList . catMaybes . O.project


--------------------------------------------------
-- Utils
--------------------------------------------------


-- | Remove the k-th element in the list.
remove :: Int -> [a] -> [a]
remove k xs = take k xs ++ drop (k+1) xs


-- | Remove info about token IDs.
-- rmTokID :: O.Node n (Maybe (Int, t)) -> O.Node n (Maybe t)
rmTokID :: O.Node n (Maybe (Int, t)) -> O.Node n (Maybe t)
rmTokID = \case
  O.Term (Just (_, x)) -> O.Term (Just x)
  O.Term Nothing -> O.Term Nothing
  O.NonTerm x -> O.NonTerm x
  O.Sister x -> O.Sister x
  O.DNode x -> O.DNode x
--   O.Foot x -> O.Foot x


-- | Remove info about token IDs.
rmTokID' :: O.Node n (Maybe (A.Tok (Int, t))) -> O.Node n (Maybe t)
rmTokID' = \case
  -- O.Term tok -> O.Term . snd $ A.terminal tok
  O.Term (Just tok) -> O.Term . Just . snd $ A.terminal tok
  O.Term Nothing -> O.Term Nothing
  O.NonTerm x -> O.NonTerm x
  O.Sister x -> O.Sister x
  O.DNode x -> O.DNode x
--   O.Foot x -> O.Foot x


-- | Remove the repeating elements from the input list.
nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
