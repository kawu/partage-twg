-- | Golden tests

module Golden where


import System.FilePath (takeBaseName, replaceExtension)

import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.Runners (TestTree)
import qualified Test.Tasty.Golden as G

import qualified NLP.Partage.AStar.Command as A
import           NLP.Partage.AStar.Command (AStarCommand(..))


-- | Full symbolic parsing tests; looking at parsed output
goldenSymTests :: IO TestTree
goldenSymTests = do
  inpFiles <- G.findByExtension [".inp"] "./tests/golden/symbolic"
  -- return . localOption G.Never $ testGroup "A* golden symbolic tests"
  return $ testGroup "A* golden symbolic tests"
    [ G.goldenVsFile
        (takeBaseName inpFile) -- test name
        goldenFile -- golden file path
        outFile -- A* parsing output file path
        (symParse inpFile outFile) -- action whose result is tested
    | inpFile <- inpFiles
    , let outFile = replaceExtension inpFile ".out"
          goldenFile = replaceExtension inpFile ".golden"
    ]


-- | Perform symbolic parsing with the A* parser
symParse
  :: FilePath -- ^ Input file
  -> FilePath -- ^ Output file
  -> IO ()
symParse inpFile outFile = do
  A.processCommand A.defAStarCommand
    { inputPath = Just inpFile
    , outputPath = Just outFile
    , fullHype = True
    , fullParse = True
    , showParses = 1000
    }


-- | A* parsing with weight tests
goldenWeightTests :: IO TestTree
goldenWeightTests = do
  inpFiles <- G.findByExtension [".inp"] "./tests/golden/weight"
  return $ testGroup "A* golden weight tests"
    [ G.goldenVsFile
        (takeBaseName inpFile) -- test name
        goldenFile -- golden file path
        outFile -- A* parsing output file path
        (weightParse inpFile outFile) -- action whose result is tested
    | inpFile <- inpFiles
    , let outFile = replaceExtension inpFile ".out"
          goldenFile = replaceExtension inpFile ".golden"
    ]


-- | Perform weight parsing with the A* parser
weightParse
  :: FilePath -- ^ Input file
  -> FilePath -- ^ Output file
  -> IO ()
weightParse inpFile outFile = do
  A.processCommand A.defAStarCommand
    { inputPath = Just inpFile
    , outputPath = Just outFile
    , verbosity = 1
    }
