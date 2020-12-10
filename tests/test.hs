import           Test.Tasty (defaultMain, testGroup)

-- import           TestSet
import qualified Unit
import qualified Golden


main :: IO ()
main = do
  goldenSymTests <- Golden.goldenSymTests
  goldenWeightTests <- Golden.goldenWeightTests
  defaultMain $
    testGroup "Tests" $
      [ testGroup "Unit"
        [ Unit.unitTestEarley
        , Unit.unitTestAStar
        ]
      , testGroup "Golden"
        [ goldenSymTests
        , goldenWeightTests ]
      ]
