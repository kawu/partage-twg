import           Test.Tasty (defaultMain, testGroup)

-- import           TestSet
import qualified Unit
import qualified Golden


main :: IO ()
main = do
  goldenSymTests <- Golden.goldenSymTests
  goldenVerboseTests <- Golden.goldenVerboseTests
  defaultMain $
    testGroup "Tests" $
      [ testGroup "Unit"
        [ Unit.unitTestEarley
        , Unit.unitTestAStar
        ]
      , testGroup "Golden"
        [ goldenSymTests
        , goldenVerboseTests ]
      ]
