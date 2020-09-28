import           Test.Tasty (defaultMain, testGroup)

-- import           TestSet
import qualified Unit
import qualified Golden


main :: IO ()
main = do
  goldenSymTests <- Golden.goldenSymTests
  defaultMain $
    testGroup "Tests" $
      [ testGroup "Unit"
        [ Unit.unitTestEarley
        , Unit.unitTestAStar
        ]
      , testGroup "Golden"
        [ goldenSymTests ]
      ]
