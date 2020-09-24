import           Test.Tasty (defaultMain, testGroup)

-- import           TestSet
import qualified Parser


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Parser.testEarley
    , Parser.testAStar
    ]
