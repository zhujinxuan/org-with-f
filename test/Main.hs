import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Data.Text.Lazy.IO (readFile)
import System.FilePath (replaceExtension)


main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  orgFiles <- findByExtension [".org"] "./test/golden-/"
  return $ testGroup "Pass0 Golden tests"
    [ goldenVsString
        orgFile -- test name
        jsonFile -- golden file path
        (yamlToJson <$> LBS.readFile yamlFile) -- action whose result is tested
    | orgFile <- orgFiles
    , let jsonFile = replaceExtension yamlFile ".json"
    ]
