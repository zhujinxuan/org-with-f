import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import qualified Data.ByteString.Lazy as LBS
import YamlToJson (yamlToJson)
import System.FilePath (takeBaseName, replaceExtension)

main :: IO ()
main = defaultMain =<< goldenTests


goldenTests :: IO TestTree
goldenTests = undefined
