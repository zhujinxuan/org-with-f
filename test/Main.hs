module Main (main) where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Data.Text.IO (readFile)
import System.FilePath (replaceExtension)


main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  orgFiles <- findByExtension [".org"] "./test/golden-orgs/"
  return $ testGroup "Pass0 Golden tests"
    [ goldenVsString
        orgFile -- test name
        jsonFile -- golden file path
        (parseToJSON <$>  readFile orgFile) -- action whose result is tested
    | orgFile <- orgFiles
    , let jsonFile = replaceExtension yamlFile "pass0.json"
    ]
