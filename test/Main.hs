module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Fix (Fix (..))
import Data.Vector qualified as Vector
import Orgmode.Internal.Types (OrgDocumentF (..))
import Orgmode.Parser.Pass0 qualified as Pass0
import System.FilePath (replaceExtension)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  orgFiles <- findByExtension [".org"] "./test/golden-orgs/"
  return $
    testGroup
      "Pass0 Golden tests"
      [ goldenVsString
          orgFile -- test name
          jsonFile -- golden file path
          (parseToJSON <$> readFileText orgFile) -- action whose result is tested
        | orgFile <- orgFiles,
          let jsonFile = replaceExtension yamlFile "pass0.json"
      ]

parseToJSON :: Text -> ByteString
parseToJSON = undefined
  where
    parser = jsonToText . orgToJSon <$> Pass0.documentParse

orgToJSon :: Fix (OrgDocumentF Pass0.BlockPass0) -> Aeson.Value
orgToJSon = Aeson.Array . Vector.fromList . orgToJsonList

orgToJsonList :: Fix (OrgDocumentF Pass0.BlockPass0) -> [Aeson.Value]
orgToJsonList a = case unFix a of
  OrgDocEOFF _ -> []
  OrgDocHeadingF x y -> Aeson.toJSON x : orgToJsonList y
  OrgDocListItemF x y -> Aeson.toJSON x : orgToJsonList y
  OrgDocParagraphF x y -> Aeson.toJSON x : orgToJsonList y
  OrgDocEmptyLineF _ y -> orgToJsonList y

jsonToText :: Aeson.Value -> ByteString
jsonToText = encodePretty . Aeson.Encoding.value
