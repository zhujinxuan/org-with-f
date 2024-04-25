module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Orgmode.Internal.Types (OrgDocumentF(..))
import Orgmode.Parser.Pass0 qualified as Pass0
import System.FilePath (replaceExtension)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Data.Fix (Fix(..))

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
      | orgFile <- orgFiles
      , let jsonFile = replaceExtension yamlFile "pass0.json"
      ]

parseToJSON :: Text -> ByteString
parseToJSON = undefined where
  parser = orgToJSON . jsonToText . encodeUtf8 <$> Pass0.documentParse

orgToJSon :: Fix (OrgDocumentF Pass0.BlockPass0)-> Aeson.Array
orgToJSon x = undefined

orgToJsonList ::Fix (OrgDocumentF Pass0.BlockPass0) -> [Aeson.Value]
orgToJsonList a = case unFix a of
  OrgDocEOFF _ -> []
  OrgDocHeadingF x y -> Aeson.toJSON x: orgToJsonList y
  OrgDocListItemF x y -> Aeson.toJSON x: orgToJsonList y
  OrgDocParagraphF x y -> Aeson.toJSON x: orgToJsonList y
  OrgDocEmptyLineF _ y ->  orgToJsonList y

jsonToText :: Aeson.Value -> ByteString
jsonToText = toStrict . Aeson.Encoding.encodingToLazyByteString . Aeson.Encoding.value
