module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as Lazy
import Data.Fix (Fix (..))
import Orgmode.Internal.Types (BlockName (blockName), OrgDocumentF (..))
import Orgmode.Parser.Internal (BParser, defaultOrgConfig)
import Orgmode.Parser.Pass0 (BlockPass0)
import Orgmode.Parser.Pass0 qualified as Pass0
import System.FilePath (replaceExtension)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Megaparsec qualified as MP

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
          (replaceExtension orgFile "pass0.json") -- golden file path
          (parseAndOut orgFile . decodeUtf8 <$> readFileBS orgFile) -- action whose result is tested
        | orgFile <- orgFiles
      ]

parseAndOut :: FilePath -> Text -> Lazy.ByteString
parseAndOut orgFile c = case runReader (MP.runParserT parser orgFile c) defaultOrgConfig of
  Right x -> encodePretty $ Aeson.toJSON x
  Left y -> show y
  where
    parser = orgToJsonList Pass0.documentParse

orgToJsonList :: BParser (Fix (OrgDocumentF Pass0.BlockPass0)) -> BParser [BlockJSON]
orgToJsonList pa = do
  a <- pa
  case unFix a of
    OrgDocEOFF _ -> return []
    OrgDocHeadingF x y -> c x y
    OrgDocListItemF x y -> c x y
    OrgDocParagraphF x y -> c x y
    OrgDocEmptyLineF _ y -> orgToJsonList $ return y
  where
    c x y = do
      y1 <- orgToJsonList $ return y
      (: y1) <$> toPJSON x

data BlockJSON = BlockJSON
  { offsetStart :: !Int,
    offsetEnd :: !Int,
    content :: !Text,
    block :: !Text
  }
  deriving stock (Show, Eq, Generic, Typeable)

instance Aeson.ToJSON BlockJSON where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

toPJSON :: (BlockName a) => BlockPass0 a -> BParser BlockJSON
toPJSON x = do
  input <- MP.getInput
  return $
    BlockJSON
      { offsetStart = Pass0.posStart x,
        offsetEnd = Pass0.posEnd x,
        content = input,
        block = blockName (_proxy x)
      }

_proxy :: BlockPass0 b -> Proxy b
_proxy _ = Proxy
