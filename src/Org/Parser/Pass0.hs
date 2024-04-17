module Org.Parser.Pass0 (BlockPass0 (..), pBlock) where

import Data.Functor.Classes (Eq1 (..))
import Org.Types
import Text.Megaparsec qualified as MP
import qualified Text.Megaparsec.Char as MC
import Text.Megaparsec.Char.Lexer (charLiteral)

data BlockPass0 a = BlockPass0
  { posStart :: !MP.SourcePos,
    posEnd :: !MP.SourcePos
  }
  deriving stock (Eq, Show, Generic, Typeable)

blockPassConvert :: BlockPass0 a -> BlockPass0 y
blockPassConvert (BlockPass0 x y) = BlockPass0 x y

instance Eq1 BlockPass0 where
  liftEq _ x y = blockPassConvert x == y

type BParser a = MP.ParsecT OrgErr Text (Reader OrgConfig) a

data OrgConfig = OrgConfig
  { orgTodoKeywords1 :: [Text],
    orgElements :: [Text]
  }
  deriving stock (Eq, Show, Generic, Typeable)

data OrgErr = OrgErr deriving stock (Eq, Ord, Show, Generic, Typeable)

wrapPass0 :: Proxy y -> BParser x -> BParser (BlockPass0 y)
wrapPass0 _ p = BlockPass0 <$> (MP.getSourcePos <* MP.try p) <*> MP.getSourcePos


skipEmptyLines :: BParser (BlockPass0 ())
skipEmptyLines = wrapPass0 Proxy (MP.many MC.hspace *> MP.eitherP MC.eol MP.eof)

skipToEndOfLine :: BParser ()
skipToEndOfLine = MP.manyTill charLiteral (MP.eitherP MP.eol MP.eof) $> ()

pHeading :: BParser (BlockPass0 (HeadingF BlockPass0))
pHeading = wrapPass0 Proxy (MC.char '*' *> skipToEndOfLine)

pParagraph :: BParser (BlockPass0 (ParagraphF BlockPass0))
pParagraph = undefined

pBlock :: BParser (a -> DocumentF BlockPass0 a)
pBlock =
  skipEmptyLines
    *> MP.choice
      [
        DocEmptyLineF <$> skipEmptyLines,
        DocHeadingF <$> pHeading,
        DocParagraphF <$> pParagraph
      ]
