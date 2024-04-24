module Orgmode.Parser.Pass0 (BlockPass0 (..), nextBlock) where

import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Functor.Classes (Eq1 (..))
import Orgmode.Internal.Types
  ( BlockName (..),
    OrgDocumentF (..),
    ParagraphF (..),
  )
import Orgmode.Parser.Internal
  ( BParser,
    skipEmptyLine,
    skipToEndOfLine,
  )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MC

{- BlockPass0 only keep the position infomation of blocks -}
data BlockPass0 a = BlockPass0
  { posStart :: !MP.SourcePos,
    posEnd :: !MP.SourcePos
  }
  deriving stock (Eq, Show, Generic, Typeable)

instance (BlockName a) => ToJSON (BlockPass0 a) where
  toJSON x =
    Aeson.object
      [ "posStart" .= posStart x,
        "posEnd" .= posEnd x,
        "blockName" .= blockName (Proxy :: Proxy a)
      ]

{- Convert BlockPass0 -}
blockPassConvert :: Proxy y -> BlockPass0 a -> BlockPass0 y
blockPassConvert _ (BlockPass0 x y) = BlockPass0 x y

instance Eq1 BlockPass0 where
  liftEq _ = (==) . blockPassConvert Proxy

-- | When a paragraph (or similar block) parsing is finished,
--    the type of new block might be implied by how the old block is finished |
data BlockStartMark = HeadingStart | EmptyLineFull | EOFEnd | ListItemStart Int | ParagraphStart | UnknownStart deriving stock (Eq, Show, Generic, Typeable)

-- | We can certainly know a new block has started by
--    - End of file
--    - Heading Start
--    - Empty line
--    - List start |
blockStart :: BParser (BlockStartMark, BlockPass0 BlockStartMark)
blockStart =
  _wrap $
    MP.choice
      [ MP.eof $> EOFEnd,
        MC.char '*' $> HeadingStart,
        skipEmptyLine $> EmptyLineFull,
        (ListItemStart . length <$> MP.many MC.hspace)
          <* MP.choice
            [ MC.char '-',
              MC.char '+',
              MC.char '*',
              MC.lowerChar *> MC.char '.',
              MP.skipSome MC.digitChar *> MC.char '.'
            ]
      ]
  where
    _wrap x = do
      st <- MP.getSourcePos
      v <- x
      ed <- MP.getSourcePos
      return (v, BlockPass0 st ed)

_unknownHelper :: BParser x -> BParser (x, BlockStartMark, BlockPass0 BlockStartMark)
_unknownHelper py = do
  y <- py
  pos <- MP.getSourcePos
  return (y, UnknownStart, BlockPass0 pos pos)

-- | Given a block token is already parsed, we can parse the following blocks |
nextBlock :: BlockStartMark -> BlockPass0 BlockStartMark -> BParser (a -> OrgDocumentF BlockPass0 a, BlockStartMark, BlockPass0 BlockStartMark)
nextBlock EOFEnd pos = _unknownHelper $ return $ const $ OrgDocEOFF (blockPassConvert Proxy pos)
nextBlock EmptyLineFull pos = _unknownHelper $ return $ OrgDocEmptyLineF (blockPassConvert Proxy pos)
nextBlock HeadingStart pos = _unknownHelper $ OrgDocHeadingF . BlockPass0 (posStart pos) <$> (skipToEndOfLine *> MP.getSourcePos)
nextBlock (ListItemStart x) pos = _unknownHelper $ OrgDocListItemF . BlockPass0 (posStart pos) <$> (skipToEndOfLine *> MP.many (MP.try $ _list x) *> MP.getSourcePos)
  where
    _list x = MP.skipCount x MC.hspace *> MC.hspace1 *> notListStart *> skipToEndOfLine
    notListStart = MP.notFollowedBy $ MP.eitherP (MP.some MC.numberChar <* MC.char '.') (MC.char '-')
nextBlock ParagraphStart pos = do
  (x, y, z) <- _innerParagraph skipToEndOfLine blockStart
  return (OrgDocParagraphF x, y, z)
nextBlock UnknownStart pos = do
  start <- MP.optional blockStart
  case start of
    Just (x, y) -> nextBlock x y
    Nothing -> nextBlock ParagraphStart pos

-- | Parse parargrph helper function |
_innerParagraph :: BParser x -> BParser (end, BlockPass0 end) -> BParser (BlockPass0 (ParagraphF BlockPass0), end, BlockPass0 end)
_innerParagraph line end = do
  start <- MP.getSourcePos
  (endValue, endBlock) <- MP.skipManyTill line end
  return (BlockPass0 start $ posStart endBlock, endValue, endBlock)
