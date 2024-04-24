{-# LANGUAGE StrictData #-}

module Orgmode.Parser.Pass0
  ( BlockPass0 (..),
    nextBlock,
  )
where

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
    parseIndent,
    skipEmptyLine,
    skipIndent,
    skipToEndOfLine,
  )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MC
import qualified Text.Megaparsec.Char.Lexer as Lexer

{- BlockPass0 only keep the position infomation of blocks -}
data BlockPass0 a = BlockPass0
  { posStart :: MP.SourcePos,
    posEnd :: MP.SourcePos
  }
  deriving stock (Eq, Show, Generic, Typeable)

instance (BlockName a) => ToJSON (BlockPass0 a) where
  toJSON x =
    Aeson.object
      [ "posStart" .= _posObject (posStart x),
        "posEnd" .= _posObject (posEnd x),
        "blockName" .= blockName (Proxy :: Proxy a)
      ]

_posObject :: MP.SourcePos -> Aeson.Value
_posObject x =
  Aeson.object
    [ "line" .= MP.unPos (MP.sourceLine x),
      "column" .= MP.unPos (MP.sourceColumn x)
    ]

{- Convert BlockPass0 -}
blockPassConvert :: Proxy y -> BlockPass0 a -> BlockPass0 y
blockPassConvert _ (BlockPass0 x y) = BlockPass0 x y

instance Eq1 BlockPass0 where
  liftEq _ = (==) . blockPassConvert Proxy

-- | When a paragraph (or similar block) parsing is finished,
--    the type of new block might be implied by how the old block is finished |
data BlockStartMark
  = HeadingStart
  | EmptyLineFull
  | EOFEnd
  | ListItemStart MP.Pos
  | ParagraphStart MP.Pos
  | UnknownStart
  deriving stock (Eq, Show, Generic, Typeable)

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
        (ListItemStart <$> parseIndent)
          <* _listStart
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

-- | Start of List after indent
_listStart :: BParser Char
_listStart =
  MP.choice
    [ MC.char '-',
      MC.char '+',
      MC.char '*',
      MC.lowerChar *> MC.char '.',
      MP.skipSome MC.digitChar *> MC.char '.'
    ]

-- | Given a block token is already parsed, we can parse the following blocks
nextBlock :: BlockStartMark -> BlockPass0 BlockStartMark -> BParser (a -> OrgDocumentF BlockPass0 a, BlockStartMark, BlockPass0 BlockStartMark)
nextBlock EOFEnd pos = _unknownHelper $ return $ const $ OrgDocEOFF (blockPassConvert Proxy pos)
nextBlock EmptyLineFull pos = _unknownHelper $ return $ OrgDocEmptyLineF (blockPassConvert Proxy pos)
nextBlock HeadingStart pos = _unknownHelper $ OrgDocHeadingF . BlockPass0 (posStart pos) <$> (skipToEndOfLine *> MP.getSourcePos)
nextBlock (ListItemStart x) pos = _unknownHelper $ do
  skipToEndOfLine
  MP.skipMany (MP.try _list)
  OrgDocListItemF . BlockPass0 (posStart pos) <$> MP.getSourcePos
  where
    _list = skipIndent x *> MC.hspace1 *> MP.notFollowedBy _listStart *> skipToEndOfLine
nextBlock (ParagraphStart x) _ = do
  (a, y, z) <- _innerParagraph (skipIndent x *> MP.notFollowedBy MC.space *> skipToEndOfLine) blockStart
  return (OrgDocParagraphF a, y, z)
nextBlock UnknownStart pos = do
  start <- MP.optional blockStart
  case start of
    Just (x, y) -> nextBlock x y
    Nothing -> do
      x <- _takeIndent
      nextBlock (ParagraphStart x) pos

-- | Consume indent
_takeIndent :: BParser MP.Pos
_takeIndent = MC.hspace *> Lexer.indentLevel

-- | Parse parargrph helper function |
_innerParagraph :: BParser x -> BParser (end, BlockPass0 end) -> BParser (BlockPass0 (ParagraphF BlockPass0), end, BlockPass0 end)
_innerParagraph line end = do
  start <- MP.getSourcePos
  (endValue, endBlock) <- MP.skipManyTill line end
  return (BlockPass0 start $ posStart endBlock, endValue, endBlock)
