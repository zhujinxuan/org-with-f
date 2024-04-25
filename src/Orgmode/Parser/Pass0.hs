{-# LANGUAGE StrictData #-}

module Orgmode.Parser.Pass0 (
  BlockPass0 (..),
  nextBlock,
  documentParse,
)
where

import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Fix (Fix (..))
import Data.Functor.Classes (Eq1 (..))
import Orgmode.Internal.Types (
  BlockName (..),
  OrgDocumentF (..),
 )
import Orgmode.Parser.Internal (
  BParser,
  parseIndent,
  skipEmptyLine,
  skipIndent,
  skipToEndOfLine,
 )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as Lexer
import qualified Text.Megaparsec.Byte as MP

{- BlockPass0 only keep the position infomation of blocks -}
data BlockPass0 a = BlockPass0
  { posStart :: Int
  , posEnd :: Int
  }
  deriving stock (Eq, Show, Generic, Typeable)


{- Convert BlockPass0 -}
blockPassConvert :: Proxy y -> BlockPass0 a -> BlockPass0 y
blockPassConvert _ (BlockPass0 x y) = BlockPass0 x y

instance Eq1 BlockPass0 where
  liftEq _ = (==) . blockPassConvert Proxy

{- | When a paragraph (or similar block) parsing is finished,
   the type of new block might be implied by how the old block is finished |
-}
data BlockStartMark
  = HeadingStart
  | EmptyLineFull
  | EOFEnd
  | ListItemStart Int
  | ParagraphStart Int
  deriving stock (Eq, Show, Generic, Typeable)

{- | We can certainly know a new block has started by
   - End of file
   - Heading Start
   - Empty line
   - List start |
-}
blockStart :: BParser (BlockStartMark, BlockPass0 BlockStartMark)
blockStart =
  _wrap $
    MP.choice
      [ MP.eof $> EOFEnd
      , MC.char '*' $> HeadingStart
      , skipEmptyLine $> EmptyLineFull
      , (ListItemStart <$> parseIndent)
          <* _listStart
      ]
  where
    _wrap x = do
      st <- MP.getOffset
      v <- x
      ed <- MP.getOffset
      return (v, BlockPass0 st ed)

-- | Start of List after indent
_listStart :: BParser Char
_listStart =
  MP.choice
    [ MC.char '-'
    , MC.char '+'
    , MC.char '*'
    , MC.lowerChar *> MC.char '.'
    , MP.skipSome MC.digitChar *> MC.char '.'
    ]

-- | Given a block token is already parsed, we can parse the following blocks
_nextBlock :: BlockStartMark -> BlockPass0 BlockStartMark -> BParser (a -> OrgDocumentF BlockPass0 a)
_nextBlock EOFEnd pos = return $ const $ OrgDocEOFF (blockPassConvert Proxy pos)
_nextBlock EmptyLineFull pos = return $ OrgDocEmptyLineF (blockPassConvert Proxy pos)
_nextBlock HeadingStart pos = OrgDocHeadingF . BlockPass0 (posStart pos) <$> (skipToEndOfLine *> MP.getOffset)
_nextBlock (ListItemStart _) pos =
  OrgDocListItemF . BlockPass0 (posStart pos) <$> do
    ind <- MC.hspace *> Lexer.indentLevel
    skipToEndOfLine
    MP.skipMany (MP.try $ _list ind)
    MP.getOffset
  where
    _list ind = skipIndent ind *> MC.hspace1 *> MP.notFollowedBy _listStart *> skipToEndOfLine
_nextBlock (ParagraphStart x) pos =
  OrgDocParagraphF <$> do
    skipToEndOfLine
    MP.skipManyTill line newBlock
    BlockPass0 (posStart pos) <$> MP.getOffset
  where
    line = skipIndent x *> MP.notFollowedBy MC.space *> skipToEndOfLine
    newBlock =
      MP.lookAhead $
        MP.choice
          [ blockStart $> ()
          , skipIndent x *> MC.space1
          ]

nextBlock :: BParser (a -> OrgDocumentF BlockPass0 a)
nextBlock = do
  start <- MP.optional blockStart
  case start of
    Just (x, y) -> _nextBlock x y
    Nothing -> do
      x <- _takeIndent
      p <- MP.getOffset
      _nextBlock (ParagraphStart x) $ BlockPass0 p p

-- | Parse the document
documentParse :: BParser (Fix (OrgDocumentF BlockPass0))
documentParse = do
  block <- MP.eitherP nextBlock MP.eof
  case block of
    Right _ -> Fix . OrgDocEOFF <$> currentBlockPass0
    Left nextB -> Fix . nextB <$> documentParse

currentBlockPass0 :: BParser (BlockPass0 a)
currentBlockPass0 = do
  pos <- MP.getOffset
  return $ BlockPass0 pos pos

-- | Consume indent
_takeIndent :: BParser Int
_takeIndent = do
  i <- MP.getOffset
  MC.hspace
  (\x -> x - i) <$> MP.getOffset
