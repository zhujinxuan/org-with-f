{-# LANGUAGE StrictData #-}

module Orgmode.Parser.Internal
  ( BParser,
    OrgConfig (..),
    OrgErr (..),
    skipEmptyLine,
    skipToEndOfLine,
    skipIndent,
    parseIndent,
  )
where

import Data.Char (isSpace)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer (charLiteral)

type BParser a = MP.ParsecT OrgErr Text (Reader OrgConfig) a

data OrgConfig = OrgConfig
  { orgTodoKeywords1 :: [Text],
    orgElements :: [Text]
  }
  deriving stock (Eq, Show, Generic, Typeable)

data OrgErr = OrgErr deriving stock (Eq, Ord, Show, Generic, Typeable)

skipEmptyLine :: BParser ()
skipEmptyLine = MC.hspace *> MP.eitherP MC.eol MP.eof $> ()

skipToEndOfLine :: BParser ()
skipToEndOfLine = MP.manyTill charLiteral (MP.eitherP MC.eol MP.eof) $> ()


-- | Is it a horizontal space character?
isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'

skipIndent :: MP.Pos -> BParser ()
skipIndent x = MP.skipCount (MP.unPos x) (MP.satisfy isHSpace) *> MP.notFollowedBy MC.space1

parseIndent :: BParser MP.Pos
parseIndent = MP.mkPos . length . take 100 <$> MP.many (MP.satisfy isHSpace)
