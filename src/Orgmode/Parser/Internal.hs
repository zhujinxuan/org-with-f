module Orgmode.Parser.Internal (BParser, OrgConfig (..), OrgErr (..), skipEmptyLine, skipToEndOfLine) where

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer (charLiteral)

type BParser a = MP.ParsecT OrgErr Text (Reader OrgConfig) a

data OrgConfig = OrgConfig
  { orgTodoKeywords1 :: ![Text],
    orgElements :: ![Text]
  }
  deriving stock (Eq, Show, Generic, Typeable)

data OrgErr = OrgErr deriving stock (Eq, Ord, Show, Generic, Typeable)

skipEmptyLine :: BParser ()
skipEmptyLine = MP.manyTill MC.hspace (MP.eitherP MC.eol MP.eof) $> ()

skipToEndOfLine :: BParser ()
skipToEndOfLine = MP.manyTill charLiteral (MP.eitherP MC.eol MP.eof) $> ()
