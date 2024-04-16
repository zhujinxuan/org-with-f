module Org.Parser.Pass0 ()
  where

import qualified Org.Types as T
import qualified Text.Megaparsec as MP
import a

data BlockPass0 a = BlockPass0
  { posStart :: Int,
    posEnd :: Int
  }


type BParser m a = MP.ParsecT OrgError Text m a
data OrgError = OrgError

pEOF :: BParser BlockPass0 (T.EOF BlockPass0 a)
pEOF = MP.eof

pBlock :: BParser BlockPass0 (T.BlockF BlockPass0 a)
pBlock = undefined
