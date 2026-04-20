module Lexer where
import Data.Text hiding (empty)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Token (LexToken(..), WithPos(..))
import Token qualified

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
symbol :: Text -> Parser Text
symbol = L.symbol sc

parseToken :: Parser (WithPos LexToken)
parseToken = Token.withPos (symbol "let" >> pure Let)