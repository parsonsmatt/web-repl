module WebRepl.Expr.Parser where

import           Protolude             hiding (try)

import qualified Data.Text             as Text
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

import           WebRepl.Expr

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        (void spaceChar)
        (L.skipLineComment "//")
        (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

integer :: Parser Integer
integer = lexeme L.integer

str :: [Char] -> Parser [Char]
str = lexeme . string

lchar :: Char -> Parser Char
lchar = lexeme . char

add :: Parser [Char]
add = lexeme (string "add")

var :: Parser Expr
var = Var . Text.pack <$> lexeme (some letterChar)

intExpr :: Parser Expr
intExpr = LInt <$> integer

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

addExpr :: Parser Expr
addExpr = do
    add
    lchar '('
    e1 <- expr
    lchar ','
    e2 <- expr
    lchar ')'
    pure (Add e1 e2)

expr :: Parser Expr
expr = intExpr <|> addExpr <|> var

parseOnly :: Text -> Maybe Expr
parseOnly = parseMaybe (spaceConsumer *> expr)

stmt :: Parser Stmt
stmt = def <|> print'

def :: Parser Stmt
def = do
    str "def"
    valName <- Text.pack <$> lexeme (some letterChar)
    str "="
    val <- expr
    pure (Assign valName val)

print' :: Parser Stmt
print' = do
    str "print("
    val <- expr
    lchar ')'
    pure (Print val)

program :: Parser Program
program = stmt `sepEndBy1` lchar ';'

programOrExpr :: Parser (Either Program Expr)
programOrExpr = Left <$> try program <|> Right <$> try expr
