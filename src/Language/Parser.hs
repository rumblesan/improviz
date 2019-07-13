module Language.Parser where

import           Control.Monad                  ( void )
import           Data.Void
import           Data.Monoid                    ( (<>) )

import           GHC.Float                      ( double2Float )

import           Text.Megaparsec
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , space1
                                                , letterChar
                                                , newline
                                                , string
                                                , char
                                                )
import           Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer    as L

import           Language.Ast
import           Language.Parser.Errors         ( ParserError )


type Parser = Parsec Void String

lineCmnt = L.skipLineComment "//"
blockCmnt = L.skipBlockComment "/*" "*/"

scn :: Parser ()
scn = L.space (void $ takeWhile1P Nothing f) lineCmnt blockCmnt
  where f x = x == '\t' || x == '\n'

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineCmnt blockCmnt
  where f x = x == ' '

whitespace :: Parser ()
whitespace = L.space space1 lineCmnt blockCmnt

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

squares :: Parser a -> Parser a
squares = between (symbol "[") (symbol "]")

comma :: Parser String
comma = symbol ","

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = ["if", "else", "null", "func", "times", "with", "time"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
  p = (:) <$> letterChar <*> many alphaNumChar
  check x = if x `elem` rws
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x

eol :: Parser ()
eol = void $ many newline

-- Improviz Language Parser

parseProgram :: String -> Either ParserError Program
parseProgram = parse program "program"

simpleParse :: Parser a -> String -> Either ParserError a
simpleParse parser = parse parser "program"

prettyPrintError :: ParserError -> String
prettyPrintError = errorBundlePretty

program :: Parser Program
program = between whitespace eof programBlock

programBlock :: Parser Program
programBlock = Program <$> many statement <?> "program block"

statement :: Parser Statement
statement = L.nonIndented scn statement' <?> "statement"

statement' :: Parser Statement
statement' =
  (   (StIf <$> ifElem)
    <|> (StAssign <$> assignment)
    <|> (StFunc <$> functionDef)
    <|> (StLoop <$> loop)
    <|> (StExpression <$> expression)
    )
    <* eol

operators =
  [ [Prefix (UnaryOp <$> symbol "-")]
  , [ InfixL (BinaryOp <$> symbol "^")
    , InfixL (BinaryOp <$> symbol "*")
    , InfixL (BinaryOp <$> symbol "/")
    , InfixL (BinaryOp <$> symbol "%")
    ]
  , [InfixL (BinaryOp <$> symbol "+"), InfixL (BinaryOp <$> symbol "-")]
  , [ InfixL (BinaryOp <$> symbol "<")
    , InfixL (BinaryOp <$> symbol ">")
    , InfixL (BinaryOp <$> symbol "<=")
    , InfixL (BinaryOp <$> symbol ">=")
    , InfixL (BinaryOp <$> symbol "==")
    , InfixL (BinaryOp <$> symbol "!=")
    ]
  , [InfixL (BinaryOp <$> symbol "&&"), InfixL (BinaryOp <$> symbol "||")]
  ]

exprs :: Parser Expression
exprs =
  EApp
    <$> application
    <|> EList
    <$> list
    <|> EVar
    <$> try variable
    <|> EVal
    <$> try value
    <|> try (parens expression)

element :: Parser Element
element =
  (   (ElIf <$> ifElem)
    <|> (ElAssign <$> assignment)
    <|> (ElLoop <$> loop)
    <|> (ElExpression <$> try expression)
    )
    <?> "element"

application :: Parser Application
application = L.indentBlock scn ap
 where
  ap = do
    ilevel <- L.indentLevel
    apVar  <- try (variable <* symbol "(")
    args   <- sepBy expression comma
    void $ symbol ")"
    return
      (L.IndentMany (Just (ilevel <> defaultTabWidth))
                    (return . (Application apVar args . blk))
                    element
      )
  blk []    = Nothing
  blk elems = Just $ Block elems

functionDef :: Parser Func
functionDef = L.indentBlock scn fb
 where
  fb = do
    ilevel <- L.indentLevel
    rword "func"
    name     <- identifier
    args     <- parens $ sepBy functionArg comma
    exprBody <- optional $ symbol "=>" *> expression
    return $ case exprBody of
      Just expr -> L.IndentNone (Func name args $ Block [ElExpression expr])
      Nothing   -> L.IndentSome (Just (ilevel <> defaultTabWidth))
                                (return . Func name args . Block)
                                element


functionArg :: Parser FuncArg
functionArg =
  (VarArg <$> identifier) <|> (BlockArg <$> (char '&' *> identifier))

loop :: Parser Loop
loop = L.indentBlock scn l
 where
  l = do
    ilevel   <- L.indentLevel
    loopExpr <- try (expression <* symbol "times")
    loopVar  <- optional (rword "with" *> identifier)
    return
      (L.IndentSome (Just (ilevel <> defaultTabWidth))
                    (return . (Loop loopExpr loopVar . Block))
                    element
      )

assignment :: Parser Assignment
assignment = absAssignment <|> condAssignment
 where
  absAssignment =
    AbsoluteAssignment
      <$> try (identifier <* symbol "=")
      <*> expression
      <?> "absolute assignment"
  condAssignment =
    ConditionalAssignment
      <$> try (identifier <* symbol ":=")
      <*> expression
      <?> "conditional assignment"

ifElem :: Parser If
ifElem = do
  i  <- ifBlock
  el <- optional elseBlock
  return $ i el

ifBlock :: Parser (Maybe Block -> If)
ifBlock = L.indentBlock scn i
 where
  i = do
    ilevel <- L.indentLevel
    rword "if"
    predicate <- parens expression
    return
      (L.IndentSome (Just (ilevel <> defaultTabWidth))
                    (return . If predicate . Block)
                    element
      )


elseBlock :: Parser Block
elseBlock = L.indentBlock scn i
 where
  i = do
    ilevel <- L.indentLevel
    rword "else"
    return
      (L.IndentSome (Just (ilevel <> defaultTabWidth)) (return . Block) element)

expression :: Parser Expression
expression = makeExprParser exprs operators >>= recurParsePostExpr

recurParsePostExpr :: Expression -> Parser Expression
recurParsePostExpr e =
  optional (postExpr e) >>= maybe (return e) recurParsePostExpr

postExpr :: Expression -> Parser Expression
postExpr e = EAccess e <$> squares expression

list :: Parser [Expression]
list = squares (sepBy expression comma)

accessor :: Parser Expression
accessor = EAccess <$> expression <*> squares expression

variable :: Parser Variable
variable = GlobalVariable <$> symbol "time" <|> LocalVariable <$> identifier

value :: Parser Value
value = v_number <|> v_symbol <|> v_null
 where
  v_symbol = try (char ':') >> Symbol <$> identifier <?> "symbol"
  v_null   = Null <$ rword "null" <?> "null"
  v_number = Number <$> number

number :: Parser Float
number =
  lexeme $ (double2Float <$> try L.float) <|> (fromIntegral <$> L.decimal)
