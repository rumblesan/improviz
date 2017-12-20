module Language.Parser where

import           Control.Monad         (void)
import           Data.Functor.Identity

import           GHC.Float             (double2Float)

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Indent    hiding (Block)
import           Text.Parsec.Language
import           Text.Parsec.Token

import           Language.Ast

type Indent = IndentT Identity

type LangParser e = IndentParser String () e

simpleParse :: LangParser a -> String -> Either ParseError a
simpleParse parser = runIndentParser parser () "program"

parseProgram :: String -> Either String Block
parseProgram prog =
  case simpleParse program prog of
    Right ast -> Right ast
    Left err  -> Left $ show err

program :: LangParser Block
program = topLevel >> skipMany space >> langBlock

exprDef :: GenLanguageDef String st Indent
exprDef =
  LanguageDef
  { commentStart = ""
  , commentEnd = ""
  , commentLine = "#"
  , nestedComments = True
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> oneOf "_'"
  , opStart = oneOf "^*/%+-^"
  , opLetter = oneOf "^*/%+-^"
  , reservedOpNames = ["^", "*", "/", "%", "+", "-"]
  , reservedNames = ["if"]
  , caseSensitive = True
  }

TokenParser { parens = m_parens
            , brackets = m_brackets
            , integer = m_integer
            , float = m_float
            , reservedOp = m_reservedOp
            , whiteSpace = m_whiteSpace
            , identifier = m_identifier
            , symbol = m_symbol
            } = makeTokenParser exprDef

table =
  [ [Prefix (m_reservedOp "-" >> return (UnaryOp "-"))]
  , [ Infix (m_reservedOp "^" >> return (BinaryOp "^")) AssocLeft
    , Infix (m_reservedOp "*" >> return (BinaryOp "*")) AssocLeft
    , Infix (m_reservedOp "/" >> return (BinaryOp "/")) AssocLeft
    , Infix (m_reservedOp "%" >> return (BinaryOp "%")) AssocLeft
    ]
  , [ Infix (m_reservedOp "+" >> return (BinaryOp "+")) AssocLeft
    , Infix (m_reservedOp "-" >> return (BinaryOp "-")) AssocLeft
    ]
  ]

atom :: LangParser Expression
atom =
  EApp <$> application <|> EVar <$> try variable <|> EVal <$> try value <|>
  try (m_parens expression)

langBlock :: LangParser Block
langBlock = Block <$> block element <?> "block"

element :: LangParser Element
element =
  ((ElLoop <$> loop) <|> (ElAssign <$> assignment) <|> (ElIf <$> ifElem) <|>
   (ElExpression <$> try expression)) <*
  eol <?> "element"

argList :: LangParser e -> LangParser [e]
argList lp = sepBy lp sep
  where
    sep = skipMany space >> char ',' >> skipMany space

application :: LangParser Application
application =
  Application <$> try (m_identifier <* m_symbol "(") <*> argList applicationArg <*
  m_symbol ")" <*>
  optionMaybe (indented >> langBlock) <?> "application"

applicationArg :: LangParser ApplicationArg
applicationArg = namedArg <|> unnamedArg
  where
    namedArg =
      try (ApplicationArg <$> (Just <$> m_identifier <* m_symbol "=")) <*>
      expression
    unnamedArg = ApplicationArg Nothing <$> expression

loop :: LangParser Loop
loop =
  Loop <$> try (expression <* m_symbol "times") <*>
  optionMaybe (m_symbol "with" *> m_identifier) <*>
  (indented >> langBlock) <?> "loop"

assignment :: LangParser Assignment
assignment =
  Assignment <$> try (m_identifier <* m_symbol "=") <*>
  expression <?> "assignment"

ifElem :: LangParser If
ifElem =
  If <$> try (m_symbol "if" *> m_parens expression) <*> langBlock <*>
  optionMaybe (m_symbol "else" *> langBlock) <?> "if"

expression :: LangParser Expression
expression = buildExpressionParser table atom <?> "expression"

variable :: LangParser Variable
variable = Variable <$> m_identifier

value :: LangParser Value
value = number <|> lambda <|> v_list <|> v_symbol <|> v_null
  where
    v_list = VList <$> m_brackets (argList expression) <?> "list"
    v_symbol = try (char ':') >> Symbol <$> m_identifier <?> "symbol"
    v_null = Null <$ m_symbol "null" <?> "null"

lambda :: LangParser Value
lambda =
  Lambda <$> m_parens (argList functionArg) <* m_symbol "=>" <*>
  (lbody <|> lexpr) <?> "lambda"
  where
    lexpr = (\e -> Block [ElExpression e]) <$> expression
    lbody = indented >> langBlock

functionArg :: LangParser FunctionArg
functionArg =
  FunctionArg <$> m_identifier <*> optionMaybe (m_symbol "=" *> value)

number :: LangParser Value
number =
  Number <$> (try (fmap double2Float m_float) <|> try m_intToFloat) <?> "number"
  where
    m_intToFloat = fmap fromIntegral m_integer

eol :: LangParser ()
eol = many newline *> eof <|> void (many newline)
