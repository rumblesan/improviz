module LCLangLite.LanguageParser where

import Control.Monad (void)

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import LCLangLite.LanguageAst

type LangParser e = Parsec String () e

exprDef :: LanguageDef ()
exprDef = emptyDef { opStart         = oneOf "^*/%+-^"
                   , opLetter        = oneOf "^*/%+-^"
                   , reservedOpNames = ["^", "*", "/", "%", "+", "-"]
}

TokenParser { parens = m_parens
            , integer = m_integer
            , float = m_float
            , reservedOp = m_reservedOp
            , whiteSpace = m_whiteSpace
            , identifier = m_identifier
            , symbol = m_symbol
            , braces = m_braces
} = makeTokenParser exprDef

table = [ [Prefix (m_reservedOp "-" >> return (UnaryOp "-"))]
        , [Infix (m_reservedOp "^" >> return (BinaryOp "^")) AssocLeft,
           Infix (m_reservedOp "*" >> return (BinaryOp "*")) AssocLeft,
           Infix (m_reservedOp "/" >> return (BinaryOp "/")) AssocLeft
          ]
        ,
          [Infix (m_reservedOp "+" >> return (BinaryOp "+")) AssocLeft,
           Infix (m_reservedOp "-" >> return (BinaryOp "-")) AssocLeft
          ]
        ]

atom :: LangParser Expression
atom = try (m_parens expression)
       <|> EApp <$> try application
       <|> EVar <$> try variable
       <|> EVal <$> try value

block :: LangParser Block
block = Block <$> many element <?> "block"

element :: LangParser Element
element = (    (ElLoop <$> try loop)
          <|> (ElAssign <$> try assignment)
          <|> (ElExpression <$> try expression))
          <* eos
          <?> "element"

application :: LangParser Application
application = Application <$> m_identifier <*> m_parens (many expression) <*> optionMaybe (m_braces block) <?> "application"

loop :: LangParser Loop
loop = Loop <$> m_integer <* m_symbol "times" <*> optionMaybe (m_symbol "with" *> m_identifier) <*> m_braces block <?> "loop"

assignment :: LangParser Assignment
assignment = Assignment <$> m_identifier <* m_symbol "=" <*> expression <?> "assignment"

expression :: LangParser Expression
expression = buildExpressionParser table atom <?> "expression"

variable :: LangParser Variable
variable = Variable <$> m_identifier

value :: LangParser Value
value = number <|> lambda <|> v_null
  where
    v_null = Null <$ m_symbol "null" <?> "null"

lambda :: LangParser Value
lambda = Lambda <$> m_parens (many m_identifier) <* m_symbol "=>" <*> (lbody <|> lexpr) <?> "lambda"
  where
    lexpr = (\e -> Block [ElExpression e]) <$> expression
    lbody = m_braces block

number :: LangParser Value
number = Number <$> (try m_float <|> try m_intToFloat) <?> "number"
  where
    m_intToFloat = fmap fromIntegral m_integer


parseProgram :: String -> Either ParseError Block
parseProgram = runParser block () "program"

eos :: LangParser Char
eos = char ';' <* many newline

eol :: LangParser ()
eol = void newline <|> eof
