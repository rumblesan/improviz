module LanguageParser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import LanguageAst

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
atom =     m_parens expression
       <|> fmap EApp (try application)
       <|> fmap EVal value
       <|> fmap (EVar . Variable) (m_identifier <* m_whiteSpace)

block :: LangParser Block
block = Block <$> many element

element :: LangParser Element
element =     (ElApplication <$> application)
          <|> (ElLoop <$> loop)
          <|> (ElAssign <$> assignment)

expression :: LangParser Expression
expression = buildExpressionParser table atom <?> "expression"

lambda :: LangParser Lambda
lambda = Lambda <$> many m_identifier <* m_symbol "=>" <*> expression

loop :: LangParser Loop
loop = Loop <$> m_integer <* m_symbol "times" <*> optionMaybe (m_symbol "with" <* m_identifier) <*> m_braces block

assignment :: LangParser Assignment
assignment = AsVar <$> m_identifier <*> expression

application :: LangParser Application
application = Application <$> m_identifier <*> m_parens (many expression) <*> optionMaybe (m_braces block)

number :: LangParser Number
number = Number <$> (m_intToFloat <|> m_float)
  where
    m_intToFloat = fmap fromIntegral m_integer

value :: LangParser Value
value = Num <$> number <|> m_null
  where m_null = Null <$ m_symbol "null"

parseProgram :: String -> Either ParseError Block
parseProgram = runParser block () "program"
