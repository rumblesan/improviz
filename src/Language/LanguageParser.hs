module Language.LanguageParser where

import Control.Monad (void)
import Data.Functor.Identity

import GHC.Float (double2Float)

import Text.Parsec
import Text.Parsec.Indent hiding (Block)
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Language.LanguageAst

type Indent = IndentT Identity
type LangParser e = IndentParser String () e

simpleParse :: LangParser a -> String -> Either ParseError a
simpleParse parser = runIndentParser parser () "program"

parseProgram :: String -> Either String Block
parseProgram prog = case simpleParse program prog of
  Right ast -> Right ast
  Left err -> Left $ show err

program :: LangParser Block
program = topLevel >> langBlock

exprDef :: GenLanguageDef String st Indent
exprDef = LanguageDef { commentStart   = ""
                      , commentEnd     = ""
                      , commentLine    = "#"
                      , nestedComments = True
                      , identStart     = letter <|> char '_'
                      , identLetter    = alphaNum <|> oneOf "_'"
                      , opStart        = oneOf "^*/%+-^"
                      , opLetter       = oneOf "^*/%+-^"
                      , reservedOpNames= ["^", "*", "/", "%", "+", "-"]
                      , reservedNames  = []
                      , caseSensitive  = True
                      }

TokenParser { parens = m_parens
            , integer = m_integer
            , float = m_float
            , reservedOp = m_reservedOp
            , whiteSpace = m_whiteSpace
            , identifier = m_identifier
            , symbol = m_symbol
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
atom =              try (m_parens expression)
       <|> EApp <$> try application
       <|> EVar <$> try variable
       <|> EVal <$> try value

langBlock :: LangParser Block
langBlock = Block
            <$> block element
            <?> "block"

element :: LangParser Element
element =    ((ElLoop <$> try loop)
          <|> (ElAssign <$> try assignment)
          <|> (ElExpression <$> try expression))
          <* eol
          <?> "element"

argList :: LangParser e -> LangParser [e]
argList lp = m_parens (sepBy lp sep)
  where sep = skipMany space >> char ',' >> skipMany space

application :: LangParser Application
application = Application
              <$> m_identifier
              <*> argList expression
              <*> optionMaybe (indented >> langBlock)
              <?> "application"

loop :: LangParser Loop
loop = Loop
       <$> expression <* m_symbol "times"
       <*> optionMaybe (m_symbol "with" *> m_identifier)
       <*> (indented >> langBlock)
       <?> "loop"

assignment :: LangParser Assignment
assignment = Assignment
             <$> m_identifier
             <* m_symbol "="
             <*> expression
             <?> "assignment"

expression :: LangParser Expression
expression = buildExpressionParser table atom <?> "expression"

variable :: LangParser Variable
variable = Variable <$> m_identifier

value :: LangParser Value
value = number <|> lambda <|> v_null
  where
    v_null = Null <$ m_symbol "null" <?> "null"

lambda :: LangParser Value
lambda = Lambda
         <$> argList m_identifier
         <* m_symbol "=>"
         <*> (lbody <|> lexpr)
         <?> "lambda"
  where
    lexpr = (\e -> Block [ElExpression e]) <$> expression
    lbody = indented >> langBlock

number :: LangParser Value
number = Number
         <$> (try (fmap double2Float m_float) <|> try m_intToFloat)
         <?> "number"
  where
    m_intToFloat = fmap fromIntegral m_integer

eol :: LangParser ()
eol = many newline *> eof
      <|> void (many newline)

