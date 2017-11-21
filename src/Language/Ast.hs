module Language.Ast
  ( Block(..)
  , Element(..)
  , Application(..)
  , Loop(..)
  , Assignment(..)
  , Expression(..)
  , Variable(..)
  , Value(..)
  , If(..)
  , Identifier
  ) where

newtype Block =
  Block [Element]
  deriving (Eq, Show)

data Element
  = ElLoop Loop
  | ElAssign Assignment
  | ElExpression Expression
  | ElIf If
  deriving (Eq, Show)

data Application =
  Application Identifier
              [Expression]
              (Maybe Block)
  deriving (Eq, Show)

data Loop =
  Loop Expression
       (Maybe Identifier)
       Block
  deriving (Eq, Show)

data Assignment =
  Assignment Identifier
             Expression
  deriving (Eq, Show)

data If =
  If Expression
     Block
     (Maybe Block)
  deriving (Eq, Show)

data Expression
  = EApp Application
  | BinaryOp String
             Expression
             Expression
  | UnaryOp String
            Expression
  | EVar Variable
  | EVal Value
  deriving (Eq, Show)

newtype Variable =
  Variable Identifier
  deriving (Eq, Show)

data Value
  = Number Float
  | Null
  | Symbol String
  | Lambda [Identifier]
           Block
  | BuiltIn [Identifier]
  deriving (Eq, Show)

type Identifier = String
