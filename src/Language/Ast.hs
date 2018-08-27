module Language.Ast
  ( Block(..)
  , Element(..)
  , Application(..)
  , ApplicationArg(..)
  , Func(..)
  , Loop(..)
  , Assignment(..)
  , Expression(..)
  , Variable(..)
  , Value(..)
  , FunctionArg(..)
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
  | ElFunc Func
  deriving (Eq, Show)

data Application =
  Application Identifier
              [ApplicationArg]
              (Maybe Block)
  deriving (Eq, Show)

data ApplicationArg =
  ApplicationArg (Maybe Identifier)
                 Expression
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

data Func =
  Func Identifier
       [FunctionArg]
       Block
  deriving (Eq, Show)

data FunctionArg =
  FunctionArg Identifier
              (Maybe Value)
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
  | Lambda [FunctionArg]
           Block
  | VList [Expression]
  | BuiltIn Identifier
            [Identifier]
  deriving (Eq, Show)

type Identifier = String
