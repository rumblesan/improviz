module Language.Ast
  ( Program(..)
  , Statement(..)
  , Block(..)
  , Element(..)
  , Application(..)
  , ApplicationArg(..)
  , Lambda(..)
  , Func(..)
  , FuncArg(..)
  , Loop(..)
  , Assignment(..)
  , Expression(..)
  , Variable(..)
  , Value(..)
  , If(..)
  , Identifier
  )
where

import           Language.Interpreter.Scope     ( ScopeStack )

newtype Program =
  Program [Statement]
  deriving (Eq, Show)

data Statement
  = StLoop Loop
  | StAssign Assignment
  | StExpression Expression
  | StIf If
  | StFunc Func
  deriving (Eq, Show)

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
  Application Variable
              [ApplicationArg]
              (Maybe Lambda)
  deriving (Eq, Show)

data ApplicationArg
  = ApplicationSingleArg Expression
  | ApplicationSpreadArg Expression
  deriving (Eq, Show)

data Loop =
  Loop Expression
       (Maybe Identifier)
       Block
  deriving (Eq, Show)

data Assignment
  = AbsoluteAssignment Identifier
                       Expression
  | ConditionalAssignment Identifier
                          Expression
  deriving (Eq, Show)

newtype If =
  If [(Expression, Block)]
  deriving (Eq, Show)

data Lambda =
  Lambda [FuncArg]
         (Maybe (ScopeStack Identifier Value))
         Block deriving (Eq, Show)

data Func =
  Func Identifier
       [FuncArg]
       Block
  deriving (Eq, Show)

data FuncArg = VarArg Identifier | BlockArg Identifier deriving (Eq, Show)

data Expression
  = EApp Application
  | BinaryOp String
             Expression
             Expression
  | UnaryOp String
            Expression
  | EVar Variable
  | EVal Value
  | EList [Expression]
  | EAccess Expression Expression
  deriving (Eq, Show)

data Variable
  = LocalVariable Identifier
  | GlobalVariable Identifier
  deriving (Eq, Show)

data Value
  = Number Float
  | Null
  | Symbol String
  | LambdaRef Lambda
  | VList [Value]
  | BuiltInFunctionRef Identifier
  deriving (Eq, Show)

type Identifier = String
