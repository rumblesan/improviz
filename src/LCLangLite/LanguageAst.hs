module LanguageAst where

data Block = Block [Element] deriving (Eq, Show)

data Element = ElApplication Application
             | ElLoop Loop
             | ElAssign Assignment
             deriving (Eq, Show)

data Loop = Loop Integer (Maybe Identifier) Block
          deriving (Eq, Show)

data Assignment = AsVar Identifier Expression
                | AsLambda Lambda
                deriving (Eq, Show)

data Application = Application Identifier [Expression] (Maybe Block)
                 deriving (Eq, Show)

data Expression = EApp Application
                | BinaryOp String Expression Expression
                | UnaryOp String Expression
                | EVar Variable
                | ENum Number
                | ELam Lambda
                deriving (Eq, Show)

data Lambda = Lambda [Identifier] Expression deriving (Eq, Show)

data Variable = Variable Identifier deriving (Eq, Show)

data Number = Number Double deriving (Eq, Show)

type Identifier = String
