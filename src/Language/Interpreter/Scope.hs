module Language.Interpreter.Scope
  ( ScopeStack(..)
  , empty
  , setVariable
  , getVariable
  , newScope
  , popScope
  )
where

import qualified Data.Map.Strict               as M

data ScopeStack k v = ScopeStack
  { current :: M.Map k v
  , stack   :: [M.Map k v]
  } deriving (Eq, Show)

empty :: Ord k => ScopeStack k v
empty = ScopeStack { current = M.empty, stack = [] }

setVariable :: Ord k => ScopeStack k v -> k -> v -> ScopeStack k v
setVariable scope name value =
  scope { current = M.insert name value (current scope) }

getVariable :: Ord k => ScopeStack k v -> k -> Maybe v
getVariable scope name = M.lookup name (current scope)

newScope :: Ord k => ScopeStack k v -> ScopeStack k v
newScope scope = scope { stack = current scope : stack scope }

popScope :: ScopeStack k v -> Either String (ScopeStack k v)
popScope scope = case stack scope of
  []                -> Left "Cannot pop scope"
  newCur : newStack -> Right $ scope { current = newCur, stack = newStack }
