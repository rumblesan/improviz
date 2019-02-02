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
  } deriving (Show)

empty :: Ord k => ScopeStack k v
empty = ScopeStack { current = M.empty, stack = [] }

setVariable :: Ord k => ScopeStack k v -> k -> v -> ScopeStack k v
setVariable scope name value =
  scope { current = M.insert name value (current scope) }

getVariable :: Ord k => ScopeStack k v -> k -> Maybe v
getVariable scope name = case M.lookup name (current scope) of
  Just v  -> Just v
  Nothing -> getVariable' (stack scope) name

getVariable' :: Ord k => [M.Map k v] -> k -> Maybe v
getVariable' (s : rest) name = case M.lookup name s of
  Just v  -> Just v
  Nothing -> getVariable' rest name
getVariable' [] _ = Nothing

newScope :: Ord k => ScopeStack k v -> ScopeStack k v
newScope scope =
  scope { current = M.fromList [], stack = current scope : stack scope }

popScope :: ScopeStack k v -> Either String (ScopeStack k v)
popScope scope = case stack scope of
  []                -> Left "Cannot pop scope"
  newCur : newStack -> Right $ scope { current = newCur, stack = newStack }
