module Language.StdLib.Maths
  ( addMathStdLib
  )
where

import           Control.Monad.Except           ( throwError )

import           Language.Ast                   ( Block
                                                , Value(Number)
                                                )
import           Language.Interpreter           ( setBuiltIn
                                                , setVariable
                                                )
import           Language.Interpreter.Types     ( InterpreterProcess )
import           Language.Interpreter.Values    ( getNumberValue )

addMathStdLib :: InterpreterProcess ()
addMathStdLib = do
  setVariable "pi" (Number pi)
  setBuiltIn "sin"   sinFunc
  setBuiltIn "cos"   cosFunc
  setBuiltIn "tan"   tanFunc
  setBuiltIn "abs"   absFunc
  setBuiltIn "ceil"  ceilFunc
  setBuiltIn "floor" floorFunc
  setBuiltIn "round" roundFunc
  setBuiltIn "max"   maxFunc
  setBuiltIn "min"   minFunc
  setBuiltIn "log"   logFunc
  setBuiltIn "sqrt"  sqrtFunc

sinFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
sinFunc args _ = case args of
  [rads] -> Number . sin <$> getNumberValue rads
  []     -> throwError "Must give sin function an argument"

cosFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
cosFunc args _ = case args of
  [rads] -> Number . cos <$> getNumberValue rads
  []     -> throwError "Must give cos function an argument"

tanFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
tanFunc args _ = case args of
  [rads] -> Number . tan <$> getNumberValue rads
  []     -> throwError "Must give tan function an argument"

absFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
absFunc args _ = case args of
  [val] -> Number . abs <$> getNumberValue val
  []    -> throwError "Must give abs function an argument"

ceilFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
ceilFunc args _ = case args of
  [val] -> Number . fromIntegral . ceiling <$> getNumberValue val
  []    -> throwError "Must give ceil function an argument"

floorFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
floorFunc args _ = case args of
  [val] -> Number . fromIntegral . floor <$> getNumberValue val
  []    -> throwError "Must give floor function an argument"

roundFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
roundFunc args _ = case args of
  [val] -> Number . fromIntegral . round <$> getNumberValue val
  []    -> throwError "Must give round function an argument"

maxFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
maxFunc args _ = case args of
  [valA, valB] -> do
    a <- getNumberValue valA
    b <- getNumberValue valB
    return $ Number $ max a b
  [] -> throwError "Must give max function two arguments"

minFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
minFunc args _ = case args of
  [valA, valB] -> do
    a <- getNumberValue valA
    b <- getNumberValue valB
    return $ Number $ min a b
  [] -> throwError "Must give min function two arguments"

logFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
logFunc args _ = case args of
  [val] -> Number . log <$> getNumberValue val
  []    -> throwError "Must give log function an argument"

sqrtFunc :: [Value] -> Maybe Block -> InterpreterProcess Value
sqrtFunc args _ = case args of
  [val] -> Number . sqrt <$> getNumberValue val
  []    -> throwError "Must give sqrt function an argument"
