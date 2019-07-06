module Language.StdLib.Maths
  ( addMathStdLib
  )
where

import           Control.Monad.Except           ( throwError )

import           Language.Ast                   ( Value(Number) )
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

sinFunc :: [Value] -> InterpreterProcess Value
sinFunc args = case args of
  [rads] -> Number . sin <$> getNumberValue rads
  []     -> throwError "Must give sin function an argument"

cosFunc :: [Value] -> InterpreterProcess Value
cosFunc args = case args of
  [rads] -> Number . cos <$> getNumberValue rads
  []     -> throwError "Must give cos function an argument"

tanFunc :: [Value] -> InterpreterProcess Value
tanFunc args = case args of
  [rads] -> Number . tan <$> getNumberValue rads
  []     -> throwError "Must give tan function an argument"

absFunc :: [Value] -> InterpreterProcess Value
absFunc args = case args of
  [val] -> Number . abs <$> getNumberValue val
  []    -> throwError "Must give abs function an argument"

ceilFunc :: [Value] -> InterpreterProcess Value
ceilFunc args = case args of
  [val] -> Number . fromIntegral . ceiling <$> getNumberValue val
  []    -> throwError "Must give ceil function an argument"

floorFunc :: [Value] -> InterpreterProcess Value
floorFunc args = case args of
  [val] -> Number . fromIntegral . floor <$> getNumberValue val
  []    -> throwError "Must give floor function an argument"

roundFunc :: [Value] -> InterpreterProcess Value
roundFunc args = case args of
  [val] -> Number . fromIntegral . round <$> getNumberValue val
  []    -> throwError "Must give round function an argument"

maxFunc :: [Value] -> InterpreterProcess Value
maxFunc args = case args of
  [valA, valB] -> do
    a <- getNumberValue valA
    b <- getNumberValue valB
    return $ Number $ max a b
  [] -> throwError "Must give max function two arguments"

minFunc :: [Value] -> InterpreterProcess Value
minFunc args = case args of
  [valA, valB] -> do
    a <- getNumberValue valA
    b <- getNumberValue valB
    return $ Number $ min a b
  [] -> throwError "Must give min function two arguments"

logFunc :: [Value] -> InterpreterProcess Value
logFunc args = case args of
  [val] -> Number . log <$> getNumberValue val
  []    -> throwError "Must give log function an argument"

sqrtFunc :: [Value] -> InterpreterProcess Value
sqrtFunc args = case args of
  [val] -> Number . sqrt <$> getNumberValue val
  []    -> throwError "Must give sqrt function an argument"
