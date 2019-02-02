module Language.StdLib.Maths
  ( addMathStdLib
  )
where

import           Language.Ast
import           Language.Interpreter           ( getVariable
                                                , setBuiltIn
                                                , setVariable
                                                )
import           Language.Interpreter.Types
import           Language.Interpreter.Values

addMathStdLib :: InterpreterProcess ()
addMathStdLib = do
  setVariable "pi" (Number pi)
  setBuiltIn "sin"   sinFunc   [VarArg "rads"]
  setBuiltIn "cos"   cosFunc   [VarArg "rads"]
  setBuiltIn "tan"   tanFunc   [VarArg "rads"]
  setBuiltIn "abs"   absFunc   [VarArg "val"]
  setBuiltIn "ceil"  ceilFunc  [VarArg "val"]
  setBuiltIn "floor" floorFunc [VarArg "val"]
  setBuiltIn "round" roundFunc [VarArg "val"]
  setBuiltIn "max"   maxFunc   [VarArg "vala", VarArg "valb"]
  setBuiltIn "min"   minFunc   [VarArg "vala", VarArg "valb"]
  setBuiltIn "log"   logFunc   [VarArg "val"]
  setBuiltIn "sqrt"  sqrtFunc  [VarArg "val"]

sinFunc :: InterpreterProcess Value
sinFunc = do
  rads <- getVariable "rads" >>= getNumberValue
  return $ Number $ sin rads

cosFunc :: InterpreterProcess Value
cosFunc = do
  rads <- getVariable "rads" >>= getNumberValue
  return $ Number $ cos rads

tanFunc :: InterpreterProcess Value
tanFunc = do
  rads <- getVariable "rads" >>= getNumberValue
  return $ Number $ tan rads

absFunc :: InterpreterProcess Value
absFunc = do
  val <- getVariable "val" >>= getNumberValue
  return $ Number $ abs val

ceilFunc :: InterpreterProcess Value
ceilFunc = do
  val <- getVariable "val" >>= getNumberValue
  return $ Number $ fromIntegral $ ceiling val

floorFunc :: InterpreterProcess Value
floorFunc = do
  val <- getVariable "val" >>= getNumberValue
  return $ Number $ fromIntegral $ floor val

roundFunc :: InterpreterProcess Value
roundFunc = do
  val <- getVariable "val" >>= getNumberValue
  return $ Number $ fromIntegral $ round val

maxFunc :: InterpreterProcess Value
maxFunc = do
  vala <- getVariable "vala" >>= getNumberValue
  valb <- getVariable "valb" >>= getNumberValue
  return $ Number $ max vala valb

minFunc :: InterpreterProcess Value
minFunc = do
  vala <- getVariable "vala" >>= getNumberValue
  valb <- getVariable "valb" >>= getNumberValue
  return $ Number $ min vala valb

logFunc :: InterpreterProcess Value
logFunc = do
  val <- getVariable "val" >>= getNumberValue
  return $ Number $ log val

sqrtFunc :: InterpreterProcess Value
sqrtFunc = do
  val <- getVariable "val" >>= getNumberValue
  return $ Number $ sqrt val
