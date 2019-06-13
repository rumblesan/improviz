{-# LANGUAGE TemplateHaskell #-}

module Language.ImpVM.VM where

import qualified Data.Vector                   as V
import qualified Data.Map                      as M
import           Data.Vector                    ( (!?)
                                                , (//)
                                                )

import           Lens.Simple                    ( use
                                                , assign
                                                , makeLenses
                                                )

import           Language.ImpVM.Types

makeLenses ''VMState

cleanVM :: externalState -> M.Map String StackItem -> VMState externalState
cleanVM es extVars = VMState { _programCounter = 0
                             , _program        = V.empty
                             , _opstack        = []
                             , _callstack      = []
                             , _memory         = V.replicate 1000 SNull
                             , _builtins       = M.empty
                             , _running        = False
                             , _vmError        = Nothing
                             , _externalVars   = extVars
                             , _externalState  = es
                             }

readInstruction :: Int -> VM es Instruction
readInstruction address = do
  pg <- use program
  case pg !? address of
    Just i  -> return i
    Nothing -> setError "Instruction address out of bounds" >> return End

readAddress :: Int -> VM es StackItem
readAddress address = do
  mem <- use memory
  case mem !? address of
    Just i  -> return i
    Nothing -> setError "Memory address out of bounds" >> return SNull

writeAddress :: Int -> StackItem -> VM es ()
writeAddress address value = do
  mem <- use memory
  let updatedMem = mem // [(address, value)]
  assign memory updatedMem

readExternal :: String -> VM es StackItem
readExternal name = do
  exts <- use externalVars
  return $ M.findWithDefault SNull name exts

pushStack :: StackItem -> VM es ()
pushStack item = do
  st <- use opstack
  assign opstack $ item : st

popStack :: VM es StackItem
popStack = do
  st <- use opstack
  assign opstack $ tail st
  return $ head st

setProgramCounter :: Int -> VM es ()
setProgramCounter = assign programCounter

getProgramCounter :: VM es Int
getProgramCounter = use programCounter

incrProgramCounter :: VM es ()
incrProgramCounter = do
  npc <- getProgramCounter
  setProgramCounter (npc + 1)

setCallstack :: VM es ()
setCallstack = do
  cs <- use callstack
  pc <- use programCounter
  assign callstack $ pc : cs

popCallstack :: VM es Int
popCallstack = do
  cs <- use callstack
  assign callstack $ tail cs
  return $ head cs

setError :: String -> VM es ()
setError msg = do
  assign running False
  assign vmError (Just $ ImpVMError msg)
