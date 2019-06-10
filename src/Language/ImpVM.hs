module Language.ImpVM where

import           Data.Vector                    ( Vector )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad.State            ( execStateT )
import           Control.Monad                  ( void
                                                , when
                                                )
import           Lens.Simple                    ( use
                                                , assign
                                                )

import           Language.ImpVM.Types
import           Language.ImpVM.VM
import           Language.ImpVM.StdLib

run :: externalState -> Vector Instruction -> IO (VMState externalState)
run es prog = execStateT eval (cleanVM es)
 where
  eval = do
    assign program prog
    assign running True
    assign vmError Nothing
    addStdLib
    execute

execute :: VM es ()
execute = do
  pc   <- getProgramCounter
  prog <- use program
  inst <- readInstruction pc
  incrProgramCounter
  runInstruction inst
  runState <- use running
  when runState execute


runInstruction :: Instruction -> VM es ()
runInstruction (Push item)         = pushStack item
runInstruction Pop                 = void popStack
runInstruction (Operator op      ) = runOp op
runInstruction (Jump     address ) = setProgramCounter address
runInstruction (RelJump  distance) = do
  pc <- use programCounter
  setProgramCounter $ pc + distance
runInstruction (Branch address) = do
  v <- popStack
  when (v /= SFloat 0) $ setProgramCounter address
runInstruction (Constant item   ) = pushStack item
runInstruction (Load     address) = do
  v <- readAddress address
  pushStack v
runInstruction (Save address) = do
  v <- popStack
  writeAddress address v
runInstruction (Call address args) = do
  setCallstack
  setProgramCounter address
runInstruction (BuiltIn name args) = do
  bi <- use builtins
  fromMaybe (setError errMsg) (M.lookup name bi)
  where errMsg = "no builtin called " ++ name
runInstruction Return = do
  a <- popCallstack
  setProgramCounter a
runInstruction End = assign running False

runOp :: Op -> VM es ()
runOp AddOp = do
  i1 <- popStack
  i2 <- popStack
  case (i1, i2) of
    (SFloat v1, SFloat v2) -> pushStack $ SFloat (v1 + v2)
    _                      -> setError "invalid args for add"
runOp SubOp = do
  i1 <- popStack
  i2 <- popStack
  case (i1, i2) of
    (SFloat v1, SFloat v2) -> pushStack $ SFloat (v1 - v2)
    _                      -> setError "invalid args for sub"
runOp MultOp = do
  i1 <- popStack
  i2 <- popStack
  case (i1, i2) of
    (SFloat v1, SFloat v2) -> pushStack $ SFloat (v1 * v2)
    _                      -> setError "invalid args for mult"
runOp DivOp = do
  i1 <- popStack
  i2 <- popStack
  case (i1, i2) of
    (SFloat v1, SFloat v2) -> pushStack $ SFloat (v1 / v2)
    _                      -> setError "invalid args for div"
