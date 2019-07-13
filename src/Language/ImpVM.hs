module Language.ImpVM where

import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.Map                      as M
import           Control.Monad.State            ( execStateT )
import           Control.Monad                  ( void
                                                , when
                                                , replicateM
                                                )
import           Lens.Simple                    ( use
                                                , assign
                                                )

import           Language.ImpVM.Types
import           Language.ImpVM.VM
import           Language.ImpVM.StdLib          ( builtInFuncs )

import           Gfx.Context                    ( GfxContext )

cleanVM :: GfxContext -> M.Map String StackItem -> VMState GfxContext
cleanVM es extVars = VMState { _programCounter = 0
                             , _program        = V.empty
                             , _opstack        = []
                             , _callstack      = []
                             , _memory         = V.replicate 1000 SNull
                             , _builtins       = builtInFuncs
                             , _running        = False
                             , _vmError        = Nothing
                             , _externalVars   = extVars
                             , _externalState  = es
                             }

run
  :: GfxContext
  -> M.Map String StackItem
  -> Vector Instruction
  -> IO (VMState GfxContext)
run es extVars prog = execStateT eval (cleanVM es extVars)
 where
  eval = do
    assign program prog
    assign running True
    assign vmError Nothing
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
runInstruction (Constant item) = pushStack item
runInstruction (External name) = do
  v <- readExternal name
  pushStack v
runInstruction (Load address) = do
  v <- readAddress address
  pushStack v
runInstruction (Save address) = do
  v <- popStack
  writeAddress address v
runInstruction (Call address args) = do
  setCallstack
  setProgramCounter address
runInstruction (BuiltIn name numArgs) = do
  bi <- use builtins
  case M.lookup name bi of
    Nothing   -> setError $ "no builtin called " ++ name
    Just func -> do
      args <- replicateM numArgs popStack
      func args
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
runOp EQOp = do
  i1 <- popStack
  i2 <- popStack
  pushStack $ SFloat $ if i1 == i2 then 1 else 0
runOp NEQOp = do
  i1 <- popStack
  i2 <- popStack
  pushStack $ SFloat $ if i1 == i2 then 0 else 1