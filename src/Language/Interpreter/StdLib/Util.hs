module Language.Interpreter.StdLib.Util
  ( addUtilStdLib
  )
where

import           Control.Monad                  ( mapM_ )
import           Control.Monad.Except           ( throwError )
import           Control.Monad.Trans            ( liftIO )
import           System.Random                  ( random
                                                , mkStdGen
                                                )

import           Logging                        ( logInfo )
import           Lens.Simple                    ( use
                                                , assign
                                                )
import           Language.Ast
import           Language.Interpreter           ( getExternal
                                                , getSystemVar
                                                , setBuiltIn
                                                , useGfxCtx
                                                )
import           Gfx.Context                    ( setDepthChecking )
import           Language.Interpreter.Types
import           Language.Interpreter.Values

addUtilStdLib :: InterpreterProcess ()
addUtilStdLib = do
  setBuiltIn "isNull"     isNullFunc
  setBuiltIn "ext"        getExtFunc
  setBuiltIn "sysVar"     getSysVarFunc
  setBuiltIn "length"     lengthFunc
  setBuiltIn "random"     randomFunc
  setBuiltIn "randomSeed" randomSeedFunc
  setBuiltIn "debug"      debugFunc
  setBuiltIn "depthCheck" depthCheck

isNullFunc :: [Value] -> InterpreterProcess Value
isNullFunc args = case args of
  []      -> throwError "Need to provide isNull with argument"
  arg : _ -> return $ if valIsNull arg then Number 1 else Number 0

getExtFunc :: [Value] -> InterpreterProcess Value
getExtFunc args = case args of
  Symbol name : defaultValue : _ -> getExternal name defaultValue
  [Symbol name] -> getExternal name Null
  _ -> throwError "Need to provide ext with a name"

getSysVarFunc :: [Value] -> InterpreterProcess Value
getSysVarFunc args = case args of
  Symbol name : _ -> getSystemVar name
  _               -> throwError "Need to provide systemVar with a name"

lengthFunc :: [Value] -> InterpreterProcess Value
lengthFunc args = case args of
  VList elems : _ -> return $ Number $ fromIntegral $ length elems
  _               -> throwError "Must give length a list"

randomFunc :: [Value] -> InterpreterProcess Value
randomFunc _ = do
  (v, newRng) <- random <$> use rnGen
  assign rnGen newRng
  return $ Number v

randomSeedFunc :: [Value] -> InterpreterProcess Value
randomSeedFunc args = case args of
  Number seed : _ -> do
    assign rnGen (mkStdGen $ round seed)
    return Null
  _ -> throwError "Must give a number argument for seed function"

debugFunc :: [Value] -> InterpreterProcess Value
debugFunc args = mapM_ (liftIO . logInfo . show) args >> return Null

depthCheck :: [Value] -> InterpreterProcess Value
depthCheck args = case args of
  v : _ -> useGfxCtx (`setDepthChecking` (v /= Number 0)) >> return Null
  _     -> throwError "Must give a value argument to depth checking"

