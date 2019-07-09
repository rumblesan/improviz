module Language.ImpVM.StdLib.Maths
  ( mathBuiltIns
  )
where


import           Language.ImpVM.Types           ( VM
                                                , StackItem(..)
                                                )
import           Language.ImpVM.VM              ( pushStack
                                                , setError
                                                )

mathBuiltIns :: [(String, [StackItem] -> VM es ())]
mathBuiltIns =
  [ ("sin"  , sinFunc)
  , ("cos"  , cosFunc)
  , ("tan"  , tanFunc)
  , ("abs"  , absFunc)
  , ("ceil" , ceilFunc)
  , ("floor", floorFunc)
  , ("round", roundFunc)
  , ("max"  , maxFunc)
  , ("min"  , minFunc)
  , ("log"  , logFunc)
  , ("sqrt" , sqrtFunc)
  ]

sinFunc :: [StackItem] -> VM es ()
sinFunc args = case args of
  [SFloat rads] -> pushStack $ SFloat $ sin rads
  []            -> setError "Must give sin function a valid argument"

cosFunc :: [StackItem] -> VM es ()
cosFunc args = case args of
  [SFloat rads] -> pushStack $ SFloat $ cos rads
  []            -> setError "Must give cos function a valid argument"

tanFunc :: [StackItem] -> VM es ()
tanFunc args = case args of
  [SFloat rads] -> pushStack $ SFloat $ tan rads
  []            -> setError "Must give tan function a valid argument"

absFunc :: [StackItem] -> VM es ()
absFunc args = case args of
  [SFloat val] -> pushStack $ SFloat $ abs val
  []           -> setError "Must give abs function a valid argument"

ceilFunc :: [StackItem] -> VM es ()
ceilFunc args = case args of
  [SFloat val] -> pushStack $ SFloat $ fromIntegral $ ceiling val
  []           -> setError "Must give ceil function a valid argument"

floorFunc :: [StackItem] -> VM es ()
floorFunc args = case args of
  [SFloat val] -> pushStack $ SFloat $ fromIntegral $ floor val
  []           -> setError "Must give floor function a valid argument"

roundFunc :: [StackItem] -> VM es ()
roundFunc args = case args of
  [SFloat val] -> pushStack $ SFloat $ fromIntegral $ round val
  []           -> setError "Must give round function a valid argument"

maxFunc :: [StackItem] -> VM es ()
maxFunc args = case args of
  [SFloat a, SFloat b] -> pushStack $ SFloat $ max a b
  []                   -> setError "Must give max function two arguments"

minFunc :: [StackItem] -> VM es ()
minFunc args = case args of
  [SFloat a, SFloat b] -> pushStack $ SFloat $ min a b
  []                   -> setError "Must give min function two arguments"

logFunc :: [StackItem] -> VM es ()
logFunc args = case args of
  [SFloat val] -> pushStack $ SFloat $ log val
  []           -> setError "Must give log function a valid argument"

sqrtFunc :: [StackItem] -> VM es ()
sqrtFunc args = case args of
  [SFloat val] -> pushStack $ SFloat $ sqrt val
  []           -> setError "Must give sqrt function a valid argument"
