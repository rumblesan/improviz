module Gfx.Setting
  ( Setting(..)
  , create
  , value
  , reset
  , resetIfUnused
  )
where

import           Lens.Simple                    ( Lens
                                                , lens
                                                )

data Setting v = Setting
  { currentValue :: v
  , defaultValue :: v
  , useCurrent :: Bool
  , setLastFrame :: Bool
  } deriving (Eq, Show)

create :: k -> Setting k
create value = Setting { currentValue = value
                       , defaultValue = value
                       , useCurrent   = False
                       , setLastFrame = False
                       }

get :: Setting k -> k
get setting =
  if useCurrent setting then currentValue setting else defaultValue setting

set :: Setting k -> k -> Setting k
set setting value =
  setting { currentValue = value, useCurrent = True, setLastFrame = True }

value :: Lens (Setting v) (Setting v) v v
value = lens get set

reset :: Setting k -> Setting k
reset setting = setting { currentValue = defaultValue setting
                        , useCurrent   = True
                        , setLastFrame = False
                        }

resetIfUnused :: Setting k -> Setting k
resetIfUnused setting =
  setting { useCurrent = setLastFrame setting, setLastFrame = False }
