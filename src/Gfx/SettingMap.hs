{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes #-}

module Gfx.SettingMap
  ( SettingMap(..)
  , create
  , value
  , reset
  , snapshot
  ) where

import qualified Data.Map.Strict               as M

import           Lens.Simple                    ( Lens'
                                                , (^.)
                                                , at
                                                , lens
                                                , makeLenses
                                                , set
                                                )

data Ord k => SettingMap k v = SettingMap
  { _valueMap      :: M.Map k v
  , _defaultValues :: M.Map k v
  }
  deriving (Eq, Show)

makeLenses ''SettingMap

create :: Ord k => [(k, v)] -> SettingMap k v
create defaults =
  SettingMap { _valueMap = M.empty, _defaultValues = M.fromList defaults }

getter :: Ord k => k -> SettingMap k v -> Maybe v
getter key settingMap = case settingMap ^. valueMap . at key of
  Nothing  -> settingMap ^. defaultValues . at key
  Just val -> Just val

setter :: Ord k => k -> SettingMap k v -> Maybe v -> SettingMap k v
setter key settingMap value = set (valueMap . at key) value settingMap

value :: Ord k => k -> Lens' (SettingMap k v) (Maybe v)
value key = lens (getter key) (setter key)

save :: Ord k => SettingMap k v -> M.Map k v
save = _valueMap

load :: Ord k => SettingMap k v -> M.Map k v -> SettingMap k v
load settingMap values = set valueMap values settingMap

snapshot :: Ord k => Lens' (SettingMap k v) (M.Map k v)
snapshot = lens save load

reset :: Ord k => SettingMap k v -> SettingMap k v
reset settingMap = set valueMap (settingMap ^. defaultValues) settingMap


