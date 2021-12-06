-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module GenericMap
  (
  Map(..)
  ) where

import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HML
import qualified Data.IntMap as IM
import Data.Hashable


-- data family Map k v

-- class Key k where
--   data Map m k :: * -> *
--   empty :: m
--   alter :: (Maybe v -> Maybe v) -> k -> m -> m
--
-- instance Key Int
--
--
--
class Map m k v | m -> k, m -> v where
  empty :: m
  alter :: (Ord k) => (Maybe v -> Maybe v) -> k -> m -> m
--
instance (Ord k) => Map (M.Map k v) k v where
  empty = M.empty
  alter = M.alter

instance Map (IM.IntMap v) Int v where
  empty = IM.empty
  alter = IM.alter

instance (Ord k, Hashable k) => Map (HML.HashMap k v) k v where
  empty = HML.empty
  alter = HML.alter
