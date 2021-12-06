{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module GenericMap
  (
  Map(..)
  ) where

import qualified Data.Map as M
-- import qualified Data.HashMap as HM
import qualified Data.IntMap as IM
--
--
--
class (m k v) => Map m k v where
  empty :: (m k v)
  alter :: (Ord k) => (Maybe v -> Maybe v) -> k -> (m k v) -> (m k v)

instance (Ord k) => Map (M.Map k v) where
  empty = M.empty
  alter = M.alter

-- instance Map (IM.IntMap v) Int v where
--   empty = IM.empty
--   alter = IM.alter
