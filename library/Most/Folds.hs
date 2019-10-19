module Most.Folds where

import Most.Prelude
import Control.Foldl
import qualified Data.IntMap.Strict as IntMap


intMap :: Fold (Int, a) (IntMap a)
intMap =
  Fold
    (\ intMap (key, value) -> IntMap.insert key value intMap)
    IntMap.empty
    id
