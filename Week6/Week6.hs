module Week6 where

import Data.Set 
import qualified Data.Set as Set

unique :: Ord a => [a] -> [a]
unique u = Set.elems (Set.fromList u)