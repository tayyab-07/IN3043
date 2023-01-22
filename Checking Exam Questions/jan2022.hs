module Jan2022 where

acronymMine :: String -> String
acronymMine s = [head n | n <- words s]


acronym :: String -> String
acronym s = [c | (c:_) <- words s]