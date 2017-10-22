
import Data.List

findSame :: [String] -> [String] -> String
findSame _ [] = ""
findSame [] _ = ""
findSame (x:xs) (y:ys) = 
    if x `elem` (y:ys) then
        x ++ findSame xs (delete x (y:ys))
    else 
        findSame xs (y:ys)