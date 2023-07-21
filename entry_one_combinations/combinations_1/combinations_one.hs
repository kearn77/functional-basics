-- Imports.
import Data.List

-- FUNCTION SIGNATURES AND DEFINITIONS.

-- Compare two tuples for equality, regardless of element order.
compareTuples :: Eq a => (a,a) -> (a,a) -> Bool
compareTuples x y   | x == y = True
                    | x == (snd y, fst y) = True
                    | otherwise = False

-- Remove duplicate tuples from a list of tuples, regardless of element order.
removeTuples :: Eq a => [(a,a)] -> [(a,a)]
removeTuples [] = []
removeTuples (x:xs) = x : removeTuples y
    where
        y = [z | z <- xs, not (compareTuples x z)]

-- FUNCTION IMPLEMENTATIONS

-- Return permutations of pocket twos as a list of tuples.
pocketTwos :: [(String, String)]
pocketTwos = [(x,y) | x <- ["2c","2d","2h","2s"], y <- ["2c","2d","2h","2s"], x /= y]

-- Pass pocketTwos to removeTuples to get all combinations of pocket twos.
combosTwos :: [(String, String)]
combosTwos = removeTuples pocketTwos

{-
Assuming our dealer is either a genie or a mechanic, we can return all possible
combinations of pocket twos with replacement.
-}
twosReplacement :: [(String, String)]
twosReplacement = removeTuples [(x,y) | x <- ["2c", "2d", "2h", "2s"], y <- ["2c", "2d", "2h", "2s"]]

{-
Note that our function will work with any type that belongs to the Eq class.
For example, we can return combinations taken from a collection of numbers.
-}
numList :: [(Integer, Integer)]
numList = removeTuples [(n,k) | n <- [10,20..50], k <- [10,20..50], n /= k]

{-
It is not always necessary to call removeTuples to get combinations of items x, y.
However, passing bigSlick into removeTuples will have no undesired effects.
No removals will take place. 
-}
bigSlick :: [(String, String)]
bigSlick = [(x,y) | x <- ["Ac","Ad","Ah","As"], y <- ["Kc","Kd","Kh","Ks"]]

bigSlickCopy :: [(String, String)]
bigSlickCopy = removeTuples bigSlick
