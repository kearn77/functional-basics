-- Script imports.
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

{-
In practice, we can simplify the code contained within combinations_one.
In fact, we can return all combinations k taken from a collection n in a 
single line.  For example, consider the list of integers zero to three.
-}

nums :: [(Int, Int)]
nums = [(x,y) | x <- [0..3], y <- [x..3], x /= y]

{-
We can apply this concept to the collection twos - a list of strings -
in a variety of ways.  

For example, we can use the subscript operator - !! -  in conjunction with the 
comprehension logic outlined in nums.  
-}

twos :: [String]
pocketTwos :: [(String, String)]

twos = ["2c","2d","2h","2s"]
pocketTwos = [(twos !! x, twos !! y) | x <- [0..length twos - 1], y <- [x..length twos -1], x /= y]

{-
As outlined on Hoogle, the subscript operator is a partial function, meaning it 
is not capable of handling all possible arguments.  Alternative functions 
are often recommended in lieu of !!.  Below is an example using Map, one of 
Haskell's implementations of a hash map.
-}

deuces :: Map Int String
pocketTwosSnd :: [(String, String)]

deuces = Map.fromList (zip [0..] twos)
pocketTwosSnd = [(fromMaybe "0" (Map.lookup x deuces), fromMaybe "0" (Map.lookup y deuces)) | 
                    x <- [0..length deuces - 1], 
                    y <- [x..length deuces - 1], 
                    x /= y]



