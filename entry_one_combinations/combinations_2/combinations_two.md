## Combinations Continued

In our first entry, we used recursion to generate all possible combinations of<br/>
pocket twos from a standard, fifty-two card deck.  It served as a nice<br/>
introduction to some core features of the language - type signatures, function<br/>
definitions, and recursion.  

Although it served as a nice introduction, we can simplify the code outlined<br/>
in combinations_1.   In fact, we can return all combinations k taken from a<br/>
collection n in a single line, without recursion.  For example, consider the<br/>
list of integers zero to three. 

```
nums = [(x,y) | x <- [0..3], y <- [x..3], x /= y]
```

Printing nums to standard output will show that we have exhausted all possible<br/>
combinations of numbers zero through three.  We can visualize the<br/>
comprehension in the following manner:

1. x = 0; y = 0,1,2,3 => ~~(0,0)~~, (0,1), (0,2), (0,3)
2. x = 1; y = 1,2,3 => ~~(1,1)~~,(1,2), (1,3)
3. x = 2; y = 2,3 => ~~(2,2)~~,(2,3)
4. x = 3; y = 3 => ~~(3,3)~~

## The Subscript Operator
We can apply this same concept to a list of strings.  For example, we can use<br/>
the subscript operator - !! -  in conjunction with the comprehension logic<br/>
outlined in nums to arrive at all combinations of pocket twos.

```
twos = ["2c","2d","2h","2s"]
pocketTwos = [(twos !! x, twos !! y) | 
                x <- [0..length twos - 1], 
                y <- [x..length twos -1], 
                x /= y]
```

The subscript operator returns the value associated with an index.  Therefore,<br/>
passing x and y to the subscript operator will return their concomitant twos.

## Data.Map

As outlined on Hoogle, a Haskell API search engine,  the subscript operator is<br/>
a partial function, meaning it is not capable of handling all possible<br/>
arguments.  Alternative functions are often recommended in lieu of !! to avoid<br/>
runtime errors.  Bound errors - requesting an index value that is not present<br/>
in the list - are a common side effect of index operations across programming<br/>
languages.  Alternatively, we can use one of Haskell’s implementations of a<br/>
hash map - a collection of key => value pairs - to arrive at the same<br/>
collection of twos.

```
deuces = Map.fromList (zip [0..] twos)
pocketTwosSnd = [(fromMaybe "0" (Map.lookup x deuces), fromMaybe "0" (Map.lookup y deuces)) | 
                    x <- [0..length deuces - 1], 
                    y <- [x..length deuces - 1], 
                    x /= y]
```

First, we create a list of tuples of the type [(Integer, String)].  This is<br/>
accomplished by passing two lists to the zip function, which pairs elements<br/>
from two lists until one of the lists is exhausted.  In our case, numbers zero<br/>
through N are paired with the string representations of twos.  This list of<br/>
tuples is then passed to the fromList function to generate our hash map,<br/>
"deuces." 
  
Once our hash map has been instantiated, we use our comprehension logic to<br/>
create a list titled pocketTwosSnd, although the information before the guard<br/>
is slightly more involved than with the subscript operator.

The Map type returns two values - "just" and "nothing," otherwise known as<br/>
"maybe" types.  The former indicates that the key exists in the Map, while the<br/>
latter indicates the opposite.  For instance, "Map.lookup 0 deuces" returns<br/>
"just 2c," of type "maybe String."
  
We can retrieve the underlying value associated with "maybe" by calling the<br/>
function fromMaybe.  fromMaybe retrieves "just" values from the map, returning<br/>
a default output - in our case "0" - in the event that "nothing" is found.
 
In summary, pocketTwosSnd composes a tuple by calling the lookup function,<br/>
once with x and once with y, and passing the results to fromMaybe.

And there you have it.  Some alternative solutions for counting ducks.
