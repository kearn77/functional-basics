# Overview<br/>
This entry contains three modules: Poker_data_types, Determine_hands,<br/>
and Describe_hands.  Collectively, they allow the user to accurately<br/>
describe any five-card poker hand.  To demonstrate module usage,<br/>
Main.hs prints the string representation of five hands to standard<br/>
output.  Alternatively, the modules may be used interactively by<br/>
loading Describe_hands in ghci.<br/>

## [Poker_data_types](./Poker_data_types.hs)<br/>
The Poker_data_types module creates four algebraic data types - Rank,<br/>
Suit, Card, and HandRank.  Additionally, it provides two helper<br/>
functions, showRank and showSuit, which return string representations<br/>
of the Rank and Suit data types, respectively.<br/>

Algebraic types use multiple constructors to create an instance of a<br/>
type, either through a sum or product of its values.  A sum of<br/>
constructors invokes a single value out of a collection of<br/>
alternatives, while a product of constructors combines its values into<br/>
a record.[^1]<br/>

For example, Rank, the first type encountered in the module, is created<br/>
by separating value constructors - Two, Three, Four - by guards,<br/>
denoted by the ‘|’ symbol.  This is a sum operation.  Conversely, a<br/>
Card type is created by combining an instance of Rank and Suit.  This<br/>
is a product operation.<br/>

In addition to its value constructors, Rank contains a deriving clause,<br/>
‘deriving (Eq, Ord, Bounded, Enum, Show, Read)’.  The tuple that<br/>
follows the deriving keyword contains type-classes, and it states that<br/>
Rank is an instance of these type-classes.<br/>

Instances provide the Rank class with a number of useful operations and<br/>
functions, such as the ability to compare instances of Rank for<br/>
equality and enumerate over a collection of Ranks.  For an in-depth<br/>
explanation of these type-classes, please reference [Hoogle](https://hoogle.haskell.org/), a Haskell<br/>
API search engine.<br/>

## [Determine_hands](./Determine_hands.hs)<br/>
Given a list of five Cards, the Determine_hands module returns the<br/>
proper hand classification for the list.   Importing the four data<br/>
types created in the prior module, it creates four functions:<br/>
isStraight, isFlush, isPaired, and getHandRank.  getHandRank uses<br/>
predicate logic and calls to isStraight, isFlush, and isPaired to<br/>
return the appropriate HandRank.<br/>

Multiple calls to two functions, rank and suit, are made throughout<br/>
this module.  An observant reader will note that we never defined any<br/>
such functions.  How are we able to call functions we never explicitly<br/>
created?<br/>

As it turns out, defining the Card type with brackets gave us the rank<br/>
and suit functions for free, without the need for additional code.  We<br/>
could have defined Card without brackets, necessitating the creation of<br/>
some boiler-plate to access a Card’s rank and suit components.<br/>

Moreover, defining Card with brackets allows us to create an instance<br/>
of Card with any ordering of components.  We simply have to provide the<br/>
proper keywords during instantiation.  We can create an instance of<br/>
Card with any of the following expressions:<br/>

```
kingHearts = Card {suit=Hearts, rank=King}
aceSpades = Card {rank=Ace, suit=Spades}
queenDiamonds = Card Queen Diamonds
```

Determine_hands imports two functions that merit explanation - group<br/>
and sortOn.  As outlined on Hoogle,  group takes a list, groups that<br/>
list’s elements into sub-lists on the basis of equality, and returns<br/>
a list of lists.  Concatenating group’s output will return the<br/>
initial input.[^2]  By comparing the lengths of sub-lists, we determine<br/>
whether a hand is paired or not.  Please note that we must pass a<br/>
sorted list of Ranks to group to achieve this result.<br/>

sortOn takes two arguments, a function and a list, and returns the<br/>
sorted result of that function applied to every element of the list.[^3]<br/>
When combined with the group function, sortOn can return a list of<br/>
lengths corresponding to the pair signature of a hand.  For example, we<br/>
would expect an instance of TwoPair to have a signature of [1,2,2].<br/>

## [Describe_hands](./Describe_hands.hs)<br/>
The first four functions encountered in Describe_hands - getGroups,<br/>
flattenGroups, filterRanks, and isWheel - are called in nine<br/>
‘describe functions,’ which return a description of a five-card<br/>
poker hand.  A describe function exists for every possible HandRank.<br/>
The evalHand function uses guards, predicate logic, and the<br/>
aforementioned ‘describe’ functions to return the proper string<br/>
description of a hand.<br/>

While most of the functions defined in this module are straightforward,<br/>
flattenGroups and filterRanks are more involved.  The former takes two<br/>
arguments - an integer and a function - and uses composition to return<br/>
a function that filters a list of lists on length, concatenates the<br/>
lists into a single list, and applies the function argument to that<br/>
list.  The latter applies flattenGroups to a list of \[Ranks\] and<br/>
returns the string representation of a single Rank, which can represent<br/>
the maximum, minimum, head, or last element of a list.<br/>

In practice, these functions allow us to properly account for the<br/>
different ranks that compose paired hands.  For instance, the set and<br/>
pair components of a FullHouse, or the max pair and min pair of a<br/>
TwoPair.<br/>

[^1]: wiki.haskell.org. "Algebraic data type." 05/22/2023.<br/>
  https://wiki.haskell.org/Algebraic_data_type .

[^2]: Mitchell, Neil.  Hoogle. "group."  08/01/2023.<br/>
  https://hoogle.haskell.org/?hoogle=group .

[^3]: Mitchell, Neil.  Hoogle. "sortOn."  08/01/2023.<br/>
  https://hoogle.haskell.org/?hoogle=sortOn .
