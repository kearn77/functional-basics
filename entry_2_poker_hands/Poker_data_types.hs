module Poker_data_types where

import Text.Printf (printf)

{-- 
Create four types: Rank, Suit, Card, and HandRank.
Rank and Suit compose instances of type Card.
HandRank provides the rankings for all poker hands.

In games where suit can function as a tie breaker -
for instance, seven card stud's bring in - Suit 
can derive Ord and Bounded, as is shown here.
--}
data Rank = Two|Three|Four|Five
            |Six|Seven|Eight|Nine
            |Ten|Jack|Queen|King|Ace
            deriving (Eq,Ord,Bounded,Enum,Show,Read)

data Suit = Clubs|Diamonds|Hearts|Spades
            deriving (Eq,Show,Enum,Read)

data Card = Card {
     rank :: Rank,
     suit :: Suit
     } deriving (Eq,Show,Read)

data HandRank = HighCard|Pair|TwoPair|ThreeKind
                |Straight|Flush|FullHouse|FourKind
                |StraightFlush
                deriving (Eq,Ord,Bounded,Enum,Show,Read)

showRank :: Rank -> String
showRank x |x == Two = "Two"
           |x == Three = "Three"
           |x == Four = "Four"
           |x == Five = "Five"
           |x == Six = "Six"
           |x == Seven = "Seven"
           |x == Eight = "Eight"
           |x == Nine = "Nine"
           |x == Ten = "Ten"
           |x == Jack = "Jack"
           |x == King = "King"
           |x == Queen = "Queen"
           |x == Ace = "Ace"
           |otherwise = "Invalid rank."

showSuit :: Suit -> String
showSuit x |x == Clubs = "Clubs"
           |x == Diamonds = "Diamonds"
           |x == Hearts = "Hearts"
           |x == Spades = "Spades"
           |otherwise = "Invalid suit."

showCard :: Card -> String
showCard (Card r s) = printf "%s %s" (showRank r) (showSuit s)