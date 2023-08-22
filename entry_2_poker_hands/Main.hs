module Main where
import Text.Printf (printf)
import Data.List (group, groupBy, sortOn)
import Poker_data_types (
    Rank (..),
    Suit (..),
    Card (..),
    HandRank (..),
    showCard
    )
import Determine_hands (getHandRank)
import Describe_hands (evalHand)

-- Three of a kind: eights.
snowmenSet :: [Card]
snowmenSet = [
    Card Eight Clubs,
    Card Eight Diamonds,
    Card Eight Hearts,
    Card Jack Spades,
    Card King Diamonds
    ]

-- Full House:  Aces full of Nines.
acesFull :: [Card]
acesFull = [
    Card Ace Diamonds,
    Card Ace Hearts,
    Card Ace Spades,
    Card Nine Clubs,
    Card Nine Hearts
    ]

eightStraight :: [Card]
eightStraight = [
    Card Four Diamonds,
    Card Five Spades,
    Card Six Hearts,
    Card Seven Clubs,
    Card Eight Clubs
    ]

topTwo :: [Card]
topTwo = [
    Card Ace Hearts,
    Card Ace Clubs,
    Card King Hearts,
    Card King Clubs,
    Card Queen Spades
    ]

nutLow :: [Card]
nutLow = [
    Card Two Hearts,
    Card Three Diamonds,
    Card Four Spades,
    Card Five Clubs,
    Card Seven Hearts
    ]

main :: IO ()
main = do
    print $ evalHand snowmenSet
    print $ evalHand acesFull
    print $ evalHand eightStraight
    print $ evalHand topTwo
    print $ evalHand nutLow