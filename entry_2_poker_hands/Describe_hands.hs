module Describe_hands where
import Text.Printf (printf)
import Data.List(sortOn,group,sort)
import Poker_data_types(
    Rank (..),
    Suit (..),
    Card (..),
    HandRank (..),
    showRank,
    showSuit,
    showCard
    )
import Determine_hands (getHandRank)

-- Helper functions that are used when describing a hand.
getGroups :: [Card] -> [[Rank]]
getGroups xs = group $ sort [rank x | x <- xs]

flattenGroups :: Int -> ([a] -> c) -> [[a]] -> c
flattenGroups n f = f . concat . filter (\x -> length x == n)

filterRanks :: [[Rank]] -> Int -> ([Rank] -> Rank) -> String
filterRanks [] _ _ = "Empty list."
filterRanks xs 0 _ = showRank $ maximum $ concat xs
filterRanks xs n f = showRank $ flattenGroups n f xs

isWheel :: [Card] -> Bool
isWheel xs = (&&) (Ace `elem` straight) (Five `elem` straight)
                where
                    straight = [rank x | x <- xs]

-- Functions that describe all possible poker hands.
describeSF :: [Card] -> String
describeSF xs   |isWheel xs = printf "Straight Flush: Ace to Five, %s." 
                    (showSuit $ suit $ head xs)
                |otherwise = printf "Straight Flush: %s to %s, %s"
                    (showRank $ minimum [rank x | x <- xs])
                    (showRank $ maximum [rank x | x <- xs])
                    (showSuit $ suit $ head xs)

describeQuads :: [Card] -> String
describeQuads xs = printf "Quads: %ss, %s kicker."
                    (filterRanks (getGroups xs) 4 head)
                    (filterRanks (getGroups xs) 1 head)
            
describeFH :: [Card] -> String
describeFH xs = printf "Full House: %ss full of %ss."
                (filterRanks (getGroups xs) 3 head)
                (filterRanks (getGroups xs) 2 head)
               
describeFlush :: [Card] -> String
describeFlush xs = printf "Flush: %s high, %s."
                    (showRank $ maximum [rank x | x <- xs])
                    (showSuit $ suit $ head xs)

describeStraight :: [Card] -> String
describeStraight xs |isWheel xs = printf "Straight: Ace to Five." 
                    |otherwise = printf "Straight: %s to %s."
                        (showRank $ minimum [rank x | x <- xs])
                        (showRank $ maximum [rank x | x <- xs])

describeThreeKind :: [Card] -> String
describeThreeKind xs = printf "Three of a kind: %ss, %s kicker."
                        (filterRanks (getGroups xs) 3 head)
                        (filterRanks (getGroups xs) 1 maximum)

describeTwoPair :: [Card] -> String
describeTwoPair xs = printf "Two Pair : %ss and %ss, %s kicker."
                    (filterRanks (getGroups xs) 2 maximum)
                    (filterRanks (getGroups xs) 2 minimum)
                    (filterRanks (getGroups xs) 1 head)

describePair :: [Card] -> String
describePair xs = printf "Pair: %ss, %s kicker."
                    (filterRanks (getGroups xs) 2 head)
                    (filterRanks (getGroups xs) 1 maximum)

describeHighCard :: [Card] -> String
describeHighCard xs = printf "High Card: %s." (showRank $ maximum [rank x | x <- xs])

-- Function that evaluates a hand.
evalHand :: [Card] -> String
evalHand xs |hand == HighCard = describeHighCard xs
            |hand == Pair = describePair xs
            |hand == TwoPair = describeTwoPair xs
            |hand == ThreeKind = describeThreeKind xs
            |hand == Straight = describeStraight xs
            |hand == Flush = describeFlush xs
            |hand == FullHouse = describeFH xs
            |hand == FourKind = describeQuads xs
            |hand == StraightFlush = describeSF xs
            |otherwise = "Invalid output."
            where 
                hand = getHandRank xs