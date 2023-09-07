module Determine_hands 
where
    import Data.List (group,sort,sortOn)
    import Poker_data_types (
        Rank (..),
        Suit (..),
        Card (..),
        HandRank (..),
        )

    {--
    Determine if a hand is a straight by checking
    if the sorted ranks of xs are equal to the first
    five elements of the enumerated head of xs.

    An Ace in poker can function as both a high card
    and a low card for straights.  The second guard 
    condition accounts for straights of the form 
    Ace to Five.
    --}
    isStraight :: [Card] -> Bool
    isStraight xs   |a == take 5 [head a ..] = True
                    |a == take 4 [head a ..] ++ [Ace] = True
                    |otherwise = False
                    where
                       a = sort [rank x | x <- xs]

    -- Determine if a hand is a flush.
    isFlush :: [Card] -> Bool
    isFlush (x:xs) = all ((== suit x) . suit) xs

    {-- 
    Determine if a hand is any type other than a
    straight or a flush.
    --}
    isPaired :: [Card] -> HandRank
    isPaired xs |c == [1,1,1,2] = Pair
                |c == [1,2,2] = TwoPair
                |c == [1,1,3] = ThreeKind
                |c == [2,3] = FullHouse
                |c == [1,4] = FourKind
                |otherwise = HighCard
                where
                    a = sort [rank x | x <- xs] 
                    b = sortOn length $ group a
                    c = [length x | x <- b]

    -- Get a hand's ranking.
    getHandRank :: [Card] -> HandRank
    getHandRank xs  |(&&) (isStraight xs) (isFlush xs) = StraightFlush
                    |isStraight xs = Straight
                    |isFlush xs = Flush
                    |otherwise = isPaired xs