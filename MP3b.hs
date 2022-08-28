module MP3b where

import Data.List
import Data.List.Split
import Test.QuickCheck
import Data.Maybe (isJust, catMaybes)
import Control.Arrow


{-
  Playing card definitions (feel free to add your own supporting types, so long
  as you keep `Card`).
-}

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | Jack | Queen | King | Ace
            deriving (Enum, Eq, Ord, Bounded)
data Color = Clubs | Diamonds | Hearts | Spades
             deriving (Enum, Eq, Ord, Bounded)
data Card = Card { rank::Rank, color::Color}
            deriving (Eq, Ord)


-- | Displaying of data types
instance Show Color where
  show = (!!) ["C", "D", "H", "S"] . fromEnum

instance Show Rank where
  show = (!!) ["2","3","4","5","6","7","8","9","10","J","Q","K","A"] . fromEnum

instance Show Card where
  show (Card color rank) = show color <> show rank

{-
  A full deck of 52 playing cards.
-}
deck :: [Card]
deck = [Card r c | r <- enumFrom (toEnum 0), c <- enumFrom (toEnum 0)]



{-
  Hand types. Don't change these.
-}
data Hand = HighCard  | Pair | TwoPair | ThreeOfAKind | Straight
            | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
            deriving (Eq, Show, Ord)


{-
  Takes a list of 5 cards and returns the strongest hand they can be
  used to make.

  Examples (note that your `Card` values may look different):

  hand [Card R2 Hearts , Card R3 Diamonds , Card Ace Hearts , Card R5 Diamonds , Card R4 Spades ]
  => Straight

  hand [Card R2 Diamonds , Card R3 Clubs , Card R2 Clubs , Card R3 Diamonds , Card R2 Hearts ]
  => FullHouse
-}

hand :: [Card] -> Hand
hand cs' = head $ catMaybes allValues
  where
    cs = sort cs'
    rs = rank <$> cs
    rGs = group rs
    cls = color <$> cs
    -- Check all these in order and return first which is not Nothing (or return Nothing if all fail)
    allValues :: [Maybe Hand]
    allValues = [royalFlush, straightFlush, fourOfAKind, fullHouse, flush, straight, threeOfAKind, twoPair, pair, highCard]

    royalFlush = if isJust straightFlush && ((rank $ last $ cs) == Ace) then Just RoyalFlush else Nothing

    straightFlush = if isJust straight && isJust flush then Just StraightFlush else Nothing

    fourOfAKind = if any ((==4) . length) rGs then Just FourOfAKind else Nothing

    fullHouse = if isJust threeOfAKind && isJust pair then Just FullHouse else Nothing

    flush  = if length (group cls) == 1 then Just Flush else Nothing

    -- to reuse usefull advantages of deriving values from enum, the exception case (where Ace is the lowest card) is listed manually
    straight = if (not $ maxBound `elem` init rs) && (succ <$> init rs) == tail rs || rs == [R2,R3,R4,R5,Ace] then Just Straight else Nothing

    threeOfAKind = if any ((==3) . length) rGs then Just ThreeOfAKind else Nothing

    twoPair = if 2 >= length (filter ((==2) . length) rGs) then Just TwoPair else Nothing

    pair = if any ((==2) . length) rGs then Just Pair else Nothing

    highCard = Just HighCard


{-
  Takes a list of 5-`Card` lists, and returns a list of tuples of type
  `(Int, Hand)`, where each tuple indicates the number of times a certain
  `Hand` occurs in the input list. The tuples should be sorted in decreasing
  order of frequency.

  See the machine problem write-up on how to test this function with the
  generators defined for you below.
-}
computeStats :: [[Card]] -> [(Int, Hand)]
computeStats  = (fmap $ length &&& head) . group . sort . fmap hand


-------------------------------------------------------------------------------

{-
  Random deck/hand generators -- you shouldn't change any of the following
  functions!
-}

genDeck :: Gen [Card]
genDeck = shuffle deck

genHand :: Gen [Card]
genHand = (take 5) <$> genDeck

genHands :: Int -> Gen [[Card]]
genHands n = (take n . chunksOf 5) <$> genDeck

test :: Int -> IO [(Int, Hand)]
test n = generate $  computeStats <$> (vectorOf n genHand)
