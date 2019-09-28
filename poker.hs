import Data.List as L
import Data.Map as M
import Data.Set as S

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
            deriving (Bounded, Enum, Eq, Ord)
-- R1 is for A in straight

data Suit = S | H | D | C
            deriving (Bounded, Enum, Eq, Ord, Show)


data Card = Card {rank :: Rank, suit :: Suit} 


type Deck = [Card]


instance Show Rank where
  show R1  = "A"
  show R2  = "2"
  show R3  = "3"
  show R4  = "4"
  show R5  = "5"
  show R6  = "6"
  show R7  = "7"
  show R8  = "8"
  show R9  = "9"
  show R10 = "10"
  show J   = "J"
  show Q   = "Q"
  show K   = "K"
  show A   = "A"


instance Show Card where
  show (Card rank suit) = show rank ++ show suit


-- full 52-card deck
fullDeck :: Deck
fullDeck = [Card r s | s <- [S, H, D, C],
                       r <- [R2 .. A]]


-- 32-card deck (cards of ranks at least 7)
piquetDeck :: Deck
piquetDeck = [Card r s | s <- [S, H, D, C],
                         r <- [R7 .. A]]


-- poker hand
newtype Hand = Hand {unHand :: [Card]}


data HandCategory = HighCard [Rank]             -- otherwise (5 different cards)
                  | OnePair Rank [Rank]         -- 2 of the same rank + 3 of different ranks
                  | TwoPair Rank Rank Rank      -- 2 of the same rank + 2 of another same rank + any (not of either rank)
                  | ThreeOfAKind Rank Rank Rank -- 3 of the same rank + any + any
                  | Straight Rank               -- sequence in different suits (here Rank denotes the highest ranked card)
                  | Flush [Rank]                -- all of the same suit not a sequence
                  | FullHouse Rank Rank         -- 3 matching cards of 1 rank and 2 matching cards of another rank
                  | FourOfAKind Rank Rank       -- 4 of the same rank + any
                  | StraightFlush Rank          -- sequence and same suits (here Rank denotes the highest ranked card)
                    deriving (Eq, Ord, Show)



-- returns True if all cards in a Hand are of the same suit.
sameSuits :: Hand -> Bool 
sameSuits h = let ((Card _ s):cs) = unHand h
              in all (\(Card _ s') -> s' == s) cs



-- returns a Just with the highest ranked card, if the Hand is a straight (or a straight flush). 
-- The Ace can count both as the highest and as the lowest ranked card in a straight


isStraight :: [Rank] -> Maybe Rank 
isStraight rs = if all (\(x,y) -> succ x == y ) $ zip (sort rs) (tail (sort rs))
                 then Just (last (sort rs))
                 else let rs' = L.map (\x -> if x == A then R1 else x) rs in (if all (\(x,y) -> succ x == y ) $ zip (sort rs') (tail (sort rs'))
                      then Just (last (sort rs'))
                      else Nothing)


-- converts a Hand into a list of Ranks, ordered from high to low.
ranks :: Hand -> [Rank]
ranks h = (sort (L.map (\(Card r s) -> r) (unHand h)))


-- converts a Hand into a list of Ranks paired with their multiplicity, order from high to low using a lexicographical ordering. 
-- For example, the hand ["7H", "7D", "QS", "7S", "QH"] should be ordered as [(3, R7), (2, Q)].

-- let f _ = Just "c"
-- alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "c")]
-- alter f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "c")]
-- alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a


f (Card r _) xs = alter (\x -> case x of 
                                      Nothing     -> Just (1, r)
                                      Just (n, r) -> Just (n+1, r))  r xs

order :: Hand -> [(Int, Rank)]
order h = M.elems (L.foldr f M.empty (unHand h))


-- converts a Hand into a HandCategory.
handCategory :: Hand -> HandCategory
handCategory h = 
         let rs = ranks h 
         in case (isStraight rs) of
            Just r  -> if (sameSuits h) then (StraightFlush r) else (Straight r)
            Nothing ->
                  case order h of
                       [_      , _       , _       , _      ,_] -> if (sameSuits h) then (Flush rs) else (HighCard rs)
                       [(4, r4), (1, r1)]                       -> FourOfAKind r4 r1
                       [(3, r3), (2, r2)]                       -> FullHouse r3 r2
                       [(3, r3), (1, r1) , (1, r1')]            -> ThreeOfAKind r3 r1 r1'
                       [(2, r2), (2, r2'), (1, r1 )]            -> TwoPair r2 r2' r1
                       [(2, r2), (1, r1) , (1, r1'), (1, r)]    -> OnePair r2 [r1, r1', r]
                       

instance Eq Hand where
  (==) h1 h2 = (==) (handCategory h1) (handCategory h2)
  
instance Ord Hand where
  compare h1 h2 = compare (handCategory h1) (handCategory h2)  



combs :: Int -> [a] -> [[a]]
combs _ [] = []
combs 0 _ = []
combs 1 x = L.map (:[]) x
combs n (x:xs) = (L.map (x:) (combs (n-1) xs) ) ++ combs n xs   


-- returns all combinations of 5-card hands than can be taken from a given deck of cards
allHands :: Deck -> [Hand]   
allHands deck = L.map (\cs -> Hand cs) (combs 5 deck)       




-- constructs a maximal set of distinct hands from deck. 
-- Hints: You will need the 'empty' and 'insert' functions from Data.Set. 
-- Use foldl’ instead of foldr to avoid a stack overflow when applying this function to large decks 

distinctHands :: Deck -> Set Hand  
distinctHands deck = S.foldl (flip S.insert) S.empty (S.fromList (allHands deck))                 
