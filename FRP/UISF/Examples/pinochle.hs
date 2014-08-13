-- Author: Daniel Winograd-Cort
-- Date Created:        unknown
-- Date Last Modified:  7/15/2014

-- This is a pinochle assistant.  The user enters his hand at the GUI
-- and selects his preferred trump suit, and his meld is displayed.
-- If the user chooses a kitty size, he can calculate his potential 
-- meld from the kitty.

-- The kitty meld currently displays the mean expected meld and the 
-- max in the form: 
--  "# of kitties that produce this much meld"x"meld value":[best possible kitties]

-- This module requires the array package.

-- make sure to use "ghc --make -main-is FRP.UISF.Examples.Pinochle -O2 pinochle.hs" for best performance

{-# LANGUAGE Arrows, BangPatterns #-}
module FRP.UISF.Examples.Pinochle where
import FRP.UISF hiding (accum)

-- We make our own special type of button for inputting hand information, 
-- so we import a few things directly from Widget and SOE.
import FRP.UISF.Widget (cyclebox', padding, (//), whenG, box, marked, pushed, popped)
import FRP.UISF.SOE (text, withColor)

import Data.List (delete, foldl', group)
import GHC.Arr (Ix(..), indexError)
import Data.Array hiding ((//))
import Data.List (transpose)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

-------------------------------------------------------------
---------------------- Cards and Hands ----------------------
-------------------------------------------------------------
data Card = Card Suit Number
    deriving (Eq, Ord)

instance Enum Card where
    toEnum i = let (q,r) = quotRem i 6 in Card (toEnum q) (toEnum r)
    fromEnum (Card s n) = (6 * fromEnum s) + fromEnum n

instance Show Card where
    show (Card suit number) = show number ++ " of " ++ show suit

instance Ix Card where
    range (c1,c2) = [c1..c2]
    unsafeIndex (c1,c2) c = fromEnum c - fromEnum c1
    index b i | inRange b i = unsafeIndex b i | otherwise = indexError b i "Card"
    inRange (m,n) i = m <= i && i <= n

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq, Enum, Ord)
data Number = Nine | Jack | Queen | King | Ten | Ace
    deriving (Show, Eq, Enum, Ord)

allSuits = [Spades, Hearts, Diamonds, Clubs]
nums = [Ace, Ten, King, Queen, Jack, Nine]

type Hand = Array Card Int

deckArray = listArray (Card Spades Nine, Card Clubs Ace) (repeat 2)

emptyHand :: Hand
emptyHand = listArray (Card Spades Nine, Card Clubs Ace) (repeat 0)

addToHand :: Hand -> [Card] -> Hand
addToHand h cs = accum (+) h $ zip cs (repeat 1)

removeFromHand :: Hand -> [Card] -> Hand
removeFromHand h cs = accum (+) h $ zip cs (repeat (-1))

complementHand :: Hand -> Hand
complementHand = fmap (2-)

handLength :: Hand -> Int
handLength = sum . elems

class ShortShow a where
    shortShow :: a -> String

instance ShortShow Suit where
    shortShow = take 1 . show

instance ShortShow Number where
    shortShow = take 1 . show

instance ShortShow Card where
    shortShow (Card suit number) = shortShow number ++ " of " ++ shortShow suit

instance ShortShow a => ShortShow [a] where
    shortShow = show . map shortShow


-------------------------------------------------------------
--------------------------- Main ----------------------------
-------------------------------------------------------------

-- The main running function
main = runUI (800,700) "Pinochole Assistant" pinochleSF

pinochleSF :: UISF () ()
pinochleSF = proc _ -> do
    clearEv <- edge <<< setSize (120,22) (button "Clear hand?") -< ()
    spadeB   <- title "Spades"   $ leftRight $ concatA $ map (cardSelector . show) nums -< repeat clearEv
    heartB   <- title "Hearts"   $ leftRight $ concatA $ map (cardSelector . show) nums -< repeat clearEv
    diamondB <- title "Diamonds" $ leftRight $ concatA $ map (cardSelector . show) nums -< repeat clearEv
    clubB    <- title "Clubs"    $ leftRight $ concatA $ map (cardSelector . show) nums -< repeat clearEv
    trump    <- leftRight $ label "Choose Trump:" >>> radio (map show allSuits) 0 >>> arr toEnum -< ()
    let spades   = concat $ zipWith replicate spadeB nums
        hearts   = concat $ zipWith replicate heartB nums
        diamonds = concat $ zipWith replicate diamondB nums
        clubs    = concat $ zipWith replicate clubB nums
        hand = addToHand emptyHand $ map (Card Spades) spades ++ map (Card Hearts) hearts ++ map (Card Diamonds) diamonds ++ map (Card Clubs) clubs 
    (trump',hand') <- delay (Spades,emptyHand) -< (trump,hand)
    rec meld <- delay [] -< if hand == hand' && trump == trump' then meld else getMeld trump hand
    --display -< shortShow hand
    leftRight $ label "Number of cards:" >>> setSize (40,22) display -< handLength hand
    leftRight $ label "Total meld =" >>> displayStr -< show (sum (map snd3 meld)) ++ ": " ++ show (map fst3 meld)
    kittenSizeStr <- leftRight $ label "Kitty size =" >>> setSize (40,22) (textboxE "2") -< case (hand == hand', handLength hand) of
            (False, 11) -> Just $ show 4
            (False, 15) -> Just $ show 3
            _ -> Nothing
    restr <- checkbox "Restrict trump suit?" False -< ()
    b <- edge <<< button "Calculate meld from kitty" -< ()
    kre <- (asyncUISF $ toAutomaton $ uncurry $ uncurry kittyResult) -< toAsyncInput $
            fmap (const ((hand, kittenSizeStr), if restr then Just trump else Nothing)) b
    k <- hold [] -< case (clearEv, kre) of
        (Just _, _) -> Just []
        (Nothing, AOValue (r,_)) -> Just r
        (Nothing, AOCalculating _) -> Just ["Calculating ..."]
        _ -> Nothing
    displayStrList -< k
    histogramWithScale (makeLayout (Stretchy 10) (Fixed 150)) -< case (clearEv, kre) of
        (Just _, _) -> Just []
        (_, AOValue (_,m)) -> Just $ prepHistogramData m
        (_, AOCalculating _) -> Just []
        _ -> Nothing
    returnA -< ()

toAsyncInput :: SEvent a -> AsyncInput a
toAsyncInput (Just a) = AIValue a
toAsyncInput Nothing = AINoValue


prepHistogramData :: Map.Map Int Int -> [(Double, String)]
prepHistogramData m = map f [0..x] where
  x = maybe 0 (fst . fst) $ Map.maxViewWithKey m -- get max meld value (the max key in the map)
  f i = (fromIntegral $ fromMaybe 0 $ Map.lookup i m, show i) -- return pair of count and meld value (in String form)


-------------------------------------------------------------
--------------------- Kitty calculation ---------------------
-------------------------------------------------------------

kittyResult :: Hand -> String -> Maybe Suit -> ([String], Map.Map Int Int)
kittyResult _ s _ | null (reads s :: [(Int,String)]) = (["Unable to parse kitty size"], Map.empty)
kittyResult hand s _ | handLength hand + fst (head (reads s :: [(Int,String)])) > handLength deckArray = 
    (["Kitty size + hand size > deck size"], Map.empty)
kittyResult hand s trump = (("Mean = " ++ show meanMeld ++ ", Max = " 
                     ++ show (fst4 $ head maxMeld) ++ " with " ++ show (sum $ map thd4 maxMeld) ++ " options:"):
                     map (\m -> show (thd4 m) ++ " of " ++ show (snd4 m) ++ " with " ++ show (fth4 m) ++ " as trump") maxMeld,
                   meldMap)
  where
    kittySize = fst (head (reads s :: [(Int,String)]))
    restOfDeck = complementHand hand
    kitties = possibleKitties kittySize restOfDeck
    getSuitMelds s = map (calc s hand) kitties
    allMelds :: [(Int, [Card], Int, Suit)]
    allMelds = maybe allMelds' getSuitMelds trump
    allMelds' = concatMap (fst . meldStats) $ transpose $ map getSuitMelds [Spades, Hearts, Diamonds, Clubs]
    -- meldStats returns the pair (list of all best melds, (sum of all melds, count of all melds))
    meldStats = foldl' (\(a@((v,_,_,_):_),c) b@(v2,_,r,_) -> ((case compare v v2 of
                            LT -> [b]
                            EQ -> b:a
                            GT -> a),Map.alter (maybe (Just r) (Just . (+ r))) v2 c))  ([(-1,[],0,Spades)], Map.empty)
    (maxMeld, meldMap) = meldStats allMelds
    meanMeld = let (s,c) = Map.foldrWithKey' (\v c' (s,c) -> (s+c'*v,c+c')) (0,0) meldMap in fromIntegral s / fromIntegral c
    calc suit h (k,n) = (sum $ map snd3 $ getMeld suit (addToHand h k),k,n,suit)
    

possibleKitties :: Int -> Hand -> [([Card],Int)]
possibleKitties i hand = map (head &&& length) $ group $ ncr (assocs hand) i


-------------------------------------------------------------
--------------------- Meld calculation ----------------------
-------------------------------------------------------------

-- | Takes a hand and a set of meld data to potentially return meld.
-- The meld data is a list of meld names, a list of meld points, and 
-- a list of meld cards.  First, it checks to see if (length meld-points) 
-- copies of the cards are in the hand (checking for 2x, 3x, etc. copies 
-- of the given meld cards).  If so, it halts with the first String and 
-- point count, and if not, it recurs.  If the cards are never found, 
-- the empty list is returned.
-- This always returns either a one-element list or an empty list!
checkMeld :: Hand -> ([String], [Int], [Card]) -> [(String, Int, [Card])]
checkMeld hand (strs,ints,m) = 
    let n = containsCount hand m in if n == 0 then [] else [(strs!!(n-1), ints!!(n-1), m)]

-- | Will return the highest value among the indexes in the list
containsCount :: Ix i => Array i Int -> [i] -> Int
containsCount a [] = maxBound
containsCount a (i:is) = let v = a!i in if v == 0 then 0 else min v $ containsCount a is

-- Meld helpers
roundhouse = concatMap (\s -> [Card s King, Card s Queen]) allSuits
straight trump = [Card trump Ace, Card trump Ten, Card trump King, Card trump Queen, Card trump Jack]

-- | getMeld
-- Given a trump suit and hand, returns all of the meld for this hand.
getMeld :: Suit -> Hand -> [(String, Int, [Card])]
getMeld trump hand = 
    concatMap (checkMeld hand) meld2 ++ 
    case containsCount hand roundhouse of
      2 -> ("2xRoundhouse",48,concat $ replicate 2 roundhouse):
            checkMeld hand (["Straight","2xStraight"],[15,30],straight trump)
      1 -> ("Roundhouse",24,roundhouse):
            case containsCount hand (straight trump) of 
              2 -> ("2xStraight",30,concat $ replicate 2 $ straight trump):concatMap (checkMeld $ removeFromHand hand roundhouse) rhMeld
              1 -> ("Straight",15,straight trump):concatMap (checkMeld $ removeFromHand hand roundhouse) (rKQs:rhMeld)
              0 -> concatMap (checkMeld $ removeFromHand hand roundhouse) (rKQs:rhMeld)
      0 -> case containsCount hand (straight trump) of
              2 -> ("2xStraight",30,concat $ replicate 2 $ straight trump):concatMap (checkMeld hand) rhMeld
              1 -> ("Straight",15,straight trump):
                checkMeld (removeFromHand hand [Card trump King, Card trump Queen]) rKQstraight ++
                concatMap (checkMeld hand) rhMeld
              0 -> concatMap (checkMeld hand) (rKQs:rhMeld)
  where
    rhMeld = map (\s -> (["KQ of "++shortShow s,"2xKQ of "++shortShow s], [2,4], [Card s King, Card s Queen])) (delete trump allSuits) ++
            [(["Eighty Kings","All the Kings"], [8,16], map (flip Card King) allSuits),
             (["Sixty Queens","All the Queens"], [6,12], map (flip Card Queen) allSuits)]
--    rhMeld1 = map (\s -> (["KQ of "++shortShow s], [2], [Card s King, Card s Queen])) (delete trump allSuits) ++
--            [(["Eighty Kings"], [8], map (flip Card King) allSuits),
--             (["Sixty Queens"], [6], map (flip Card Queen) allSuits)]
    rKQs = (["Royal Marriage","2xRoyal Marriage"], [4,8], [Card trump King, Card trump Queen])
    rKQstraight = (["Bonus Royal Marriage"], [4], [Card trump King, Card trump Queen])
    meld2 = [(["Hundred Aces","Thousand Aces"], [10,20], map (flip Card Ace) allSuits),
             (["Forty Jacks","All the Jacks"], [4,8], map (flip Card Jack) allSuits),
             (["Pinochle","Double Pinochle"], [4,30], [Card Diamonds Jack, Card Spades Queen]),
             (["9 of Trump","2x9s of Trump"], [1,2], [Card trump Nine])]


-------------------------------------------------------------
-------------------------- Widgets --------------------------
-------------------------------------------------------------

-- cardSelector is a widget that looks kind of like a button except that 
-- in its unpressed state, it shows 0, when it's pressed once, it shows 
-- 1, and when it's pressed twice, it shows 2.  A third press resets it.
-- It takes as argument the names of the cards to select and a dynamic 
-- "clear" event.
cardSelector :: String -> UISF (SEvent ()) Int
cardSelector str = arr (fmap (const 0)) >>> cyclebox' d lst 0 where
    (tw, th) = (8 * (length str + 3), 16) 
    (minw, minh) = (tw + padding * 2, th + padding * 2)
    d = makeLayout (Stretchy minw) (Fixed minh)
    draw (i, s) b@((x,y), (w,h)) inFocus = 
      let x' = x + (w - tw) `div` 2 + if i>0 then 0 else -1
          y' = y + (h - th) `div` 2 + if i>0 then 0 else -1
      in withColor Black (text (x', y') s) 
         // whenG inFocus (box marked b)
         // box (if i>0 then pushed else popped) b
    lst = zip (map draw [(0,"0 "++str++"s"), (1, "1 "++str), (2, "2 "++str++"s")]) [0,1,2]


displayStrList :: UISF [String] ()
displayStrList = proc strs -> 
    if null strs then returnA -< () else (arr snd <<< (displayStr *** displayStrList) -< (head strs, tail strs))


-------------------------------------------------------------
--------------------- Helper Functions ----------------------
-------------------------------------------------------------

-- this only works for the ints in the list between 0 and 2 inclusive.
ncr :: [(a, Int)] -> Int -> [[a]]
ncr _ r | r < 0  = error "Zero or more elements should be extracted."
ncr _ 0          = [[]]
ncr [] _         = []
ncr ((x,0):xs) r = ncr xs r
ncr ((x,1):xs) r = map (x:) (ncr xs (r-1)) ++ ncr xs r
ncr ((x,2):xs) 1 = [[x],[x]] ++ ncr xs 1
ncr ((x,2):xs) r = map ([x,x]++) (ncr xs (r-2)) ++ concatMap (\l -> [x:l,x:l]) (ncr xs (r-1)) ++ ncr xs r

mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- Standard deviation of sample
stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ var xs

-- Sample variance
var xs = var' 0 0 0 xs / fromIntegral (length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta/fromIntegral (n + 1)

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
thd4 (a,b,c,d) = c
fth4 (a,b,c,d) = d



