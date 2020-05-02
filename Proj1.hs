{-
Author: Anthony Giang <giang@student.unimelb.edu.au>
Purpose: Solves two-player logical card guessing game. 

The Card Game:
Two players face each other, each with a complete standard deck of western playing cards (without jokers).  
Oneplayer will be the answerer and the other is the guesser. The answerer begins by selecting 
some number of cards from their deck without showing the guesser. These cards will form the answer. 
The aim of the game is for the guesser to guess the answer. Once the answerer has selected the answer, 
the guesser chooses the same number of cards from their deck to form the guess and shows them to the answerer.

The answerer responds by telling the guesser these five numbers as feedback for the guess:
1.  (correct cards) How many of the cards in the answer are also in the guess.
2.  (lower ranks) How many cards in the answer have rank lower than the lowest rank in the guess. 
    Ranks, in order from low to high, are 2–10, Jack, Queen, King, and Ace.
3.  (correct ranks) How many of the cards in the answer have the same rank as a card in the guess. 
    For this, each card in the guess is only counted once. That is, if the answer has two queens and the guess has one, 
    the correct ranks number would be 1, not 2. Likewise if there is one queen in the answer and two in the guess.
4.  (higher ranks) How many cards in the answer have rank higher than the highest rank in the guess.
5.  (correct suits) How many of the cards in the answer have the same suit as a card in the guess, 
    only counting a card in the guess once. For example, if the answer has two clubs and 
    the guess has one club, or vice versa, the correct suits number would be 1, not 2.

The Solver:
    The solver defines 3 functions:
1.  feedback. Takes a target and a guess (in that order), each represented as a list of Cards,
    and returns the five feedback numbers.
2.  initialGuess. Takes the number of cards in the answer as input and returns a pair of an initial guess, 
    which is a list of the specified number of cards, and a game state.
3.  nextGuess. Takes as input a pair of the previous guess and game state, and the feedback to this guess 
    as a quintuple of counts of correct cards, low ranks, correct ranks, high ranks, and correct suits, 
    and returns a pair of the next guess and new game state.
-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where 
import Card
import Data.List

-- a list of possible cards.
type GameState = [[Card]]


{-feedback---}
-- uses 4 helper functions to calculate the five feedback
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback targets guesses = (nsamecard targets guesses, 
                            nlowerrank targets guesses, 
                            nsame rank targets guesses,
                            nhigherrank targets guesses,
                            nsame suit targets guesses)

-- returns the no. of cards in the answer that is also in the guess.
nsamecard :: [Card] -> [Card] -> Int
nsamecard ts gs = length ts - length (ts \\ gs)

-- returns the no. of cards in the answer that have rank lower than the lowest --rank in the guess.
nlowerrank :: [Card] -> [Card] -> Int
nlowerrank ts gs = length (filter (<minrank) targets)
    where minrank = minimum (map rank gs) 
          targets = map rank ts
-- returns the no. of cards in the answer that have rank higher than the --highest rank in the guess.
nhigherrank :: [Card] -> [Card] -> Int
nhigherrank ts gs = length (filter (>maxrank) targets)
    where maxrank = maximum (map rank gs) 
          targets = map rank ts
-- returns the no. of cards in the answer that have same rank or suit in the --guess.
nsame :: (Eq xs) => (Card -> xs) -> [Card] -> [Card] -> Int
nsame fn ts gs = length ts - length (targets \\ guesses)
    where targets = map fn ts 
          guesses = map fn gs


{-initialGuess---}
-- hardcode initial guess to have the ranks be evenly spaced out to minimise guessing time.
initialGuess :: Int -> ([Card],GameState)
initialGuess x
    | x == 2 = ([Card Club R6, Card Club R10],
      sublists 2 allCards)
    | x == 3 = ([Card Club R5, Card Club R8, Card Club Jack],
      sublists 3 allCards)
    | otherwise = ([Card Club R5, Card Club R7, Card Club R9, Card Club Queen],
      sublists 4 allCards)
    where allCards = [minBound..maxBound]::[Card]

--returns all combinations with length n, from a list.
sublists :: Int -> [a] -> [[a]]
sublists 0 _ = [[]]
sublists _ [] = []
sublists n (x:xs) = map (x:) (sublists (n-1) xs) ++ sublists n xs


{-nextGuess−−-}
-- filters the list of possible cards (gamestate) by removing cards that do not provide the same feedback as the answer. Takes the first possible combination as the next guess.
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (prevg, state) prevf = (head possible, possible)
    where possible = filter (\x -> feedback x prevg == prevf) state