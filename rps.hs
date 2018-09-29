#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}
-- Adam Selker, 2018-09-24
-- A program which plays rock-paper-scissors like a human

import System.Random

-- Data structures
data Throw = Rock | Paper | Scissors deriving (Eq, Show)
data Result = Win | Tie | Lose deriving (Eq, Show)

data State = State { -- The "mental state" of the bot
  rng :: StdGen, -- A random-number generator
  history :: [(Throw, Throw)] -- All of the moves in the past, from last to first
  } deriving Show

-- Utility functions

-- Interpret a human's input
stringToMove :: String -> Maybe Throw
stringToMove "r" = Just Rock
stringToMove "p" = Just Paper
stringToMove "s" = Just Scissors
stringToMove _ = Nothing

-- Win, lose, or tie?
result :: (Throw, Throw) -> Result
result (Rock, Paper) = Lose
result (Paper, Scissors) = Lose
result (Scissors, Rock) = Lose
result (x, y) = if x == y then Tie else Win

-- Win -> 1, tie -> 0, loss -> -1
winValue :: Result -> Float
winValue Win = 1
winValue Tie = 0
winValue Lose = -1

-- Average winRates
winRate :: [Result] -> Float
winRate [] = 0
winRate h = (sum $ map winValue h) / (fromIntegral $ length h) 

-- Some functions to perform part of the thinking.  If only I could have those...

-- If no other info, just pick something
wDefault :: [(Throw, Throw)] -> (Float, Float, Float)
wDefault _ = (1, 1, 1)

-- Do what's worked in the past
wPastOutcomes :: [(Throw, Throw)] -> (Float, Float, Float)
wPastOutcomes [] = (0, 0, 0)
wPastOutcomes history =
  -- Find the win rates of different moves
  let myRockRate = winRate [result x | x <- history, fst x == Rock] in 
  let myPaperRate = winRate [result x | x <- history, fst x == Paper] in 
  let myScissorsRate = winRate [result x | x <- history, fst x == Scissors] in 
  (myRockRate, myPaperRate, myScissorsRate)
  
-- Try to throw about equal amounts of Rock, Paper, Scissors... with a fudge factor =)
wEvenThrows :: [(Throw, Throw)] -> (Float, Float, Float)
wEvenThrows [] = (0, 0, 0)
wEvenThrows history =
  let rocks = fromIntegral $ length $ filter (\(x, _) -> x == Rock) history in
  let papers = fromIntegral $ length $ filter (\(x, _) -> x == Paper) history in
  let scissorss = fromIntegral $ length $ filter (\(x, _) -> x == Scissors) history in
  (-rocks * 0.9, -papers, -scissorss) -- Humans tend to throw a lot of rocks.

-- Helper function for wGamblersFallacy
gamblersStrength :: [(Throw, Throw)] -> Float
gamblersStrength [] = 0
gamblersStrength [(_, _)] = 0
gamblersStrength ((_, y1):(x2, y2):rest) =
  if y1 == y2 then 1 + (gamblersStrength ((x2,y2):rest))
  else 0

-- Humans percieve "runs" where none exist... 
wGamblersFallacy :: [(Throw, Throw)] -> (Float, Float, Float)
wGamblersFallacy [] = (0, 0, 0)
wGamblersFallacy history =
  let s = gamblersStrength history in
  case snd (head history) of Rock -> (0, s, -s)
                             Paper -> (-s, 0, s)
                             Scissors -> (s, -s, 0)

-- Helpers to merge weights and work with tuples
add3 :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
add3 (a, b, c) (d, e, f) = (a+d, b+e, c+f)

scale3 :: Num a => a -> (a, a, a) -> (a, a, a)
scale3 x (a, b, c) = (x*a, x*b, x*c)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- Weighted average of weights
mergeWs :: [Float] -> [(Float, Float, Float)] -> (Float, Float, Float)
mergeWs ss ws =
  let scaled = zipWith scale3 ss ws in -- Scale each weight triplet by the given values
  let (ua, ub, uc) = foldl add3 (0, 0, 0) scaled in -- Sum the different sources of weights
  let m = min ua (min ub uc) in 
  let (oa, ob, oc) =  (ua - m, ub - m, uc - m) in -- Subtract the lowest, so we start at 0
  scale3 ( 1/(oa + ob + oc)) (oa, ob, oc) -- Normalize so they sum to 1

-- The main decision function
chooseMove :: State -> (Throw, StdGen)
chooseMove (State rng history) = 
  let ws = mergeWs [1, 1, 0.1, 1] ( map (\x -> x history) [wDefault, wPastOutcomes, wEvenThrows, wGamblersFallacy] ) in
  let (rn :: Float, newRng) = random rng in
  let move = if (rn < fst3 ws) then Rock else if (rn < fst3 ws + snd3 ws) then Paper else Scissors in
  (move, newRng)

-- IO wrapper for the decision function
doMove :: State -> IO ()
doMove state = 
  do 
    userIn <-  getLine 
    case stringToMove userIn of
      Nothing -> do
        putStrLn "Please enter r, p, or s.  Use ctrl-D or ctrl-C to exit."
        doMove state
      Just x -> let (myThrow, newRng) = chooseMove state in 
        let newState = (\(State rng history) -> State newRng ((myThrow, x):history)) state in do
        putStrLn $ show myThrow
        doMove newState

main =
  let initState = State (mkStdGen 2347) [] in
  doMove initState

