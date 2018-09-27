#!/usr/bin/env runhaskell
-- Adam Selker, 2018-09-24
-- A program which plays rock-paper-scissors like a human

import System.Random

-- Data structures
data Throw = Rock | Paper | Scissors deriving (Eq, Show)
data Result = Win | Draw | Lose deriving (Eq, Show)

data State = State { -- The "mental state" of the bot
  rng :: StdGen, -- A random-number generator
  history :: [(Throw, Throw)] -- All of the moves in the past, from last to first
  } deriving Show

-- Utility functions
stringToMove :: String -> Maybe Throw
stringToMove "r" = Just Rock
stringToMove "p" = Just Paper
stringToMove "s" = Just Scissors
stringToMove _ = Nothing

result :: Throw -> Throw -> Result
result Rock Paper = Lose
result Paper Scissors = Lose
result Scissors Rock = Lose
result x y = if x == y then Draw else Win

-- Some functions to perform part of the thinking.  If only I could have those...

-- Do what's worked in the past
wPastOutcomes :: State -> (Float, Float, Float)
wPastOutcomes (State _ history) =
  let winRate h = (/ length h) $ sum $ map (\x -> (
                          case x of Win -> 1 -- "Average win rate"
                                    Tie -> 0
                                    Loss -> -1)) results in
  -- Find the win rates of different moves
  let myRockRate = winRate . filter (\(x, _) -> x == Rock) history in
  let myPaperRate = winRate . filter (\(x, _) -> x == Paper) history in
  let myScissorsRate = winRate . filter (\(x, _) -> x == Scissors) history in
  (myRockRate, myPaperRate, myScissorsRate)
  
-- Try to throw about equal amounts of Rock, Paper, Scissors... with a fudge factor =)
wEvenThrows :: State -> (Float, Float, Float)
wEvenThrows (State _ history) =
  let rocks = length $ filter (\(x, _) -> x == Rock) history in
  let papers = length $ filter (\(x, _) -> x == Paper) history in
  let scissorss = length $ filter (\(x, _) -> x == Scissors) history in
  (-rocks * 0.9, -papers, -scissors) -- Humans tend to throw a lot of rocks.

-- Humans percieve "runs" where none exist... 
wGamblersFallacy :: State -> (Float, Float, Float)
wGamblersFallacy (State rng history) =
  let weight = 1 in
  if fst history[0] == snd history[0]
    then let (r, p, s) = wGamblersFallacy (State rng history) in
    case fst history[0] of Rock -> (r+weight, p, s)
                           Paper -> (r, p+weight, s)
                           Scissors -> (r, p, s+weight)
    else (0,0,0)

-- The main decision function
chooseMove :: State -> (Throw, State)
chooseMove (State rng history) = 
  let (rn, newRng) = randomR (0::Int,2::Int) rng in
  let move = case (mod rn 3) of 
    0 -> Rock
    1 -> Paper
    2 -> Scissors in
  (move, State newRng history)

-- IO wrapper for the decision function
doMove :: State -> IO ()
doMove state = 
  do 
    userIn <-  getLine 
    case stringToMove userIn of
      Nothing -> do
        putStrLn "invalid input"
        doMove state
      Just x -> let (myThrow, newState) = chooseMove state in do
        putStrLn $ show myThrow
        doMove newState

main =
  let initState = State (mkStdGen 2347) [] in
  doMove initState

