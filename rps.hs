#!/usr/bin/env runhaskell
-- Adam Selker, 2018-09-24
-- A program which plays rock-paper-scissors like a human

import System.Random

data Throw = Rock | Paper | Scissors deriving (Eq, Show)
data Result = Win | Draw | Lose deriving (Eq, Show)

data State = State {
	rng :: StdGen,
	myThrows :: (Int, Int, Int),
	yourThrows :: (Int, Int, Int)
	} deriving Show


stringToMove :: String -> Maybe Throw
stringToMove "r" = Just Rock
stringToMove "p" = Just Paper
stringToMove "s" = Just Scissors
stringToMove _ = Nothing

win :: Throw -> Throw -> Result
win Rock Paper = Lose
win Paper Scissors = Lose
win Scissors Rock = Lose
win x y = if x == y then Draw else Win

chooseMove :: State -> (Throw, State)
chooseMove (State rng myThrows yourThrows) = 
	let (rn, newRng) = randomR (0::Int,2::Int) rng in
	let move = case (mod rn 3) of 
		0 -> Rock
		1 -> Paper
		2 -> Scissors in
	(move, State newRng myThrows yourThrows)

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
	let initState = State (mkStdGen 2347) (0,0,0) (0,0,0) in
	doMove initState

