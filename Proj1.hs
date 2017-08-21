-- Code for Declarative Programming Project 1
-- Marvin Lai
-- Derl 754672
-- Produces guesses for the game ChordProbe, as described in the project specs
-- most most most of the time, program simply assumes correct input
-- there is too much extra to include to verify each single input

import Data.List
import Data.Char

type Note = Char
type Oct  = Int
data Pitch = Pitch Note Oct
    deriving (Eq, Ord, Show)
{-data Chord     = Chord Pitch Pitch Pitch
    deriving (Eq, Ord, Show)
-}
-- GameState stores a list of all possible pitches remaining in the game
type GameState = [Pitch]

-- convert a string to a chord
strToPitch :: String -> Pitch
strToPitch (note:oct:[]) = (Pitch note (digitToInt oct))

-- convert a chord to a string
pitchToStr :: Pitch -> String
pitchToStr (Pitch note oct) = note:(intToDigit oct):[]

{-
just an initial guess
answer format is ncorrect pitches, ncorrect notes, ncorrect octaves
-}
intialGuess :: ([String], GameState)
intialGuess = ([guess1, guess2, guess3], [ch])
    where guess1 = "A1"
          guess2 = "C1"
          guess3 = "E1"
          ch = [(Chord n o) | n <- ['A'..'G'], o = [1..3]]
