-- Code for Declarative Programming Project 1
-- Marvin Lai
-- Derl 754672
-- Produces guesses for the game ChordProbe, as described in the project specs
-- most most most of the time, program simply assumes correct input
-- there is too much extra to include to verify each single input

import Data.List
import Data.Char

type Note = Char
type Oct  = Char

data Pitch = Pitch Note Oct
    deriving (Eq, Ord)

type Guess     = [String]
type Chord     = (Pitch, Pitch, Pitch)
-- GameState stores a list of all possible guesses in the game
type GameState = [Chord]

instance Show Pitch where
  show (Pitch n o) = [n,o]

-- convert a string to a pitch
strToPitch :: String -> Pitch
strToPitch (note:oct:[]) = (Pitch note oct)

-- convert a pitch to a string
pitchToStr :: Pitch -> String
pitchToStr (Pitch note oct) = [note, oct]

-- convert a chord to a guess
-- (pitch, pitch, pitch) to [str, str, str]
chordToGuess :: Chord -> Guess
chordToGuess (p1,p2,p3) = [show p1, show p2, show p3]

-- convert a guess to a chord
-- [str, str, str] to (pitch, pitch, pitch)
guessToChord :: Guess -> Chord
guessToChord [g1, g2, g3] = (c1, c2, c3)
    where c1 = strToPitch g1
          c2 = strToPitch g2
          c3 = strToPitch g3

-- A1 C1 E1
initialGuess :: (Guess, GameState)
initialGuess = ([g1, g2, g3], ch)
    where g1 = "A1"
          g2 = "C1"
          g3 = "E1"
          ch = allGuesses
{-
Create all possible guesses in the game
-}
allGuesses :: GameState
allGuesses =
  let ns = (map strToPitch [[n,o] | n <- ['A'..'G'], o <- ['1'..'3']])
      in map (\(p1:p2:p3:[]) -> (p1,p2,p3)) -- convert list of 3 items to tuple
          (nub [sort [p1,p2,p3]
          | p1 <- ns, p2 <- ns, p3 <- ns, p1 /= p2, p2/=p3, p1/= p3])

{-
receive previous guess, with feedback
feedback is ncorrect pitches, ncorrect notes, ncorrect octaves
BUT correct pitches are not counted as correct notes or octaves
generate another guess with an updated GameState
-}
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (pv, gs) (p, n, o) = dumbGuess (pv, gs) (p,n,o)

dumbGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
dumbGuess (_, []) _   = error "no more guesses, generation algorithm sucks"
dumbGuess (_, x:xs) _ = (chordToGuess x, xs)
