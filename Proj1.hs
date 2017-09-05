-- Code for Declarative Programming Project 1
-- Marvin Lai
-- Derl 754672
-- Produces guesses for the game ChordProbe, as described in the project specs
-- most most most of the time, program simply assumes correct input
-- there is too much extra to include to verify each single input
module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import Data.Char

-- renamed for clarity in code
-- same goes for all Type declaration
-- no new data type was needed
type Pitch     = String
type Chord     = [Pitch]
type Feedback  = (Int, Int, Int)
type GameState = [Chord]

-- set of all possible notes and octaves
notes = ['A'..'G']
octaves = ['1'..'3']

-- Create all possible guesses in the game
-- Basis of Gamestate is the remaining possible guesses
allGuesses :: GameState
allGuesses =
  let ns = ([[n,o] | n <- notes, o <- octaves])
      in ([[p1,p2,p3] | p1 <- ns, p2 <- ns, p3 <- ns, p1 < p2, p2 < p3])
      -- prevents duplicate chords

-- Initial guess, gives a decent amount of info to prune GameState
initialGuess :: (Chord, GameState)
initialGuess = (fg, gs)
    where ag = allGuesses
          -- Just a hardcoded guess, gives info on every octave and some notes
          fg = ["A1", "C2", "E3"]
          -- remove current guess from GameState
          -- game would either end if correct, or continue if not correct
          -- hence, we don't want to guess it again
          gs = ag \\ [fg]

-- receive previous guess, with feedback
-- generate another guess with an updated GameState
nextGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
nextGuess pv fb = betterGuess pv fb

-- throws out the next guess available in the gamestate
-- continue until it finds a correct guess
dumbGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
dumbGuess (_, []) _   = error "no more guesses, algorithm failed"
dumbGuess (_, gst) _   = (head gst, tail gst)

-- throws out a guess which would result in the same feedback if
-- that guess was the target for the last guess
betterGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
betterGuess (_ , []) _  = error "no more guesses, algorithm failed"
betterGuess (lg, gst) fb =
  let ngst = filter (sameScore lg fb) gst
      ng = mid ngst
      --ng = ngst !! (mod 2017 (length ngst))
    in (ng, ngst \\ [ng])

-- takes prev guess, feedback, and "target"
-- checks to see if guess target would result in same feedback score
sameScore :: Chord -> Feedback -> Chord -> Bool
sameScore ch fb tg = my_response ch tg == fb

-- scoring function with own interpretation
-- feedback is ncorrect pitches, ncorrect notes, ncorrect octaves
-- BUT correct pitches are not counted as correct notes or octaves
my_response :: Chord -> Chord -> Feedback
my_response gs tg = (pitch, note, oct)
  where gsn   = nub gs
        n     = length gsn
        pitch = n - length (tg \\ gsn)
        note  = n - length ((getNths 0 tg) \\ (getNths 0 gsn)) - pitch
        oct   = n - length ((getNths 1 tg) \\ (getNths 1 gsn))- pitch

-- gets the nth element of each list in a list of lists
getNths :: Eq a => Int -> [[a]] -> [a]
getNths _ []     = []
getNths n (x:xs) = (x !! n) : getNths n xs

-- gets middleth element of a list
mid :: [a] -> a
mid [] = error "Empty list"
mid x  = x !! (div (length x) 2)


-- FIN --
