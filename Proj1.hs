-- Code for Declarative Programming Project 1
-- Marvin Lai
-- Derl 754672
-- Produces guesses for the game ChordProbe, as described in the project specs
-- most most most of the time, program simply assumes correct input
-- there is too much extra to include to verify each single input
module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import Data.Char

type Pitch     = String
type Chord     = [Pitch]
type Feedback  = (Int, Int, Int)
-- GameState stores a list of all possible guesses in the game
type Scored    = (Chord, Feedback)
type GameState = [Scored]

-- A1 C1 E1
initialGuess :: (Chord, GameState)
initialGuess = (hardInit, ch)
    where g1 = "A1"
          g2 = "C2"
          g3 = "E3"
          hardInit = [g1, g2, g3]
          -- produces a score if each item in allGuesses were target
          ch = map (chordScore hardInit) (allGuesses \\ [hardInit])
{-
Create all possible guesses in the game
-}
allGuesses :: [Chord]
allGuesses =
  let ns = ([[n,o] | n <- ['A'..'G'], o <- ['1'..'3']])
      in (nub [sort [p1,p2,p3] -- remove duplicate Chords
          | p1 <- ns, p2 <- ns, p3 <- ns, p1 /= p2, p2/=p3, p1/= p3])
          -- prevent items from having duplicate pitches

-- score function stolen from the supplied Proj1Test.hs,
-- modified to work with map
my_response :: Chord -> Chord -> (Int,Int,Int)
my_response guess target = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (my_eqNth 0) guess' target)
                    - right
        rightOctave = num - (length $ deleteFirstsBy (my_eqNth 1) guess' target)
                    - right

-- | eqNth n l1 l2 returns True iff element n of l1 is equal to
--   element n of l2.
my_eqNth :: Eq a => Int -> [a] -> [a] -> Bool
my_eqNth n l1 l2 = (l1 !! n) == (l2 !! n)
{-
receive previous guess, with feedback
feedback is ncorrect pitches, ncorrect notes, ncorrect octaves
BUT correct pitches are not counted as correct notes or octaves
generate another guess with an updated GameState
-}
nextGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
nextGuess pv fb = betterGuess pv fb

dumbGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
dumbGuess (_, []) _   = error "no more guesses, generation algorithm sucks"
dumbGuess (_, (x,_):xs) _ = (x, (dropGuess x xs))

betterGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
betterGuess (_ , []) _ = error "no more guesses, the algorithm sucks"
betterGuess (lg, gs) fb =
  let
    ng = findSameScore gs fb
    gs1 = dropGuess ng gs
    gs2 = refactorGS ng gs1
    in (ng, gs2)

refactorGS :: Chord -> GameState -> GameState
refactorGS _ [] = []
refactorGS ch ((tg, _):gss) = (chordScore ch tg) : (refactorGS ch gss)

chordScore :: Chord -> Chord -> Scored
chordScore ch tg = (tg, my_response ch tg)

dropGuess :: Chord -> GameState -> GameState
dropGuess _ [] = []
dropGuess ch ((g,sc):gss)
   | (sort g) == (sort ch) = dropGuess ch gss
   | otherwise = (g,sc) : dropGuess ch gss

findSameScore :: GameState -> Feedback -> Chord
findSameScore [] _ = []
findSameScore ((g, sc):gss) fb -- guess, score, gamestates, feedback
    | sc == fb = g
    | otherwise = findSameScore gss fb

extractGS :: (Chord, GameState) -> GameState
extractGS (_, gs) = gs
