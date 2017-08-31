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
type GameState = [Chord]

{-
Create all possible guesses in the game
-}
allGuesses :: GameState
allGuesses =
  let ns = ([[n,o] | n <- ['A'..'G'], o <- ['1'..'3']])
      in (nub [sort [p1,p2,p3] -- remove duplicate Chords
          | p1 <- ns, p2 <- ns, p3 <- ns, p1 /= p2, p2/=p3, p1/= p3])
          -- prevent items from having duplicate pitches

-- score function stolen from the supplied Proj1Test.hs,
-- modified to work with map
my_response :: Chord -> Chord -> Feedback
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

-- A1 C1 E1
-- rework, perhaps A1 B1 C2
initialGuess :: (Chord, GameState)
initialGuess = (fg, gs)
    where ag = allGuesses
          fg = mid ag
          -- produces a score if each item in allGuesses were target
          gs = ag \\ [fg]
{-
receive previous guess, with feedback
feedback is ncorrect pitches, ncorrect notes, ncorrect octaves
BUT correct pitches are not counted as correct notes or octaves
generate another guess with an updated GameState
-}
nextGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
nextGuess pv fb = betterGuess pv fb

dumbGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
dumbGuess (_, []) _   = error "no more guesses, algorithm failed"
dumbGuess (_, gs) _   = (head gs, tail gs)

betterGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
betterGuess (_ , []) _  = error "no more guesses, algorithm failed"
betterGuess (lg, gs) fb =
  let
    {- nextGuess is the next item in the list which gets the same score as
    the Feedback -}
    ngs = filter (sameScore lg fb) gs
    in (mid ngs, ngs)

{- replaced by filter
findSameScore :: Chord -> Feedback -> GameState -> GameState
findSameScore _ _ [] = []
findSameScore ch fb (gs:gss) -- guess, score, gamestates, feedback
    | (my_response ch gs) == fb = gs : findSameScore ch fb gss
    | otherwise = findSameScore ch fb gss
-}

sameScore :: Chord -> Feedback -> Chord -> Bool
sameScore ch fb gs = my_response ch gs == fb

mid :: [a] -> a
mid [] = error "Empty list"
mid x  = x !! (div (length x) 2)

{-
lookAheadGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
lookAheadGuess (_, []) _ = error "no more guesses, algorithm failed"
lookAheadGuess (lg, gs) fb =
  let
    fl = head $ findSameScore fb gs -- first look guess
    gsf = dropChord fl gs -- GameState without first look
    gsfr = refactorGS fl gsf -- GameState after first look refactored

    ng = fl -- next guess
    gsr = gsfr -- GameState refactored
    in (ng, gsr)
-}

{-
refactorGS :: Chord -> GameState -> GameState
refactorGS _ [] = []
refactorGS ch ((tg, _):gss) = (chordScore ch tg) : (refactorGS ch gss)
-}

{-
-- check, not sure if correct
chordScore :: Chord -> Chord -> Feedback
chordScore ch tg = my_response ch
-}

{-
dropChord :: Chord -> GameState -> GameState
dropChord _ [] = []
dropChord ch ((g,sc):gss)
   | (sort g) == (sort ch) = dropChord ch gss
   | otherwise = (g,sc) : dropChord ch gss
-}

{-
extractGS :: (Chord, GameState) -> GameState
extractGS (_, gs) = gs
-}

{-
righter :: Scored -> Scored -> Bool
righter (_, (x,_,_)) (_, (y,_,_)) = x >= y

wronger :: Scored -> Scored -> Bool
wronger (_, (x,_,_)) (_, (y,_,_)) = x < y

-- I think getting them ordered in number of correct pitches,
-- results in better performance
sortByCorrect :: GameState -> GameState
sortByCorrect [] = []
sortByCorrect (gs:[]) = [gs]
sortByCorrect (gs:gss) = (sortByCorrect correcter) ++
    [gs] ++ (sortByCorrect further)
      where
        correcter = filter (righter gs) gss
        further   = filter (wronger gs) gss
-}
