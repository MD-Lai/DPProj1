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

--Create all possible guesses in the game
allGuesses :: GameState
allGuesses =
  let ns = ([[n,o] | n <- ['A'..'G'], o <- ['1'..'3']])
      in (nub [sort [p1,p2,p3] -- remove duplicate Chords
          | p1 <- ns, p2 <- ns, p3 <- ns, p1 /= p2, p2/=p3, p1/= p3])
          -- no duplicate pitches

-- A1 B2 C3
initialGuess :: (Chord, GameState)
initialGuess = (fg, gs)
    where ag = allGuesses
          fg = ["A1", "C2", "E3"]
          --fg = mid ag
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

-- throws out the next guess available in the gamestate
dumbGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
dumbGuess (_, []) _   = error "no more guesses, algorithm failed"
dumbGuess (_, gs) _   = (head gs, tail gs)

-- throws out a guess which would result in the same feedback
-- if that guess was the target for the last guess
betterGuess :: (Chord,GameState) -> Feedback -> (Chord,GameState)
betterGuess (_ , []) _  = error "no more guesses, algorithm failed"
betterGuess (lg, gs) fb =
  let
    {- nextGuess is the middleth item of the list of all guesses for which
    when taken as a target will result in the same score-}
    ngs = filter (sameScore lg fb) gs
    in (mid ngs, ngs)


-- takes prev guess, feedback, and guess target
-- checks to see if guess target would result in same feedback score
sameScore :: Chord -> Feedback -> Chord -> Bool
sameScore ch fb gs = my_response ch gs == fb

-- scoring function with own interpretation
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

mid :: [a] -> a
mid [] = error "Empty list"
mid x  = x !! (div (length x) 2)



{- replaced by filter
findSameScore :: Chord -> Feedback -> GameState -> GameState
findSameScore _ _ [] = []
findSameScore ch fb (gs:gss) -- guess, score, gamestates, feedback
    | (my_response ch gs) == fb = gs : findSameScore ch fb gss
    | otherwise = findSameScore ch fb gss
-}

{-
-- score function stolen from the supplied Proj1Test.hs,
-- modified to work with map
-- need to rewrite this ourselves
giv_response :: Chord -> Chord -> Feedback
giv_response guess target = (right, rightNote, rightOctave)
  where guess'      = nub guess -- remove duplicated pitches
        right       = length $ intersect guess' target -- how many are the same between the two
        num         = length guess' -- total number of guesses (should be 3)
        rightNote   = num - (length $ deleteFirstsBy (giv_eqNth 0) guess' target) 
                    - right -- checks how many right notes there are
        rightOctave = num - (length $ deleteFirstsBy (giv_eqNth 1) guess' target)
                    - right -- checks how many right octaves there are

-- | eqNth n l1 l2 returns True iff element n of l1 is equal to
--   element n of l2.
giv_eqNth :: Eq a => Int -> [a] -> [a] -> Bool
giv_eqNth n l1 l2 = (l1 !! n) == (l2 !! n)
-}

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
