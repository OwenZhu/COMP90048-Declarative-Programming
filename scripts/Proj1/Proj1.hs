--  File     : Proj1.hs
--  ID       : 811443
--  Author   : Hanwei Zhu
--  Origin   : Mon Aug 21 21:13:00 2017
--  Purpose  : This is a program that plays the game of "ChordProbe", which implements the performer
--             part of the game.

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List


-- | Put remaining candidate targets in the game state
type GameState = [[String]]


-- | takes no input arguments, and returns a pair of an initial guess and a game state. 
--   After several attempts, I found the initial guess "A1 D2 G3" gains the least average guess.
initialGuess :: ([String], GameState)
initialGuess = (["A1","D2","G3"], initGameState space 3)
    where space = ["A1","A2","A3","B1","B2","B3","C1","C2","C3","D1","D2","D3","E1","E2","E3","F1","F2","F3","G1","G2","G3"]


-- | inputs the game space, returns all candidate targets. Just similar to brute force method.
initGameState :: [String] -> Int -> GameState
initGameState _ 0 = [[]]
initGameState [] _ = []
initGameState (x:xs) n = [x:ys| ys <- initGameState xs (n-1)] ++ (initGameState xs n)


-- | Main program. Takes as input a pair of the previous guess and game state, and the 
--   feedback to this guess as a triple of correct pitches, notes, and octaves, and 
--   returns a pair of the next guess and game state.
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (pitches, targets) (fb1, fb2, fb3) = 
    (returnAnswer delete_pitches_notes_octaves, delete_pitches_notes_octaves)
    where
    delete_pitches = deletePitch fb1 pitches new_targets
    delete_pitches_notes = deleteNote (fb1 + fb2) pitches delete_pitches
    delete_pitches_notes_octaves = deleteOctave (fb1 + fb3) pitches delete_pitches_notes
    new_targets = targets\\[pitches]


-- | delete the impossibe pitches appearing in remaining candidate targets, 
--   according to the feedback of correct piches. Import Data.List, so I can 
--   use the operator "\\" to do substraction between two lists.
deletePitch :: Int -> [String] -> GameState -> GameState
deletePitch num pitches targets = 
    case num of
        0 -> filter (\e -> length (e\\pitches) == 3) targets
        1 -> filter (\e -> length (e\\pitches) == 2) targets
        2 -> filter (\e -> length (e\\pitches) == 1) targets
        3 -> filter (\e -> length (e\\pitches) == 0) targets


-- | delete the impossible pitches appearing in remaining candidate targets,
--   according to the feedback of correct notes.
deleteNote :: Int -> [String] -> GameState -> GameState
deleteNote num pitches targets = 
    case num of
        0 -> filter (\e -> length (getNote e \\ getNote pitches) == 3) targets
        1 -> filter (\e -> length (getNote e \\ getNote pitches) == 2) targets
        2 -> filter (\e -> length (getNote e \\ getNote pitches) == 1) targets
        3 -> filter (\e -> length (getNote e \\ getNote pitches) == 0) targets


-- | inputs one pitch, returns the octave list of it.
getNote :: [String] -> [String]
getNote xs = [[xs!!0!!0]] ++ [[xs!!1!!0]] ++ [[xs!!2!!0]]


-- | delete the impossibe pitches appearing in remaining candidate targets,
--   according to the feedback of correct octaves.
deleteOctave :: Int -> [String] -> GameState -> GameState
deleteOctave num pitches targets = 
    case num of
        0 -> filter (\e -> length (getOctave e \\ getOctave pitches) == 3) targets
        1 -> filter (\e -> length (getOctave e \\ getOctave pitches) == 2) targets
        2 -> filter (\e -> length (getOctave e \\ getOctave pitches) == 1) targets
        3 -> filter (\e -> length (getOctave e \\ getOctave pitches) == 0) targets


-- | inputs one pitch, returns the octave list of it.
getOctave :: [String] -> [String]
getOctave xs = [[xs!!0!!1]] ++ [[xs!!1!!1]] ++ [[xs!!2!!1]]


-- | returns the best next guess from remaining candidate targets, by calculating their expected number.
--   The candidate has the least expected number is the best next guess.
returnAnswer :: GameState -> [String]
returnAnswer targets = snd $ minimum $ [(getExpectedNumber t targets, t)| t <- targets]


-- | returns the expected number for each next guess'. Group the collection by feedbacks, and apply the
--   equation to calculate the expected number.
getExpectedNumber :: [String] -> GameState -> Float
getExpectedNumber guess' targets = 
    sum [fromIntegral (length x) * fromIntegral (length x)  | x <- xs] / fromIntegral (length targets)
    where
    xs = group (collectFeedback guess' targets)


-- | Returns all feedbacks in a list for one guess from each remaining candidate targets.
collectFeedback :: [String] -> GameState -> [(Int, Int, Int)]
collectFeedback guess' targets = [fb| t<- targets, let fb@(fb1, fb2, fb3) = response t guess']


-- | Compute the correct answer to a guess. First argument is the 
--   target, second is the guess.
response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (fb1, fb2, fb3)
  where fb1 = 3 - length (target \\ guess)
        fb2 = 3 - length (getNote target \\ getNote guess) - fb1
        fb3 = 3 - length (getOctave target \\ getOctave guess) - fb1
