-- File: proj1.hs
-- Author: Chenhan Ma, 823289
-- Created on Aug 2017
-- Project 1 for Comp90048 Declarative Programming
-- Guess a three pitch chord in haskell

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import qualified Data.Set as Set
import Debug.Trace

            -----------------------------------------
---------------         All Data Structures         ------------------
            -----------------------------------------

-- Data Structure for notes
data Note =  A | B | C | D | E | F | G
    deriving (Eq, Ord, Show, Read, Enum)

-- Data Structure for octaves
data Octave = One | Two | Three
    deriving (Eq, Ord, Read, Enum)

instance Show Octave where
    show One = "1"
    show Two = "2"
    show Three = "3"

-- Data Structure for Chords
data Pitch = Pitch Note Octave
    deriving (Eq, Ord, Read)

-- Show Chords in String
instance Show Pitch where
    show (Pitch note octave) = show note ++ show octave 

type Chord = [Pitch]

-- Generate all posible pitches that can form a Chord
allPitches :: Chord
allPitches = [Pitch n o | n <- [A .. G], o <- [One .. Three]]

-- Representation of GameState
type GameState = [Chord]

            -----------------------------------------
---------------           All Functions            ------------------
            -----------------------------------------

-- Generate all possible 3-Pitch Chords without replication
allChords :: GameState
allChords = [[pitch1, pitch2, pitch3] | 
            pitch1 <- allPitches, pitch2 <- allPitches, pitch3 <- allPitches, 
            pitch1 < pitch2, pitch2 < pitch3]

-- Initial Guess of Chord
-- Return an initial guess and a game state representation
initialchord :: Chord
initialchord = [Pitch A One, Pitch B One, Pitch C Two]


-- *************   Function initialGuess   ************ --
--
-- Takes no arguements, return an initial guess and Gamestate
-- Gamestate is representated by all possible chords remaining
initialGuess :: ([String], GameState)
initialGuess = (map show initialchord, 
               (delete initialchord (allChords :: [Chord])) :: GameState) 


-- Find Same Elements between Strings 
findSame :: [Char] -> [Char] -> String
findSame _ [] = ""
findSame [] _ = ""
findSame (x:xs) (y:ys) = 
    if x `elem` (y:ys) then
        x : findSame xs (delete x (y:ys))
    else 
        findSame xs (y:ys)

-- Find Distance between Chords
-- Takes two Chords
answer :: [String] -> Chord -> (Int, Int, Int)
answer guess targetChord = (correct, correctNote, correctOctave)
    where chordStr      = map show targetChord
          correct       = length(intersect guess chordStr)
          correctNote   = length(findSame (map head guess)(map head chordStr))
                          - correct
          correctOctave = length(findSame (map last guess)(map last chordStr))
                          - correct          

-- Used in function hint5
-- Calculate single Chord against [Chord] for expected number
-- Calculate one Chord's expected number
expectedNumber :: [Chord] -> Chord -> Float
expectedNumber [] a        = 9999.9
expectedNumber (y:ys) x    = expectedNum
    where listofValue      = map snd (map (\x -> (head x, length x)) 
                            (group (sort (map (answer (map show x)) (y:ys)))))
          expectedNum      = fromIntegral (sum ((map (^2) listofValue)))

-- Implement a function for hint 5
-- Calculate the expected value for each chord in the gamestate
-- Return the chord with minimum expected number
hint5 :: [Chord] -> [Chord] -> Chord
hint5 _ [] = []
hint5 [] _ = []
hint5 (x:xs) (y:ys) = chord
    where list = map (expectedNumber (y:ys)) (x:xs)
          index = elemIndices (minimum list) list
          chord = (x:xs) !! (head index)

-- *************   Function nextGuess   ************ --

-- Guess next Chord based on answer returned
-- Takes a guess and gamestate and a respone triple int, 
-- return a next guess with gamestate
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (preGuess, gamestate) response = (nextguess, nextGameState)
  where nextGameState = filter(\x -> answer preGuess x == response) gamestate
        nextguess     = map show (hint5 nextGameState nextGameState)
        --nextguess = map show (head nextGameState)
