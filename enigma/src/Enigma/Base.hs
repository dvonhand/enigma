module Enigma.Base (
  EnigmaDatum,
  allEnigmaData,
  decrementBy,
  incrementBy,
  stepIncrement
) where

class Eq a => EnigmaDatum a where
  allEnigmaData :: [a]
  decrementBy :: a -> a -> a
  incrementBy :: a -> a -> a
  stepIncrement :: a
