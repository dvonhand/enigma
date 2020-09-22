module Enigma.Character (
  EnigmaChar,
  InvalidCharacterException,
  enigmaChar,
  enigmaIreflectorA,
  enigmaIreflectorB,
  enigmaIreflectorC,
  enigmaIrotorI,
  enigmaIrotorII,
  enigmaIrotorIII,
  enigmaIrotorIV,
  enigmaIrotorV,
  enigmaIwheelI,
  enigmaIwheelII,
  enigmaIwheelIII,
  enigmaIwheelIV,
  enigmaIwheelV,
  enigmaString,
  firstValidEnigmaChar,
  invalidCharacter,
  lastValidEnigmaChar,
  unEnigmaChar,
  unEnigmaString,
  validEnigmaChars
) where

import Control.Monad.Trans.Except
import Data.Char
import Data.Functor.Identity
import Enigma.Base
import Enigma.Reflector
import Enigma.Rotor

newtype InvalidCharacterException = InvalidCharacterException Char
  deriving (Show)

invalidCharacter :: InvalidCharacterException -> Maybe Char
invalidCharacter (InvalidCharacterException x) = Just x

firstValidEnigmaChar :: Char
firstValidEnigmaChar = 'A'

lastValidEnigmaChar :: Char
lastValidEnigmaChar = 'Z'

validEnigmaChars :: [Char]
validEnigmaChars = enumFromTo firstValidEnigmaChar lastValidEnigmaChar

newtype EnigmaChar = EnigmaChar Char
  deriving (Eq, Show)

instance EnigmaDatum EnigmaChar where
  allEnigmaData = fmap EnigmaChar validEnigmaChars
  decrementBy (EnigmaChar c) (EnigmaChar o) = let firstCharOffset = ord firstValidEnigmaChar
                                                  offsetOrdinal = ord o - firstCharOffset
                                                  originalOrdinal = ord c - firstCharOffset
                                                  newOrdinal = (originalOrdinal - offsetOrdinal) `mod` (length validEnigmaChars)
                                                in EnigmaChar . chr $ newOrdinal + firstCharOffset
  incrementBy (EnigmaChar c) (EnigmaChar o) = let firstCharOffset = ord firstValidEnigmaChar
                                                  offsetOrdinal = ord o - firstCharOffset
                                                  originalOrdinal = ord c - firstCharOffset
                                                  newOrdinal = (originalOrdinal + offsetOrdinal) `mod` (length validEnigmaChars)
                                                in EnigmaChar . chr $ newOrdinal + firstCharOffset
  stepIncrement = EnigmaChar 'B'

enigmaChar :: (Monad m) => Char -> ExceptT InvalidCharacterException m EnigmaChar
enigmaChar char = if elem char validEnigmaChars then return . EnigmaChar $ char
                                                else throwE . InvalidCharacterException $ char

unEnigmaChar :: EnigmaChar -> Char
unEnigmaChar (EnigmaChar x) = x

enigmaString :: (Monad m) => [Char] -> ExceptT InvalidCharacterException m [EnigmaChar]
enigmaString = sequence . fmap enigmaChar

unEnigmaString :: [EnigmaChar] -> [Char]
unEnigmaString = fmap unEnigmaChar

makeRotorWiringFromString :: [Char] -> RotorWiring EnigmaChar
makeRotorWiringFromString str = either undefined id . runExcept $ makeRotorWiring pairs
  where pairs = zip allEnigmaData outputs
        outputs = fmap EnigmaChar str

enigmaIwheelI :: RotorWiring EnigmaChar
enigmaIwheelI = makeRotorWiringFromString "EKMFLGDQVZNTOWYHXUSPAIBRCJ"

enigmaIwheelII :: RotorWiring EnigmaChar
enigmaIwheelII = makeRotorWiringFromString "AJDKSIRUXBLHWTMCQGZNPYFVOE"

enigmaIwheelIII :: RotorWiring EnigmaChar
enigmaIwheelIII = makeRotorWiringFromString "BDFHJLCPRTXVZNYEIWGAKMUSQO"

enigmaIwheelIV :: RotorWiring EnigmaChar
enigmaIwheelIV = makeRotorWiringFromString "ESOVPZJAYQUIRHXLNFTGKDCMWB"

enigmaIwheelV :: RotorWiring EnigmaChar
enigmaIwheelV = makeRotorWiringFromString "VZBRGITYUPSDNHLXAWMJQOFECK"

makeCharRotor :: RotorWiring EnigmaChar -> [Char] -> EnigmaChar -> Rotor EnigmaChar
makeCharRotor wiring turnovers ringSetting = makeRotor ringSetting (fmap EnigmaChar turnovers) wiring

enigmaIrotorI :: EnigmaChar -> Rotor EnigmaChar
enigmaIrotorI = makeCharRotor enigmaIwheelI ['R']

enigmaIrotorII :: EnigmaChar -> Rotor EnigmaChar
enigmaIrotorII = makeCharRotor enigmaIwheelII ['F']

enigmaIrotorIII :: EnigmaChar -> Rotor EnigmaChar
enigmaIrotorIII = makeCharRotor enigmaIwheelIII ['W']

enigmaIrotorIV :: EnigmaChar -> Rotor EnigmaChar
enigmaIrotorIV = makeCharRotor enigmaIwheelIV ['K']

enigmaIrotorV :: EnigmaChar -> Rotor EnigmaChar
enigmaIrotorV = makeCharRotor enigmaIwheelV ['A']

makeReflectorFromString :: [Char] -> Reflector EnigmaChar
makeReflectorFromString str = either undefined id . runExcept $ makeReflector pairs
  where pairs = unique $ zip allEnigmaData outputs
        unique = foldr (\p@(from, to) -> \p' -> if (foldr (\(from', to') -> \found -> from == from' || from == to' || to == from' || to == to' || found) False p') then p' else p:p') []
        outputs = fmap EnigmaChar str

enigmaIreflectorA :: Reflector EnigmaChar
enigmaIreflectorA = makeReflectorFromString "EJMZALYXVBWFCRQUONTSPIKHGD"

enigmaIreflectorB :: Reflector EnigmaChar
enigmaIreflectorB = makeReflectorFromString "YRUHQSLDPXNGOKMIEBFZCWVJAT"

enigmaIreflectorC :: Reflector EnigmaChar
enigmaIreflectorC = makeReflectorFromString "FVPJIAOYEDRZXWGCTKUQSBNMHL"
