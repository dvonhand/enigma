module Enigma.Rotor (
  Rotor,
  RotorWiring,
  RotorWiringError,
  duplicateCharacterError,
  forwardRotor,
  forwardRotorWiring,
  isDuplicateCharacterError,
  isMissingCharactersError,
  makeRotor,
  makeRotorWiring,
  missingCharactersError,
  reverseRotor,
  reverseRotorWiring,
  ringSetting,
  rotorWiring,
  rotorWiringMappings,
  setRingSetting,
  turnoverPositions
) where

import Control.Monad.Trans.Except
import Data.List
import Data.Tuple
import Enigma.Base

data RotorWiringError a = DuplicateCharacterError a
                        | MissingCharactersError [a]
  deriving (Show)

duplicateCharacterError :: RotorWiringError a -> Maybe a
duplicateCharacterError (DuplicateCharacterError a) = Just a
duplicateCharacterError _ = Nothing

isDuplicateCharacterError :: RotorWiringError a -> Bool
isDuplicateCharacterError (DuplicateCharacterError _) = True
isDuplicateCharacterError _ = False

isMissingCharactersError :: RotorWiringError a -> Bool
isMissingCharactersError (MissingCharactersError _) = True
isMissingCharactersError _ = False

missingCharactersError :: RotorWiringError a -> Maybe [a]
missingCharactersError (MissingCharactersError a) = Just a
missingCharactersError _ = Nothing

data RotorWiring a = RotorWiring [(a, a)] [(a, a)]
  deriving (Show)

rotorWiringMappings :: (EnigmaDatum a) => RotorWiring a -> [(a, a)]
rotorWiringMappings (RotorWiring a _) = a

makeRotorWiring :: (EnigmaDatum a, Monad m) => [(a, a)] -> ExceptT (RotorWiringError a) m (RotorWiring a)
makeRotorWiring mappings = let (err, _) = addAllMappings mappings
                               missingCharacters = filter (isMissing mappings) allEnigmaData
                             in if (null missingCharacters) then return $ RotorWiring mappings (fmap swap mappings)
                                                            else throwE . MissingCharactersError  $ missingCharacters
  where addAllMappings mappings = foldr (\currentMapping -> \(err, seenMappings) -> maybe (err, currentMapping:seenMappings)
                                                                                          (\e -> ((Just e), seenMappings))
                                                                                          (validator currentMapping seenMappings))
                                        (Nothing, [])
                                        mappings
        isMissing mappings character = maybe True (const False) (find (\(from, _) -> character == from) mappings)

validator :: EnigmaDatum a => (a, a) -> [(a, a)] -> Maybe (RotorWiringError a)
validator newMapping = foldr (\existing -> \err -> maybe err Just $ validateAddMapping newMapping existing) Nothing
  where validateAddMapping (from, to) (from', to')
          | from == from' = Just . DuplicateCharacterError $ from
          | to == to' = Just . DuplicateCharacterError $ to
          | otherwise = Nothing

forwardRotorWiring :: EnigmaDatum a => RotorWiring a -> a -> a
forwardRotorWiring (RotorWiring pairs _) char = maybe undefined
                                                      id
                                                      (lookup char pairs)

reverseRotorWiring :: EnigmaDatum a => RotorWiring a -> a -> a
reverseRotorWiring (RotorWiring _ pairs) char = maybe undefined
                                                      id
                                                      (lookup char pairs)

data Rotor a = Rotor a [a] (RotorWiring a)
  deriving (Show)

ringSetting :: EnigmaDatum a => Rotor a -> a
ringSetting (Rotor setting _ _) = setting

setRingSetting :: EnigmaDatum a => Rotor a -> a -> Rotor a
setRingSetting (Rotor _ turnovers wiring) newSetting = Rotor newSetting turnovers wiring

rotorWiring :: EnigmaDatum a => Rotor a -> RotorWiring a
rotorWiring (Rotor _ _ wiring) = wiring

turnoverPositions :: EnigmaDatum a => Rotor a -> [a]
turnoverPositions (Rotor _ turnoverPositions _) = turnoverPositions

forwardRotor :: EnigmaDatum a => Rotor a -> a -> a
forwardRotor (Rotor setting _ wiring) char = incrementBy (forwardRotorWiring wiring (decrementBy char setting)) setting

reverseRotor :: EnigmaDatum a => Rotor a -> a -> a
reverseRotor (Rotor setting _ wiring) char = incrementBy (reverseRotorWiring wiring (decrementBy char setting)) setting

makeRotor :: (EnigmaDatum a) => a -> [a] -> RotorWiring a -> Rotor a
makeRotor = Rotor
