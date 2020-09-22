module Enigma.Reflector (
  Reflector,
  ReflectorError,
  duplicateCharacterError,
  isDuplicateCharacterError,
  isMissingCharactersError,
  makeReflector,
  missingCharactersError,
  reflectorMappings,
  runReflector
) where

import Control.Monad.Trans.Except
import Data.List
import Enigma.Base

data ReflectorError a = DuplicateCharacterError a
                      | MissingCharactersError [a]
  deriving (Show)

duplicateCharacterError :: ReflectorError a -> Maybe a
duplicateCharacterError (DuplicateCharacterError a) = Just a
duplicateCharacterError _ = Nothing

isDuplicateCharacterError :: ReflectorError a -> Bool
isDuplicateCharacterError (DuplicateCharacterError _) = True
isDuplicateCharacterError _ = False

isMissingCharactersError :: ReflectorError a -> Bool
isMissingCharactersError (MissingCharactersError _) = True
isMissingCharactersError _ = False

missingCharactersError :: ReflectorError a -> Maybe [a]
missingCharactersError (MissingCharactersError a) = Just a
missingCharactersError _ = Nothing

newtype Reflector a = Reflector [(a, a)]
  deriving (Show)

reflectorMappings :: (EnigmaDatum a) => Reflector a -> [(a, a)]
reflectorMappings (Reflector a) = a

makeReflector :: (EnigmaDatum a, Monad m) => [(a, a)] -> ExceptT (ReflectorError a) m (Reflector a)
makeReflector mappings = let (err, _) = addAllMappings mappings
                             missingCharacters = filter (isMissing mappings) allEnigmaData
                           in if (null missingCharacters) then return . Reflector $ mappings
                                                          else throwE . MissingCharactersError $ missingCharacters
  where addAllMappings mappings = foldr (\currentMapping -> \(err, seenMappings) -> maybe (err, currentMapping:seenMappings)
                                                                                          (\e -> ((Just e), seenMappings))
                                                                                          (validator currentMapping seenMappings))
                                        (Nothing, [])
                                        mappings
        isMissing mappings character = maybe True (const False) (find (\(from, to) -> character == from || character == to) mappings)

validator :: EnigmaDatum a => (a, a) -> [(a, a)] -> Maybe (ReflectorError a)
validator newMapping = foldr (\existing -> \err -> maybe err Just $ validateAddMapping newMapping existing) Nothing
  where validateAddMapping (from, to) (from', to')
          | from == from' || from == to' = Just . DuplicateCharacterError $ from
          | to == to' || to == from' = Just . DuplicateCharacterError $ to
          | otherwise = Nothing

runReflector :: EnigmaDatum a => Reflector a -> a -> a
runReflector (Reflector pairs) char = go pairs char
  where go [] _ = undefined
        go ((from, to):pairs) char
          | from == char = to
          | to == char = from
          | otherwise = go pairs char
