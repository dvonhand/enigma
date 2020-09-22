module Enigma.Plugboard (
  Plugboard,
  PlugboardError,
  duplicateCharacterError,
  isDuplicateCharacterError,
  runPlugboard,
  makePlugboard,
  plugboardMappings
) where

import Control.Monad.Trans.Except
import Enigma.Base

newtype PlugboardError a = DuplicateCharacterError a
  deriving (Show)

duplicateCharacterError :: PlugboardError a -> Maybe a
duplicateCharacterError (DuplicateCharacterError a) = Just a

isDuplicateCharacterError :: PlugboardError a -> Bool
isDuplicateCharacterError (DuplicateCharacterError _) = True

newtype Plugboard a = Plugboard [(a, a)]
  deriving (Show)

plugboardMappings :: (EnigmaDatum a) => Plugboard a -> [(a, a)]
plugboardMappings (Plugboard a) = a

makePlugboard :: (EnigmaDatum a, Monad m) => [(a, a)] -> ExceptT (PlugboardError a) m (Plugboard a)
makePlugboard mappings = let (err, _) = addAllMappings mappings
                           in maybe (return . Plugboard $ mappings)
                                    throwE
                                    err
  where addAllMappings mappings = foldr (\currentMapping -> \(err, seenMappings) -> maybe (err, currentMapping:seenMappings)
                                                                                          (\e -> ((Just e), seenMappings))
                                                                                          (validator currentMapping seenMappings))
                                        (Nothing, [])
                                        mappings

validator :: EnigmaDatum a => (a, a) -> [(a, a)] -> Maybe (PlugboardError a)
validator newMapping@(from, to) mappings = if from == to then Just . DuplicateCharacterError $ from
                                                         else foldr (\existing -> \err -> maybe err Just $ validateAddMapping newMapping existing) Nothing mappings
  where validateAddMapping (from, to) (from', to')
          | from == from' || from == to' = Just . DuplicateCharacterError $ from
          | to == from' || to == to' = Just . DuplicateCharacterError $ to
          | otherwise = Nothing

runPlugboard :: EnigmaDatum a => Plugboard a -> a -> a
runPlugboard (Plugboard mappings) item = go mappings item
  where go [] item = item
        go ((c1, c2):mappings) item
          | c1 == item = c2
          | c2 == item = c1
          | otherwise = go mappings item
