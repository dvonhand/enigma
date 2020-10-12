module Bombe.Menu where

import Enigma.Base

allPossibleAlignments :: EnigmaDatum d => [d] -> [d] -> [[(d, d)]]
allPossibleAlignments a b = let (longer, longerLength, shorter, shorterLength) = if al > bl then (a, al, b, bl) else (b, bl, a, al)
                             in go longer longerLength shorter shorterLength
  where al = length a
        bl = length b
        go longer@(_:longer') longerLength shorter shorterLength
          | longerLength == shorterLength = [zip longer shorter]
          | otherwise = (zip longer shorter):(go longer' (longerLength - 1) shorter shorterLength)

possibleAlignments :: EnigmaDatum d => [d] -> [d] -> [[(d, d)]]
possibleAlignments a b = filter (all (\(a', b') -> a' /= b')) $ allPossibleAlignments a b
