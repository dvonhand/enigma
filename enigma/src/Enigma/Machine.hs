module Enigma.Machine (
  Machine,
  RotorPlacement,
  crypt,
  cryptList,
  forwardPlacement,
  makeMachine,
  placements,
  plugboard,
  position,
  reflector,
  reversePlacement,
  rotor
) where

import Control.Monad.Trans.State
import Data.List
import Enigma.Base
import Enigma.Plugboard
import Enigma.Reflector
import Enigma.Rotor

newtype RotorPlacement a = RotorPlacement (a, Rotor a)
  deriving (Show)

position :: (EnigmaDatum a) => RotorPlacement a -> a
position (RotorPlacement (a, _)) = a

rotor :: (EnigmaDatum a) => RotorPlacement a -> Rotor a
rotor (RotorPlacement (_, a)) = a

forwardPlacement :: (EnigmaDatum a) => RotorPlacement a -> a -> a
forwardPlacement (RotorPlacement (offset, rotor)) c = decrementBy (forwardRotor rotor $ incrementBy c offset) offset

reversePlacement :: (EnigmaDatum a) => RotorPlacement a -> a -> a
reversePlacement (RotorPlacement (offset, rotor)) c = decrementBy (reverseRotor rotor $ incrementBy c offset) offset

data Machine a = Machine ([RotorPlacement a]) (Reflector a) (Plugboard a)
  deriving (Show)

makeMachine :: (EnigmaDatum a) => [(a, Rotor a)] -> Reflector a -> Plugboard a -> Machine a
makeMachine rotors = Machine (fmap RotorPlacement rotors)

placements :: (EnigmaDatum a) => Machine a -> [RotorPlacement a]
placements (Machine a _ _) = a

plugboard :: (EnigmaDatum a) => Machine a -> Plugboard a
plugboard (Machine _ _ a) = a

reflector :: (EnigmaDatum a) => Machine a -> Reflector a
reflector (Machine _ a _) = a

crypt :: (EnigmaDatum a, Monad m) => a -> StateT (Machine a) m a
crypt char = do advanceRotors
                Machine pl r pb <- get
                let postPlugboardChar = runPlugboard pb char
                let postRotorChar = foldr forwardPlacement postPlugboardChar pl
                let reflectedChar = runReflector r postRotorChar
                let postRotorChar' = foldl (flip reversePlacement) reflectedChar pl
                let postPlugboardChar' = runPlugboard pb postRotorChar'
                return postPlugboardChar'

cryptList :: (EnigmaDatum a, Monad m) => [a] -> StateT (Machine a) m [a]
cryptList [] = return []
cryptList (c:cs) = do c' <- crypt c
                      cs' <- cryptList cs
                      return (c':cs')

advanceRotors :: (EnigmaDatum a, Monad m) => StateT (Machine a) m ()
advanceRotors = do (Machine rotorPlacements reflector plugboard) <- get
                   let (_, rotorPlacements') = foldr go (True, []) rotorPlacements
                   put $ Machine rotorPlacements' reflector plugboard
  where go placement@(RotorPlacement (a, rotor)) (advance, placements) = let a' = incrementBy stepIncrement a
                                                                             placement' = if advance then RotorPlacement (a', rotor) else placement
                                                                             advance' = elem a' $ turnoverPositions rotor
                                                                           in (advance', placement':placements)
